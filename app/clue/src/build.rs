use crate::model::{DependencyKind, TargetKind};
use crate::target::{self, TargetConfig};
use crate::{ProjectKind, TargetTriple};
use anyhow::{Context, bail};
use riddlec::pipeline;
use std::collections::{HashMap, HashSet, hash_map::DefaultHasher};
use std::env;
use std::ffi::{OsStr, OsString};
use std::fs;
use std::hash::{Hash, Hasher};
use std::path::{Path, PathBuf};
use std::process::Command;

#[derive(Clone)]
pub enum BuildArtifact {
    Executable { path: PathBuf, target: TargetTriple },
    Library { links: Vec<LibraryLink> },
}

#[derive(Clone)]
pub struct LibraryLink {
    pub archive: PathBuf,
    pub object: PathBuf,
}

#[derive(Clone, PartialEq, Eq, Hash)]
struct LibraryUnit {
    root: PathBuf,
    profile: BuildProfile,
    triple: TargetTriple,
    features: Vec<String>,
    no_default_features: bool,
}

#[derive(Default)]
struct BuildState {
    building: HashSet<PathBuf>,
    libraries: HashMap<LibraryUnit, BuildArtifact>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum BuildProfile {
    Debug,
    Release,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum Flavor {
    Unix,
    Msvc,
}

#[derive(Debug, Clone, Copy)]
enum OutputMode {
    Executable,
    SharedLibrary,
    Object { pic: bool },
}

#[derive(Debug, Clone)]
struct CCompiler {
    program: OsString,
    flavor: Flavor,
    version: Vec<u8>,
    clang: bool,
    target: TargetConfig,
    profile: BuildProfile,
}

pub fn run_with_options(
    root: &Path,
    explicit_target: Option<TargetTriple>,
    profile: BuildProfile,
    bin: Option<&str>,
    features: &[String],
    no_default_features: bool,
) -> anyhow::Result<BuildArtifact> {
    run_target_with_options(
        root,
        explicit_target,
        profile,
        &crate::project::LoadOptions {
            bin: bin.map(str::to_owned),
            features: features.to_vec(),
            no_default_features,
            require_bin: true,
            ..crate::project::LoadOptions::default()
        },
    )
}

pub(crate) fn run_target_with_options(
    root: &Path,
    explicit_target: Option<TargetTriple>,
    profile: BuildProfile,
    load_options: &crate::project::LoadOptions,
) -> anyhow::Result<BuildArtifact> {
    if crate::cancellation_requested() {
        bail!("build cancelled");
    }
    let root = if root.is_absolute() {
        root.to_path_buf()
    } else {
        env::current_dir()?.join(root)
    };
    let analysis = crate::analyze_for_built_target(&root, explicit_target, true, load_options)?;
    ensure_analysis_success(&analysis)?;
    let triple = target::resolve(explicit_target, analysis.build_target.as_deref())?;
    let target = target::load(triple, true)?;
    let mut state = BuildState::default();
    state.building.insert(root.clone());
    let dependencies =
        build_dependencies(&root, triple, &target, profile, load_options, &mut state)?;
    state.building.remove(&root);
    build_analysis(&root, &analysis, triple, &target, profile, &dependencies)
}

fn build_dependencies(
    root: &Path,
    triple: TargetTriple,
    target: &TargetConfig,
    profile: BuildProfile,
    options: &crate::project::LoadOptions,
    state: &mut BuildState,
) -> anyhow::Result<Vec<BuildArtifact>> {
    let manifest = crate::manifest::read(root, ProjectKind::Binary)?;
    let resolution = crate::model::resolve_features(
        &manifest.features,
        &manifest.dependencies,
        &options.features,
        !options.no_default_features,
    )
    .map_err(anyhow::Error::msg)?;

    // Select the dependencies this build actually needs, keeping manifest
    // order so link arguments stay deterministic.
    let mut wanted: Vec<(PathBuf, crate::project::LoadOptions)> = Vec::new();
    for dependency in &manifest.dependencies {
        if dependency.kind == DependencyKind::Development && !options.include_dev {
            continue;
        }
        if dependency.optional && !resolution.active.contains(&dependency.alias) {
            continue;
        }
        let dependency_root = crate::package::dependency_root(root, dependency)?;
        let dependency_manifest = crate::manifest::read(&dependency_root, ProjectKind::Library)?;
        if dependency_manifest.kind == ProjectKind::ProcMacro
            || dependency_manifest
                .library
                .is_some_and(|target| target.kind == TargetKind::ProcMacro)
        {
            continue;
        }
        let mut features = dependency.features.clone();
        if let Some(forwarded) = resolution.dependency_features.get(&dependency.alias) {
            features.extend(forwarded.iter().cloned());
        }
        features.sort();
        features.dedup();
        wanted.push((
            dependency_root,
            crate::project::LoadOptions {
                features,
                no_default_features: !dependency.default_features,
                ..crate::project::LoadOptions::default()
            },
        ));
    }

    if wanted.len() <= 1 {
        return wanted
            .iter()
            .map(|(dependency_root, load_options)| {
                build_library_dependency(
                    dependency_root,
                    triple,
                    target,
                    profile,
                    load_options,
                    state,
                )
            })
            .collect();
    }

    // Sibling dependencies build in parallel across `jobs` workers. Each
    // worker owns its recursion stack, so package cycles are still caught
    // inside a worker; concurrent builds of the same package serialize on
    // that package's `.build-lock` instead.
    let jobs = crate::package::configured_jobs(root)
        .unwrap_or_else(|_| {
            std::thread::available_parallelism()
                .map(|value| value.get())
                .unwrap_or(1)
        })
        .clamp(1, wanted.len());
    let outcomes: std::sync::Mutex<Vec<Option<anyhow::Result<BuildArtifact>>>> =
        std::sync::Mutex::new((0..wanted.len()).map(|_| None).collect());
    let next = std::sync::atomic::AtomicUsize::new(0);
    std::thread::scope(|scope| {
        for _ in 0..jobs {
            let next = &next;
            let outcomes = &outcomes;
            let wanted = &wanted;
            scope.spawn(move || {
                loop {
                    let index = next.fetch_add(1, std::sync::atomic::Ordering::SeqCst);
                    let Some((dependency_root, load_options)) = wanted.get(index) else {
                        break;
                    };
                    let mut worker_state = BuildState::default();
                    let outcome = build_library_dependency(
                        dependency_root,
                        triple,
                        target,
                        profile,
                        load_options,
                        &mut worker_state,
                    );
                    outcomes
                        .lock()
                        .unwrap_or_else(|poisoned| poisoned.into_inner())[index] = Some(outcome);
                }
            });
        }
    });
    // Slots are indexed by manifest position, so the collected artifacts —
    // and the link arguments built from them — stay deterministic no matter
    // which worker finishes first.
    let slots = outcomes
        .into_inner()
        .unwrap_or_else(|poisoned| poisoned.into_inner());
    let mut collected = Vec::with_capacity(wanted.len());
    for slot in slots {
        match slot {
            Some(Ok(artifact)) => collected.push(artifact),
            Some(Err(error)) => bail!("dependency build failed: {error:#}"),
            None => bail!("dependency build did not produce every artifact"),
        }
    }
    Ok(collected)
}

fn build_library_dependency(
    root: &Path,
    triple: TargetTriple,
    target: &TargetConfig,
    profile: BuildProfile,
    options: &crate::project::LoadOptions,
    state: &mut BuildState,
) -> anyhow::Result<BuildArtifact> {
    let root = fs::canonicalize(root)?;
    let mut features = options.features.clone();
    features.sort();
    features.dedup();
    let unit = LibraryUnit {
        root: root.clone(),
        profile,
        triple,
        features,
        no_default_features: options.no_default_features,
    };
    if let Some(artifact) = state.libraries.get(&unit) {
        return Ok(artifact.clone());
    }
    if !state.building.insert(root.clone()) {
        bail!("cyclic package dependency involving `{}`", root.display());
    }
    let analysis = crate::analyze_project_impl_with_load_options(
        &root,
        &HashMap::new(),
        crate::analysis_options(triple),
        true,
        options,
    )?;
    ensure_analysis_success(&analysis)?;
    if analysis.kind == ProjectKind::Binary {
        bail!(
            "dependency `{}` does not provide a library target",
            root.display()
        );
    }
    let dependencies = build_dependencies(&root, triple, target, profile, options, state)?;
    let artifact = build_analysis(&root, &analysis, triple, target, profile, &dependencies)?;
    state.building.remove(&root);
    state.libraries.insert(unit, artifact.clone());
    Ok(artifact)
}

fn ensure_analysis_success(analysis: &crate::ProjectAnalysis) -> anyhow::Result<()> {
    let errors = riddlec::diagnostics::report_mapped(
        &analysis.result,
        &analysis.source,
        &analysis.entry.display().to_string(),
    );
    if errors > 0 || !analysis.result.success() {
        bail!("build failed");
    }
    Ok(())
}

fn build_analysis(
    root: &Path,
    analysis: &crate::ProjectAnalysis,
    triple: TargetTriple,
    target: &TargetConfig,
    profile: BuildProfile,
    dependencies: &[BuildArtifact],
) -> anyhow::Result<BuildArtifact> {
    let profile_name = match profile {
        BuildProfile::Debug => "debug",
        BuildProfile::Release => "release",
    };
    let build_dir = if profile == BuildProfile::Debug
        && TargetTriple::host().is_ok_and(|host| host == triple)
    {
        root.join(".clue").join("build")
    } else {
        root.join(".clue")
            .join("build")
            .join(triple.as_str())
            .join(profile_name)
    };
    fs::create_dir_all(&build_dir)?;
    let _guard = crate::lock::FileGuard::acquire(&build_dir.join(".build-lock"))?;
    let compiler = CCompiler::detect(&build_dir, target.clone(), profile)?;
    let target_name = analysis
        .target_name
        .as_deref()
        .unwrap_or(&analysis.package_name);
    let c_path = build_dir.join(format!("{target_name}.c"));
    let custom_runtime_path = analysis.runtime_source.as_ref().map(|path| root.join(path));
    let runtime_source = Some(if analysis.gc_enabled {
        match &custom_runtime_path {
            Some(path) => fs::read_to_string(path)
                .with_context(|| format!("failed to read runtime source `{}`", path.display()))?,
            None => match &target.runtime_source {
                Some(path) => fs::read_to_string(path).with_context(|| {
                    format!("failed to read target runtime `{}`", path.display())
                })?,
                None => gc::RUNTIME_C.to_owned(),
            },
        }
    } else {
        gc::NO_GC_RUNTIME_C.to_owned()
    });
    let runtime_path = Some({
        custom_runtime_path
            .clone()
            .unwrap_or_else(|| build_dir.join(format!("{target_name}.runtime.c")))
    });
    let args_runtime_path = build_dir.join(format!("{target_name}.args.c"));
    let hash_path = build_dir.join(format!("{target_name}.hash"));
    let hash = fingerprint(
        &analysis.manifest_fingerprint,
        &analysis.source.source,
        runtime_source.as_deref(),
        Some(&compiler),
        target,
    );
    let source_is_fresh = c_path.is_file()
        && args_runtime_path.is_file()
        && runtime_path.as_ref().is_none_or(|path| path.is_file())
        && fs::read_to_string(&hash_path).unwrap_or_default() == hash;

    if !source_is_fresh {
        let module = analysis
            .result
            .mir_module
            .as_ref()
            .context("successful compilation did not produce MIR")?;
        let c_code = pipeline::generate_c_for_package_with_source_map(
            module,
            analysis.package_index,
            analysis.gc_enabled,
            &analysis.source.source_map,
            &analysis.entry.display().to_string(),
        )
        .map_err(anyhow::Error::msg)?;
        atomic_write(&c_path, c_code.as_bytes())?;
        if analysis.runtime_source.is_none()
            && let (Some(path), Some(source)) = (&runtime_path, &runtime_source)
        {
            atomic_write(path, source.as_bytes())?;
        }
        atomic_write(&args_runtime_path, gc::ARGS_RUNTIME_C.as_bytes())?;
    }

    if analysis.kind != ProjectKind::Binary {
        let object = build_dir.join(format!(
            "{target_name}.{}",
            if triple.is_windows() { "obj" } else { "o" }
        ));
        let archive = build_dir.join(format!("{target_name}.rlib"));
        let dependencies_present = || {
            dependencies.iter().all(|dependency| match dependency {
                BuildArtifact::Library { links, .. } => links
                    .iter()
                    .all(|link| link.archive.is_file() && link.object.is_file()),
                BuildArtifact::Executable { .. } => true,
            })
        };
        if source_is_fresh
            && object.is_file()
            && archive.is_file()
            && library_outputs_exist(&analysis.library_types, &build_dir, target_name, triple)
            && dependencies_present()
        {
            println!("clue: fresh library `{target_name}`");
            return Ok(BuildArtifact::Library {
                links: library_links(&archive, &object, dependencies),
            });
        }

        // Global build cache: restore a previously built copy of this
        // library when its fingerprint matches, or publish the fresh build
        // so other projects can reuse it.
        let cache_entry = build_cache_entry(root, triple, profile).map(|dir| dir.join(&hash));
        let restored = cache_entry.as_ref().is_some_and(|entry| {
            restore_cached_library(entry, &build_dir, target_name)
                && library_outputs_exist(&analysis.library_types, &build_dir, target_name, triple)
                && dependencies_present()
        });
        if restored {
            atomic_write(&hash_path, hash.as_bytes())?;
            println!("clue: cached library `{target_name}`");
            return Ok(BuildArtifact::Library {
                links: library_links(&archive, &object, dependencies),
            });
        }

        build_library_artifacts(
            analysis,
            &compiler,
            (
                runtime_path
                    .as_deref()
                    .context("library build did not select a runtime")?,
                &args_runtime_path,
            ),
            &build_dir,
            target_name,
            triple,
            dependencies,
        )?;
        atomic_write(&hash_path, hash.as_bytes())?;
        if let Some(entry) = &cache_entry {
            publish_cached_library(entry, &build_dir, target_name);
        }
        println!("clue: built library `{target_name}`");
        return Ok(BuildArtifact::Library {
            links: library_links(&archive, &object, dependencies),
        });
    }

    let executable = executable_path(&c_path, triple);
    if source_is_fresh
        && executable.is_file()
        && dependencies.iter().all(|dependency| match dependency {
            BuildArtifact::Library { links, .. } => links
                .iter()
                .all(|link| link.archive.is_file() && link.object.is_file()),
            BuildArtifact::Executable { .. } => true,
        })
    {
        println!("clue: fresh {}", executable.display());
        return Ok(BuildArtifact::Executable {
            path: executable,
            target: triple,
        });
    }

    let temp_executable = temporary_path(&executable);
    let mut sources = vec![
        c_path.as_path(),
        runtime_path
            .as_deref()
            .context("binary build did not select a runtime")?,
        args_runtime_path.as_path(),
    ];
    sources.extend(
        dependencies
            .iter()
            .filter_map(|dependency| match dependency {
                BuildArtifact::Library { links, .. } => {
                    Some(links.iter().map(|link| link.archive.as_path()))
                }
                BuildArtifact::Executable { .. } => None,
            })
            .flatten(),
    );
    compiler.compile(&sources, &temp_executable, &[], false, &[])?;
    replace_file(&temp_executable, &executable)?;
    atomic_write(&hash_path, hash.as_bytes())?;
    println!("clue: built {}", executable.display());
    Ok(BuildArtifact::Executable {
        path: executable,
        target: triple,
    })
}

fn build_library_artifacts(
    analysis: &crate::ProjectAnalysis,
    compiler: &CCompiler,
    runtime_paths: (&Path, &Path),
    build_dir: &Path,
    target_name: &str,
    triple: TargetTriple,
    dependencies: &[BuildArtifact],
) -> anyhow::Result<()> {
    let (runtime_path, args_runtime_path) = runtime_paths;
    let c_path = build_dir.join(format!("{target_name}.c"));
    let object = build_dir.join(format!(
        "{target_name}.{}",
        if triple.is_windows() { "obj" } else { "o" }
    ));
    compiler.compile_object(&c_path, &object, true)?;
    let metadata = serde_json::json!({
        "schema": 1,
        "name": analysis.package_name,
        "version": analysis.package_version,
        "target": triple.as_str(),
        "source_hash": analysis.manifest_fingerprint,
    });
    atomic_write(
        &build_dir.join(format!("{target_name}.rmeta")),
        serde_json::to_vec_pretty(&metadata)?.as_slice(),
    )?;

    let library_types = if analysis.library_types.is_empty() {
        vec![crate::model::LibraryType::RiddleLib]
    } else {
        analysis.library_types.clone()
    };
    compiler.archive(
        &[object.as_path()],
        &build_dir.join(format!("{target_name}.rlib")),
    )?;
    if library_types.contains(&crate::model::LibraryType::StaticLib) {
        let runtime_object = build_dir.join(format!(
            "{target_name}.runtime.{}",
            if triple.is_windows() { "obj" } else { "o" }
        ));
        let args_runtime_object = build_dir.join(format!(
            "{target_name}.args.{}",
            if triple.is_windows() { "obj" } else { "o" }
        ));
        compiler.compile_object(runtime_path, &runtime_object, true)?;
        compiler.compile_object(args_runtime_path, &args_runtime_object, true)?;
        let mut objects = vec![
            object.as_path(),
            runtime_object.as_path(),
            args_runtime_object.as_path(),
        ];
        objects.extend(
            dependencies
                .iter()
                .filter_map(|dependency| match dependency {
                    BuildArtifact::Library { links, .. } => {
                        Some(links.iter().map(|link| link.object.as_path()))
                    }
                    BuildArtifact::Executable { .. } => None,
                })
                .flatten(),
        );
        compiler.archive(
            &objects,
            &build_dir.join(static_library_name(target_name, triple)),
        )?;
    }
    if library_types.contains(&crate::model::LibraryType::Cdylib) {
        let output = build_dir.join(shared_library_name(target_name, triple));
        let temp = temporary_path(&output);
        let mut sources = vec![c_path.as_path(), runtime_path, args_runtime_path];
        sources.extend(
            dependencies
                .iter()
                .filter_map(|dependency| match dependency {
                    BuildArtifact::Library { links, .. } => {
                        Some(links.iter().map(|link| link.archive.as_path()))
                    }
                    BuildArtifact::Executable { .. } => None,
                })
                .flatten(),
        );
        compiler.compile(&sources, &temp, &[], true, &[])?;
        replace_file(&temp, &output)?;
    }
    Ok(())
}

fn library_links(
    archive: &Path,
    object: &Path,
    dependencies: &[BuildArtifact],
) -> Vec<LibraryLink> {
    std::iter::once(LibraryLink {
        archive: archive.to_path_buf(),
        object: object.to_path_buf(),
    })
    .chain(dependencies.iter().flat_map(|dependency| match dependency {
        BuildArtifact::Library { links, .. } => links.clone().into_iter(),
        BuildArtifact::Executable { .. } => Vec::new().into_iter(),
    }))
    .collect()
}

fn library_outputs_exist(
    library_types: &[crate::model::LibraryType],
    build_dir: &Path,
    target_name: &str,
    triple: TargetTriple,
) -> bool {
    (!library_types.contains(&crate::model::LibraryType::StaticLib)
        || build_dir
            .join(static_library_name(target_name, triple))
            .is_file())
        && (!library_types.contains(&crate::model::LibraryType::Cdylib)
            || build_dir
                .join(shared_library_name(target_name, triple))
                .is_file())
}

fn static_library_name(name: &str, target: TargetTriple) -> String {
    if target.is_windows() {
        format!("{name}.lib")
    } else {
        format!("lib{name}.a")
    }
}

fn shared_library_name(name: &str, target: TargetTriple) -> String {
    if target.is_windows() {
        format!("{name}.dll")
    } else if target.is_macos() {
        format!("lib{name}.dylib")
    } else {
        format!("lib{name}.so")
    }
}

pub struct ProcMacroArtifacts {
    pub library: PathBuf,
    pub runner: PathBuf,
}

pub fn build_proc_macro_library(
    package: &crate::project::ProcMacroPackage,
    exports: &[crate::proc_macro::HostMacroExport],
    expanded_source: &str,
) -> anyhow::Result<ProcMacroArtifacts> {
    let host = TargetTriple::host().map_err(anyhow::Error::msg)?;
    let target = target::load(host, true)?;
    let build_dir = package.root.join(".clue").join("build");
    fs::create_dir_all(&build_dir)?;
    let _guard = crate::lock::FileGuard::acquire(&build_dir.join(".build-lock"))?;
    let compiler = CCompiler::detect(&build_dir, target.clone(), BuildProfile::Release)?;

    let source = crate::proc_macro::host_source(expanded_source, exports);
    let bridge = crate::proc_macro::host_runtime_c(exports);
    let c_path = build_dir.join(format!("{}.proc-macro.c", package.name));
    let runtime_path = build_dir.join(format!("{}.proc-macro.runtime.c", package.name));
    let args_runtime_path = build_dir.join(format!("{}.proc-macro.args.c", package.name));
    let bridge_path = build_dir.join(format!("{}.proc-macro.host.c", package.name));
    let runner_c_path = build_dir.join(format!("{}.proc-macro.runner.c", package.name));
    let hash_path = build_dir.join(format!("{}.proc-macro.hash", package.name));
    let library = build_dir.join(format!(
        "{}{}.proc-macro{}",
        env::consts::DLL_PREFIX,
        package.name,
        env::consts::DLL_SUFFIX
    ));
    let runner = build_dir.join(format!(
        "{}.proc-macro-runner{}",
        package.name,
        host.executable_suffix()
    ));
    let runner_source = crate::proc_macro::proc_macro_runner_c();
    let hash = fingerprint(
        &format!(
            "{}\n{bridge}\n{runner_source}",
            package.manifest_fingerprint
        ),
        &source,
        Some(gc::RUNTIME_C),
        Some(&compiler),
        &target,
    );
    if c_path.is_file()
        && runtime_path.is_file()
        && args_runtime_path.is_file()
        && bridge_path.is_file()
        && runner_c_path.is_file()
        && library.is_file()
        && runner.is_file()
        && fs::read_to_string(&hash_path).unwrap_or_default() == hash
    {
        return Ok(ProcMacroArtifacts { library, runner });
    }

    let analysis = pipeline::compile_with_options(&source, pipeline::CompileOptions::default());
    let errors = riddlec::diagnostics::report(
        &analysis,
        Some(&source),
        &package.entry.display().to_string(),
    );
    if errors > 0 || !analysis.success() {
        bail!("failed to compile proc-macro package `{}`", package.name);
    }
    let module = analysis
        .mir_module
        .as_ref()
        .context("successful proc-macro compilation did not produce MIR")?;
    let generated =
        pipeline::generate_c_with_gc_and_source(module, true, &package.entry.display().to_string())
            .map_err(anyhow::Error::msg)?;
    atomic_write(
        &c_path,
        format!(
            "void riddle_proc_abort(void);\n{}",
            generated.replace("abort()", "riddle_proc_abort()")
        )
        .as_bytes(),
    )?;
    atomic_write(&runtime_path, gc::RUNTIME_C.as_bytes())?;
    atomic_write(
        &args_runtime_path,
        format!(
            "void riddle_proc_abort(void);\n{}",
            gc::ARGS_RUNTIME_C.replace("abort()", "riddle_proc_abort()")
        )
        .as_bytes(),
    )?;
    atomic_write(&bridge_path, bridge.as_bytes())?;
    atomic_write(&runner_c_path, runner_source.as_bytes())?;
    let temp_library = temporary_path(&library);
    compiler.compile(
        &[
            c_path.as_path(),
            runtime_path.as_path(),
            args_runtime_path.as_path(),
            bridge_path.as_path(),
        ],
        &temp_library,
        &["putchar=riddle_proc_putchar"],
        true,
        // ponytail: glibc folds `putchar` into `putc(c, stdout)` at -O2 after
        // macro expansion, bypassing the -D rename; -fno-inline stops it.
        &["-fno-inline"],
    )?;
    replace_file(&temp_library, &library)?;
    let temp_runner = temporary_path(&runner);
    let runner_args = if host.is_linux() { &["-ldl"][..] } else { &[] };
    compiler.compile(
        &[runner_c_path.as_path()],
        &temp_runner,
        &[],
        false,
        runner_args,
    )?;
    replace_file(&temp_runner, &runner)?;
    atomic_write(&hash_path, hash.as_bytes())?;
    Ok(ProcMacroArtifacts { library, runner })
}

fn fingerprint(
    manifest: &str,
    source: &str,
    runtime: Option<&str>,
    compiler: Option<&CCompiler>,
    target: &TargetConfig,
) -> String {
    let mut hasher = DefaultHasher::new();
    manifest.hash(&mut hasher);
    source.hash(&mut hasher);
    runtime.hash(&mut hasher);
    gc::ARGS_RUNTIME_C.hash(&mut hasher);
    "c11".hash(&mut hasher);
    riddlec::GIT_HASH.hash(&mut hasher);
    target.hash(&mut hasher);
    if let Some(compiler) = compiler {
        compiler.program.hash(&mut hasher);
        compiler.flavor.hash(&mut hasher);
        compiler.version.hash(&mut hasher);
    }
    format!("{:016x}", hasher.finish())
}

fn executable_path(c_path: &Path, target: TargetTriple) -> PathBuf {
    let mut path = c_path.with_extension("");
    if !target.executable_suffix().is_empty() {
        path.set_extension(target.executable_suffix().trim_start_matches('.'));
    }
    path
}

fn temporary_path(path: &Path) -> PathBuf {
    let name = path
        .file_name()
        .and_then(|name| name.to_str())
        .unwrap_or("artifact");
    path.with_file_name(format!(".{name}.tmp-{}", std::process::id()))
}

fn atomic_write(path: &Path, bytes: &[u8]) -> anyhow::Result<()> {
    let temp = temporary_path(path);
    fs::write(&temp, bytes)?;
    replace_file(&temp, path)
}

fn replace_file(source: &Path, destination: &Path) -> anyhow::Result<()> {
    crate::lock::replace_file(source, destination)?;
    Ok(())
}

impl CCompiler {
    fn detect(
        build_dir: &Path,
        target: TargetConfig,
        profile: BuildProfile,
    ) -> anyhow::Result<Self> {
        if let Some(program) = env::var_os("CC") {
            let compiler = Self::new(&program, target, profile).ok_or_else(|| {
                anyhow::anyhow!(
                    "C compiler from CC `{}` could not report its version",
                    program.to_string_lossy()
                )
            })?;
            if compiler.probe(build_dir) {
                return Ok(compiler);
            }
            bail!(
                "C compiler from CC `{}` cannot compile and link C11",
                program.to_string_lossy()
            );
        }

        if let Some(program) = &target.c_toolchain.compiler {
            let compiler =
                Self::new(program.as_os_str(), target.clone(), profile).ok_or_else(|| {
                    anyhow::anyhow!(
                        "configured C compiler `{}` could not report its version",
                        program.display()
                    )
                })?;
            if compiler.probe(build_dir) {
                return Ok(compiler);
            }
            bail!(
                "configured C compiler `{}` cannot compile and link for `{}`",
                program.display(),
                target.triple
            );
        }

        let candidates: &[&str] = if target.triple.is_windows() && cfg!(windows) {
            &["clang-cl", "clang", "cc", "gcc", "cl"]
        } else if target.triple.is_windows() {
            &["clang", "cc", "gcc", "clang-cl"]
        } else {
            &["clang", "cc", "gcc"]
        };
        let mut programs = candidates
            .iter()
            .copied()
            .map(OsString::from)
            .collect::<Vec<_>>();
        programs.extend(versioned_compilers());
        let tried = programs
            .iter()
            .map(|program| program.to_string_lossy())
            .collect::<Vec<_>>()
            .join(", ");
        programs
            .into_iter()
            .filter_map(|program| Self::new(&program, target.clone(), profile))
            .find(|compiler| compiler.probe(build_dir))
            .ok_or_else(|| {
                anyhow::anyhow!(
                    "no usable C11 compiler and linker found; tried {tried}; set CC to a compiler executable"
                )
            })
    }

    fn new(program: &OsStr, target: TargetConfig, profile: BuildProfile) -> Option<Self> {
        let program = resolve_program(program);
        let flavor = flavor_for(&program);
        let output = Command::new(&program)
            .arg(match flavor {
                Flavor::Unix => "--version",
                Flavor::Msvc => "/?",
            })
            .output()
            .ok()?;
        let mut version = output.stdout;
        version.extend(output.stderr);
        let clang = program_name(&program).starts_with("clang")
            || String::from_utf8_lossy(&version)
                .to_ascii_lowercase()
                .contains("clang");
        let host = TargetTriple::host().ok()?;
        if target.triple != host && !clang {
            return None;
        }
        if !target.triple.is_windows() && flavor == Flavor::Msvc {
            return None;
        }
        (!version.is_empty()).then_some(Self {
            program,
            flavor,
            version,
            clang,
            target,
            profile,
        })
    }

    fn probe(&self, build_dir: &Path) -> bool {
        let identity = self.identity();
        let stamp = build_dir.join(format!(".cc-{identity:016x}"));
        if stamp.is_file() {
            return true;
        }

        let source = build_dir.join(format!(".cc-{identity:016x}.c"));
        let executable = executable_path(&source, self.target.triple);
        if fs::write(&source, "int main(void) { return 0; }\n").is_err() {
            return false;
        }
        let success = self
            .command(&[source.as_path()], &executable, &[], false, &[])
            .output()
            .is_ok_and(|output| output.status.success() && executable.is_file());
        let _ = fs::remove_file(&source);
        let _ = fs::remove_file(&executable);
        let _ = fs::remove_file(source.with_extension("obj"));
        if success {
            let _ = fs::write(stamp, b"c11\n");
        }
        success
    }

    fn identity(&self) -> u64 {
        let mut hasher = DefaultHasher::new();
        self.program.hash(&mut hasher);
        self.flavor.hash(&mut hasher);
        self.version.hash(&mut hasher);
        self.clang.hash(&mut hasher);
        self.target.hash(&mut hasher);
        self.profile.hash(&mut hasher);
        hasher.finish()
    }

    fn compile(
        &self,
        sources: &[&Path],
        executable: &Path,
        defines: &[&str],
        shared_library: bool,
        extra_args: &[&str],
    ) -> anyhow::Result<()> {
        let status = self
            .command(sources, executable, defines, shared_library, extra_args)
            .status()
            .with_context(|| {
                format!(
                    "failed to run C compiler `{}`",
                    self.program.to_string_lossy()
                )
            })?;
        if !status.success() {
            bail!(
                "C compiler `{}` exited with {status}",
                self.program.to_string_lossy()
            );
        }
        Ok(())
    }

    fn compile_object(&self, source: &Path, object: &Path, pic: bool) -> anyhow::Result<()> {
        let status = self
            .command_mode(&[source], object, &[], &[], OutputMode::Object { pic })
            .status()?;
        if !status.success() {
            bail!(
                "C compiler `{}` exited with {status}",
                self.program.to_string_lossy()
            );
        }
        Ok(())
    }

    fn archive(&self, objects: &[&Path], output: &Path) -> anyhow::Result<()> {
        let (program, args) = if self.flavor == Flavor::Msvc {
            let program = if self.clang { "llvm-lib" } else { "lib" };
            let mut args = vec![format!("/OUT:{}", tool_path(output))];
            args.extend(objects.iter().map(|object| tool_path(object)));
            (program, args)
        } else {
            let mut args = vec!["crs".to_owned(), tool_path(output)];
            args.extend(objects.iter().map(|object| tool_path(object)));
            ("ar", args)
        };
        let status = Command::new(program)
            .args(&args)
            .current_dir(output.parent().unwrap_or_else(|| Path::new(".")))
            .status()
            .with_context(|| format!("failed to run archive tool `{program}`"))?;
        if !status.success() {
            bail!("archive tool `{program}` exited with {status}");
        }
        Ok(())
    }

    fn command(
        &self,
        sources: &[&Path],
        executable: &Path,
        defines: &[&str],
        shared_library: bool,
        extra_args: &[&str],
    ) -> Command {
        self.command_mode(
            sources,
            executable,
            defines,
            extra_args,
            if shared_library {
                OutputMode::SharedLibrary
            } else {
                OutputMode::Executable
            },
        )
    }

    fn command_mode(
        &self,
        sources: &[&Path],
        executable: &Path,
        defines: &[&str],
        extra_args: &[&str],
        mode: OutputMode,
    ) -> Command {
        let shared_library = matches!(mode, OutputMode::SharedLibrary);
        let compile_only = matches!(mode, OutputMode::Object { .. });
        let pic = matches!(mode, OutputMode::Object { pic: true });
        // MSYS2 gcc's linker fails on absolute paths containing non-ASCII
        // characters (the path reaches ld in the ANSI code page). When any
        // tool-facing path is non-ASCII, run the compiler from a common
        // parent with relative paths, which are encoding-agnostic. Paths
        // that are pure ASCII keep their original absolute form.
        let non_ascii = sources
            .iter()
            .copied()
            .chain(std::iter::once(executable))
            .chain(std::iter::once(Path::new(&self.program)))
            .any(|path| !path.as_os_str().to_string_lossy().is_ascii());
        let base = non_ascii
            .then(|| {
                common_base(
                    sources
                        .iter()
                        .copied()
                        .chain(std::iter::once(executable))
                        .collect::<Vec<_>>()
                        .as_slice(),
                )
            })
            .flatten();
        let source_args = sources
            .iter()
            .map(|source| relative_tool_path(source, base.as_deref()))
            .collect::<Vec<_>>();
        let mut command = Command::new(&self.program);
        if let Some(base_dir) = base.clone() {
            let program =
                std::path::absolute(&self.program).unwrap_or_else(|_| PathBuf::from(&self.program));
            command = Command::new(program);
            command.current_dir(&base_dir);
        }
        let host = TargetTriple::host().ok();
        let cross = host.is_some_and(|host| host != self.target.triple);
        match self.flavor {
            Flavor::Unix => {
                command.args([
                    "-std=c11",
                    if self.profile == BuildProfile::Release {
                        "-O2"
                    } else {
                        "-O0"
                    },
                ]);
                if cross && self.clang {
                    command.arg(format!("--target={}", self.target.triple));
                } else if matches!(
                    self.target.triple,
                    TargetTriple::I686UnknownLinuxGnu | TargetTriple::I686PcWindowsMsvc
                ) {
                    command.arg("-m32");
                } else if self.target.triple == TargetTriple::X86_64UnknownLinuxGnu {
                    command.arg("-m64");
                }
                if cross {
                    command.arg("-fuse-ld=lld");
                }
                if shared_library {
                    command.arg(if self.target.triple.is_macos() {
                        "-dynamiclib"
                    } else {
                        "-shared"
                    });
                    if !self.target.triple.is_windows() {
                        command.arg("-fPIC");
                    }
                } else if pic {
                    command.arg("-fPIC");
                }
                if compile_only {
                    command.arg("-c");
                }
                self.apply_unix_target_options(&mut command);
                for define in defines {
                    command.arg(format!("-D{define}"));
                }
                command
                    .args(&source_args)
                    .args(extra_args)
                    .arg("-o")
                    .arg(relative_tool_path(executable, base.as_deref()));
            }
            Flavor::Msvc => {
                command.args([
                    "/nologo",
                    "/std:c11",
                    "/utf-8",
                    if self.profile == BuildProfile::Release {
                        "/O2"
                    } else {
                        "/Od"
                    },
                ]);
                if self.clang && (cross || self.target.triple == TargetTriple::I686PcWindowsMsvc) {
                    command.arg(format!("--target={}", self.target.triple));
                }
                if cross && self.clang {
                    command.arg("-fuse-ld=lld");
                }
                if shared_library {
                    command.arg("/LD");
                }
                if compile_only {
                    command.arg("/c");
                }
                self.apply_msvc_target_options(&mut command);
                for define in defines {
                    command.arg(format!("/D{define}"));
                }
                if self.clang {
                    command.args(extra_args);
                }
                command.args(&source_args);
                if compile_only {
                    command.arg(format!("/Fo{}", executable.display()));
                } else {
                    command.arg(format!("/Fe{}", executable.display()));
                }
            }
        }
        command.current_dir(executable.parent().unwrap_or_else(|| Path::new(".")));
        command
    }

    fn apply_unix_target_options(&self, command: &mut Command) {
        if let Some(sysroot) = &self.target.c_toolchain.sysroot {
            if self.target.triple.is_macos() {
                command.arg("-isysroot").arg(sysroot);
            } else {
                command.arg(format!("--sysroot={}", sysroot.display()));
            }
        }
        if self.target.triple.is_windows() {
            for include in self.windows_include_paths() {
                command.arg("-isystem").arg(include);
            }
            for library in self.windows_library_paths() {
                command.arg("-L").arg(library);
            }
        }
    }

    fn apply_msvc_target_options(&self, command: &mut Command) {
        let includes = self.windows_include_paths();
        if !includes.is_empty()
            && let Ok(value) = env::join_paths(includes)
        {
            command.env("INCLUDE", value);
        }
        let libraries = self.windows_library_paths();
        if !libraries.is_empty()
            && let Ok(value) = env::join_paths(libraries)
        {
            command.env("LIB", value);
        }
    }

    fn windows_include_paths(&self) -> Vec<PathBuf> {
        let mut paths = Vec::new();
        if let Some(msvc) = &self.target.c_toolchain.msvc {
            paths.push(msvc.join("include"));
        }
        if let Some(sdk) = &self.target.c_toolchain.windows_sdk
            && let Some(version) = latest_child(&sdk.join("Include"))
        {
            for name in ["ucrt", "shared", "um", "winrt"] {
                paths.push(version.join(name));
            }
        }
        paths
    }

    fn windows_library_paths(&self) -> Vec<PathBuf> {
        let arch = self.target.triple.msvc_architecture();
        let mut paths = Vec::new();
        if let Some(msvc) = &self.target.c_toolchain.msvc {
            paths.push(msvc.join("lib").join(arch));
        }
        if let Some(sdk) = &self.target.c_toolchain.windows_sdk
            && let Some(version) = latest_child(&sdk.join("Lib"))
        {
            paths.push(version.join("ucrt").join(arch));
            paths.push(version.join("um").join(arch));
        }
        paths
    }
}

/// The deepest common ancestor directory of the paths, when they share one.
fn common_base(paths: &[&Path]) -> Option<std::path::PathBuf> {
    let mut base = std::path::absolute(paths.first()?)
        .ok()?
        .parent()?
        .to_path_buf();
    for path in paths {
        let absolute = std::path::absolute(path).ok()?;
        let mut components = absolute.components().peekable();
        let mut base_components = base.components().peekable();
        let mut common = std::path::PathBuf::new();
        loop {
            match (base_components.next(), components.next()) {
                (Some(a), Some(b)) if a == b => common.push(a.as_os_str()),
                _ => break,
            }
        }
        base = common;
    }
    if base.as_os_str().is_empty() {
        None
    } else {
        Some(base)
    }
}

/// A tool-facing path relative to `base` when one was chosen.
fn relative_tool_path(path: &Path, base: Option<&std::path::Path>) -> String {
    let Some(base) = base else {
        return tool_path(path);
    };
    let absolute = std::path::absolute(path).unwrap_or_else(|_| path.to_path_buf());
    match absolute.strip_prefix(base) {
        Ok(relative) => relative
            .to_string_lossy()
            .replace(std::path::MAIN_SEPARATOR, "/"),
        Err(_) => tool_path(path),
    }
}

fn tool_path(path: &Path) -> String {
    let path = path.to_string_lossy();
    if let Some(path) = path.strip_prefix(r"\\?\UNC\") {
        format!(r"\\{path}")
    } else {
        path.strip_prefix(r"\\?\").unwrap_or(&path).to_owned()
    }
}

fn latest_child(root: &Path) -> Option<PathBuf> {
    let mut directories = fs::read_dir(root)
        .ok()?
        .flatten()
        .map(|entry| entry.path())
        .filter(|path| path.is_dir())
        .collect::<Vec<_>>();
    directories.sort();
    directories.pop()
}

fn resolve_program(program: &OsStr) -> OsString {
    let path = Path::new(program);
    if path.components().count() > 1 {
        return ordinary_absolute(path).into_os_string();
    }
    let Some(search_path) = env::var_os("PATH") else {
        return program.to_owned();
    };
    for directory in env::split_paths(&search_path) {
        let direct = directory.join(path);
        if direct.is_file() {
            return ordinary_absolute(&direct).into_os_string();
        }
        if cfg!(windows) {
            let executable = directory.join(format!("{}.exe", program.to_string_lossy()));
            if executable.is_file() {
                return ordinary_absolute(&executable).into_os_string();
            }
        }
    }
    program.to_owned()
}

fn ordinary_absolute(path: &Path) -> PathBuf {
    if path.is_absolute() {
        path.to_path_buf()
    } else {
        env::current_dir().map_or_else(|_| path.to_path_buf(), |current| current.join(path))
    }
}

fn versioned_compilers() -> Vec<OsString> {
    let Some(path) = env::var_os("PATH") else {
        return Vec::new();
    };
    let mut programs = env::split_paths(&path)
        .enumerate()
        .flat_map(|(path_index, directory)| {
            fs::read_dir(directory)
                .into_iter()
                .flatten()
                .flatten()
                .filter_map(move |entry| {
                    let version = versioned_compiler_name(&entry.file_name())?;
                    entry
                        .path()
                        .is_file()
                        .then_some((version, path_index, entry.path()))
                })
        })
        .collect::<Vec<_>>();
    programs.sort_by(|left, right| {
        right
            .0
            .cmp(&left.0)
            .then(left.1.cmp(&right.1))
            .then(left.2.cmp(&right.2))
    });
    programs
        .into_iter()
        .map(|(_, _, path)| path.into_os_string())
        .collect()
}

fn versioned_compiler_name(name: &OsStr) -> Option<Vec<u32>> {
    let name = name.to_string_lossy().to_ascii_lowercase();
    let name = name.strip_suffix(".exe").unwrap_or(&name);
    let version = ["clang-cl-", "clang-", "gcc-"]
        .into_iter()
        .find_map(|prefix| name.strip_prefix(prefix))?;
    (!version.is_empty() && version.chars().all(|ch| ch.is_ascii_digit() || ch == '.')).then(|| {
        version
            .split('.')
            .map(|part| part.parse().unwrap_or(0))
            .collect()
    })
}

fn flavor_for(program: &OsStr) -> Flavor {
    let name = program_name(program);
    if name == "cl" || name == "clang-cl" || name.starts_with("clang-cl-") {
        Flavor::Msvc
    } else {
        Flavor::Unix
    }
}

fn program_name(program: &OsStr) -> String {
    let name = program
        .to_string_lossy()
        .rsplit(['/', '\\'])
        .next()
        .unwrap_or_default()
        .to_ascii_lowercase();
    name.strip_suffix(".exe").unwrap_or(&name).to_owned()
}

/// Directory of the global build cache for one target/profile, or `None`
/// when the cache is disabled via `[build] cache = false` in `Clue.toml`
/// or `CLUE_BUILD_CACHE=0`.
fn build_cache_entry(root: &Path, triple: TargetTriple, profile: BuildProfile) -> Option<PathBuf> {
    if env::var("CLUE_BUILD_CACHE").is_ok_and(|value| value == "0") {
        return None;
    }
    let manifest = crate::manifest::read(root, ProjectKind::Binary).ok()?;
    if manifest.build_cache == Some(false) {
        return None;
    }
    let profile_name = match profile {
        BuildProfile::Debug => "debug",
        BuildProfile::Release => "release",
    };
    crate::package::clue_home().ok().map(|home| {
        home.join("cache")
            .join("build")
            .join(triple.as_str())
            .join(profile_name)
    })
}

/// Copies every `{name}.*` build artifact (excluding fingerprints) from a
/// cache entry into the project's build directory.
fn restore_cached_library(entry: &Path, build_dir: &Path, name: &str) -> bool {
    let Ok(entries) = fs::read_dir(entry) else {
        return false;
    };
    let mut restored = false;
    for file in entries.flatten() {
        let owned_name = file.file_name().to_string_lossy().into_owned();
        let file_name = owned_name.as_str();
        if !file_name.starts_with(&format!("{name}.")) || file_name.ends_with(".hash") {
            continue;
        }
        let Ok(bytes) = fs::read(file.path()) else {
            return false;
        };
        if atomic_write(&build_dir.join(file_name), &bytes).is_err() {
            return false;
        }
        restored = true;
    }
    restored
}

/// Publishes a freshly built library's artifacts into the global cache.
/// Cache failures are non-fatal: the build itself already succeeded.
fn publish_cached_library(entry: &Path, build_dir: &Path, name: &str) {
    let Ok(entries) = fs::read_dir(build_dir) else {
        return;
    };
    let Ok(_) = fs::create_dir_all(entry) else {
        return;
    };
    for file in entries.flatten() {
        let owned_name = file.file_name().to_string_lossy().into_owned();
        let file_name = owned_name.as_str();
        if !file_name.starts_with(&format!("{name}."))
            || file_name.ends_with(".hash")
            || file_name.starts_with(".tmp-")
        {
            continue;
        }
        let Ok(bytes) = fs::read(file.path()) else {
            continue;
        };
        let target = entry.join(file_name);
        let temp = entry.join(format!(".{file_name}.tmp-{}", std::process::id()));
        if fs::write(&temp, &bytes).is_ok() {
            let _ = crate::lock::replace_file(&temp, &target);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::target::CToolchainConfig;

    fn shared_library_args(flavor: Flavor, triple: TargetTriple, clang: bool) -> Vec<String> {
        CCompiler {
            program: "cc".into(),
            flavor,
            version: Vec::new(),
            clang,
            target: TargetConfig {
                triple,
                runtime_source: None,
                c_toolchain: CToolchainConfig::default(),
            },
            profile: BuildProfile::Release,
        }
        .command(&[Path::new("input.c")], Path::new("output"), &[], true, &[])
        .get_args()
        .map(|argument| argument.to_string_lossy().into_owned())
        .collect()
    }

    #[test]
    fn shared_library_flags_cover_supported_platforms() {
        let linux = shared_library_args(Flavor::Unix, TargetTriple::I686UnknownLinuxGnu, false);
        assert!(linux.iter().any(|argument| argument == "-shared"));
        assert!(linux.iter().any(|argument| argument == "-fPIC"));
        assert!(linux.iter().any(|argument| argument == "-m32"));

        let macos = shared_library_args(Flavor::Unix, TargetTriple::Aarch64AppleDarwin, true);
        assert!(macos.iter().any(|argument| argument == "-dynamiclib"));

        let windows = shared_library_args(Flavor::Msvc, TargetTriple::I686PcWindowsMsvc, true);
        assert!(windows.iter().any(|argument| argument == "/LD"));
        assert!(windows.iter().any(|argument| argument == "/utf-8"));
        assert!(
            windows
                .iter()
                .any(|argument| argument == "--target=i686-pc-windows-msvc")
        );
    }
}
