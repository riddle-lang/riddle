mod build;
mod doc;
mod lock;
mod manifest;
mod model;
mod package;
mod proc_macro;
mod project;
mod target;
mod workspace;

use anyhow::bail;
use std::collections::hash_map::DefaultHasher;
use std::collections::{BTreeMap, HashMap};
use std::ffi::OsString;
use std::hash::{BuildHasher, Hash, Hasher};
use std::path::{Path, PathBuf};
use std::process::{Command, ExitStatus};
use std::sync::Arc;
use std::sync::atomic::{AtomicBool, Ordering};
use std::time::SystemTime;

pub use build::BuildProfile;
pub use manifest::CLUE_PROJECT_FILE_NAME;
pub use project::{ProjectKind, init, new};

static CANCELLED: AtomicBool = AtomicBool::new(false);

pub fn request_cancellation() {
    CANCELLED.store(true, Ordering::Release);
}

pub(crate) fn cancellation_requested() -> bool {
    CANCELLED.load(Ordering::Acquire)
}
pub use riddlec::target::TargetTriple;
pub use workspace::Workspace;

pub struct DependencyAddOptions {
    pub name: String,
    pub version: Option<String>,
    pub path: Option<PathBuf>,
    pub git: Option<String>,
    pub branch: Option<String>,
    pub tag: Option<String>,
    pub rev: Option<String>,
    pub registry: Option<String>,
    pub package: Option<String>,
    pub features: Vec<String>,
    pub default_features: bool,
    pub optional: bool,
    pub dev: bool,
}

pub struct InstallOptions {
    pub package: Option<String>,
    pub path: Option<PathBuf>,
    pub git: Option<String>,
    pub branch: Option<String>,
    pub tag: Option<String>,
    pub rev: Option<String>,
    pub version: Option<String>,
    pub registry: Option<String>,
    pub release: bool,
    pub offline: bool,
}

#[derive(Clone, Copy)]
struct TargetSelection<'a> {
    kind: model::TargetKind,
    name: Option<&'a str>,
    package: Option<&'a str>,
    workspace: bool,
}

pub fn add_dependency(root: &Path, options: &DependencyAddOptions) -> anyhow::Result<()> {
    package::add(
        root,
        &package::AddOptions {
            name: &options.name,
            version: options.version.as_deref(),
            path: options.path.as_deref(),
            git: options.git.as_deref(),
            branch: options.branch.as_deref(),
            tag: options.tag.as_deref(),
            rev: options.rev.as_deref(),
            registry: options.registry.as_deref(),
            package: options.package.as_deref(),
            features: &options.features,
            default_features: options.default_features,
            optional: options.optional,
            dev: options.dev,
        },
    )
}

pub fn remove_dependency(root: &Path, name: &str, dev: bool) -> anyhow::Result<()> {
    package::remove(root, name, dev)
}

pub fn fetch_packages(
    root: &Path,
    locked: bool,
    offline: bool,
    include_dev: bool,
    features: &[String],
) -> anyhow::Result<()> {
    if let Some(workspace) = workspace::load_for_path(root)? {
        let feature_package = workspace.member_for_path(root);
        if !features.is_empty() && feature_package.is_none() {
            bail!("--features from a workspace root requires selecting a member path");
        }
        package::prepare_workspace_target(
            &workspace.root,
            workspace.members(),
            locked,
            offline.then_some(true),
            include_dev,
            feature_package.as_deref(),
            features,
        )
        .map(|_| ())
    } else {
        package::prepare(root, locked, offline.then_some(true), include_dev, features).map(|_| ())
    }
}

pub fn update_packages(root: &Path, offline: bool) -> anyhow::Result<()> {
    if let Some(workspace) = workspace::load_for_path(root)? {
        package::update_workspace(
            &workspace.root,
            workspace.members(),
            offline.then_some(true),
        )
        .map(|_| ())
    } else {
        package::update(root, offline.then_some(true)).map(|_| ())
    }
}

pub fn dependency_tree(root: &Path) -> anyhow::Result<String> {
    dependency_tree_with_features(root, false)
}

pub fn dependency_tree_with_features(root: &Path, show_features: bool) -> anyhow::Result<String> {
    fetch_packages(root, false, false, false, &[])?;
    package::tree(root, show_features)
}

pub fn package_metadata(root: &Path) -> anyhow::Result<String> {
    fetch_packages(root, false, false, false, &[])?;
    package::metadata(root)
}

pub fn package_archive(root: &Path) -> anyhow::Result<PathBuf> {
    package::archive(root)
}

pub fn package_contents(root: &Path) -> anyhow::Result<Vec<PathBuf>> {
    package::package_list(root)
}

pub fn publish_package(root: &Path, registry: Option<&str>) -> anyhow::Result<PathBuf> {
    package::publish(root, registry, false)
}

pub fn publish_package_dry_run(root: &Path, registry: Option<&str>) -> anyhow::Result<PathBuf> {
    package::publish(root, registry, true)
}

pub fn install_package(root: &Path, options: &InstallOptions) -> anyhow::Result<PathBuf> {
    if usize::from(options.package.is_some())
        + usize::from(options.path.is_some())
        + usize::from(options.git.is_some())
        > 1
    {
        bail!("choose only one package name, --path, or --git");
    }
    if options.git.is_none()
        && (options.branch.is_some() || options.tag.is_some() || options.rev.is_some())
    {
        bail!("--branch, --tag, and --rev require --git");
    }
    if usize::from(options.branch.is_some())
        + usize::from(options.tag.is_some())
        + usize::from(options.rev.is_some())
        > 1
    {
        bail!("choose only one of --branch, --tag, or --rev");
    }
    if options.registry.is_some() && options.package.is_none() {
        bail!("--registry requires a package name");
    }
    let profile = if options.release {
        BuildProfile::Release
    } else {
        BuildProfile::Debug
    };
    let offline = options.offline.then_some(true);
    if let Some(path) = &options.path {
        return package::install_path(&root.join(path), profile, offline);
    }
    if let Some(git) = &options.git {
        let reference = if let Some(branch) = &options.branch {
            model::GitReference::Branch(branch.clone())
        } else if let Some(tag) = &options.tag {
            model::GitReference::Tag(tag.clone())
        } else if let Some(rev) = &options.rev {
            model::GitReference::Rev(rev.clone())
        } else {
            model::GitReference::DefaultBranch
        };
        return package::install_git(root, git, reference, profile, offline);
    }
    if let Some(package) = &options.package {
        let (package, inline_version) = package
            .rsplit_once('@')
            .filter(|(name, version)| !name.is_empty() && !version.is_empty())
            .map_or((package.as_str(), None), |(name, version)| {
                (name, Some(version))
            });
        if inline_version.is_some() && options.version.is_some() {
            bail!("specify the install version either as `name@requirement` or --version");
        }
        return package::install_registry(
            root,
            package,
            inline_version.or(options.version.as_deref()),
            options.registry.as_deref(),
            profile,
            offline,
        );
    }
    package::install_path(root, profile, offline)
}

pub fn uninstall_package(root: &Path, name: &str) -> anyhow::Result<PathBuf> {
    package::uninstall(root, name)
}

pub fn clean(root: &Path) -> anyhow::Result<()> {
    if let Some(workspace) = workspace::load_for_path(root)? {
        for member in workspace.members() {
            package::clean(member)?;
        }
        return package::clean(&workspace.root);
    }
    package::clean(root)
}

pub fn test_targets(
    root: &Path,
    selected: Option<&str>,
    no_run: bool,
    options: &CommandOptions,
) -> anyhow::Result<()> {
    test_targets_with_selection(root, selected, no_run, None, false, options)
}

pub fn test_targets_with_selection(
    root: &Path,
    selected: Option<&str>,
    no_run: bool,
    package: Option<&str>,
    workspace: bool,
    options: &CommandOptions,
) -> anyhow::Result<()> {
    run_selected_manifest_targets(
        root,
        TargetSelection {
            kind: model::TargetKind::Test,
            name: selected,
            package,
            workspace,
        },
        &[],
        no_run,
        options,
    )
}

pub fn bench_targets(
    root: &Path,
    selected: Option<&str>,
    args: &[OsString],
    options: &CommandOptions,
) -> anyhow::Result<()> {
    bench_targets_with_selection(root, selected, args, None, false, options)
}

pub fn bench_targets_with_selection(
    root: &Path,
    selected: Option<&str>,
    args: &[OsString],
    package: Option<&str>,
    workspace: bool,
    options: &CommandOptions,
) -> anyhow::Result<()> {
    run_selected_manifest_targets(
        root,
        TargetSelection {
            kind: model::TargetKind::Bench,
            name: selected,
            package,
            workspace,
        },
        args,
        false,
        options,
    )
}

pub fn run_example(
    root: &Path,
    selected: Option<&str>,
    args: &[OsString],
    options: &CommandOptions,
) -> anyhow::Result<ExitStatus> {
    run_example_with_selection(root, selected, args, None, options)
}

pub fn run_example_with_selection(
    root: &Path,
    selected: Option<&str>,
    args: &[OsString],
    package: Option<&str>,
    options: &CommandOptions,
) -> anyhow::Result<ExitStatus> {
    let statuses = run_selected_manifest_targets_collect(
        root,
        TargetSelection {
            kind: model::TargetKind::Example,
            name: selected,
            package,
            workspace: false,
        },
        args,
        false,
        options,
    )?;
    if statuses.len() != 1 {
        bail!("choose one example with `--example <name>`");
    }
    statuses
        .into_iter()
        .next()
        .flatten()
        .ok_or_else(|| anyhow::anyhow!("example was not run"))
}

pub fn build_examples(
    root: &Path,
    selected: Option<&str>,
    options: &CommandOptions,
) -> anyhow::Result<()> {
    build_examples_with_selection(root, selected, None, false, options)
}

pub fn build_examples_with_selection(
    root: &Path,
    selected: Option<&str>,
    package: Option<&str>,
    workspace: bool,
    options: &CommandOptions,
) -> anyhow::Result<()> {
    run_selected_manifest_targets(
        root,
        TargetSelection {
            kind: model::TargetKind::Example,
            name: selected,
            package,
            workspace,
        },
        &[],
        true,
        options,
    )
}

fn run_selected_manifest_targets(
    root: &Path,
    selection: TargetSelection<'_>,
    args: &[OsString],
    no_run: bool,
    options: &CommandOptions,
) -> anyhow::Result<()> {
    for status in run_selected_manifest_targets_collect(root, selection, args, no_run, options)? {
        if status.is_some_and(|status| !status.success()) {
            bail!("{} target failed", target_kind_name(selection.kind));
        }
    }
    Ok(())
}

fn run_selected_manifest_targets_collect(
    root: &Path,
    selection: TargetSelection<'_>,
    args: &[OsString],
    no_run: bool,
    options: &CommandOptions,
) -> anyhow::Result<Vec<Option<ExitStatus>>> {
    let packages = selected_target_packages(root, selection.package, selection.workspace)?;
    let mut statuses = Vec::new();
    for package in packages {
        let manifest = manifest::read(&package, ProjectKind::Binary)?;
        let targets = targets_for_kind(&manifest, selection.kind)?;
        if targets.is_empty()
            || selection
                .name
                .is_some_and(|name| !targets.iter().any(|target| target.name == name))
        {
            continue;
        }
        statuses.extend(run_manifest_targets_collect(
            &package,
            selection.kind,
            selection.name,
            args,
            no_run,
            options,
        )?);
    }
    if statuses.is_empty() {
        bail!(
            "{} target `{}` was not found",
            target_kind_name(selection.kind),
            selection.name.unwrap_or("*")
        );
    }
    Ok(statuses)
}

fn run_manifest_targets_collect(
    root: &Path,
    kind: model::TargetKind,
    selected: Option<&str>,
    args: &[OsString],
    no_run: bool,
    options: &CommandOptions,
) -> anyhow::Result<Vec<Option<ExitStatus>>> {
    let features = command_features(root, options)?;
    if let Some(workspace) = workspace::load_for_path(root)? {
        let member = workspace.member_for_path(root);
        package::prepare_workspace_target(
            &workspace.root,
            workspace.members(),
            options.locked,
            options.offline.then_some(true),
            true,
            member.as_deref(),
            &features,
        )?;
    } else {
        package::prepare(
            root,
            options.locked,
            options.offline.then_some(true),
            true,
            &features,
        )?;
    }
    let manifest = manifest::read(root, ProjectKind::Binary)?;
    let targets = targets_for_kind(&manifest, kind)?;
    let targets = targets
        .iter()
        .filter(|target| selected.is_none_or(|selected| target.name == selected))
        .collect::<Vec<_>>();
    if targets.is_empty() {
        bail!(
            "{} target `{}` was not found",
            target_kind_name(kind),
            selected.unwrap_or("*")
        );
    }
    let enabled_features =
        project::enabled_features(&manifest, &features, options.no_default_features)?;
    let mut statuses = Vec::new();
    for target in targets {
        let missing = target
            .required_features
            .iter()
            .filter(|feature| !enabled_features.contains(*feature))
            .cloned()
            .collect::<Vec<_>>();
        if !missing.is_empty() {
            if selected.is_some() {
                bail!(
                    "target `{}` requires features: {}",
                    target.name,
                    missing.join(", ")
                );
            }
            continue;
        }
        let artifact = build::run_target_with_options(
            root,
            None,
            options.profile,
            &project::LoadOptions {
                entry: Some(target.path.clone()),
                target_name: Some(target.name.clone()),
                features: features.clone(),
                no_default_features: options.no_default_features,
                include_dev: true,
                ..project::LoadOptions::default()
            },
        )?;
        let build::BuildArtifact::Executable {
            path,
            target: built,
        } = artifact
        else {
            bail!(
                "{} target did not produce an executable",
                target_kind_name(kind)
            );
        };
        if no_run {
            statuses.push(None);
            continue;
        }
        if built != TargetTriple::host().map_err(anyhow::Error::msg)? {
            bail!("cannot run target `{built}` on this host");
        }
        let status = Command::new(&path).args(args).current_dir(root).status()?;
        statuses.push(Some(status));
    }
    Ok(statuses)
}

fn targets_for_kind(
    manifest: &manifest::Manifest,
    kind: model::TargetKind,
) -> anyhow::Result<&[model::Target]> {
    match kind {
        model::TargetKind::Test => Ok(&manifest.tests),
        model::TargetKind::Example => Ok(&manifest.examples),
        model::TargetKind::Bench => Ok(&manifest.benches),
        _ => bail!("unsupported executable target kind"),
    }
}

fn selected_target_packages(
    path: &Path,
    package: Option<&str>,
    workspace_flag: bool,
) -> anyhow::Result<Vec<PathBuf>> {
    if workspace_flag && package.is_some() {
        bail!("`--workspace` cannot be combined with `--package`");
    }
    let Some(workspace) = workspace::load_for_path(path)? else {
        if package.is_some() || workspace_flag {
            bail!("`--package` and `--workspace` require a workspace root");
        }
        return Ok(vec![path.to_path_buf()]);
    };
    if let Some(name) = package {
        return workspace
            .package_by_name(name)
            .map(|package| vec![package.root.clone()])
            .ok_or_else(|| anyhow::anyhow!("package `{name}` is not a workspace crate"));
    }
    if workspace_flag || workspace.member_for_path(path).is_none() {
        Ok(workspace.ordered_members())
    } else {
        Ok(vec![
            workspace
                .member_for_path(path)
                .expect("member path was checked"),
        ])
    }
}

const fn target_kind_name(kind: model::TargetKind) -> &'static str {
    match kind {
        model::TargetKind::Library => "library",
        model::TargetKind::ProcMacro => "proc-macro",
        model::TargetKind::Binary => "binary",
        model::TargetKind::Test => "test",
        model::TargetKind::Example => "example",
        model::TargetKind::Bench => "bench",
    }
}

#[derive(Clone, Debug)]
pub struct CommandOptions {
    pub profile: BuildProfile,
    pub bin: Option<String>,
    pub locked: bool,
    pub features: Vec<String>,
    pub all_features: bool,
    pub all_targets: bool,
    pub no_default_features: bool,
    pub offline: bool,
    pub jobs: Option<usize>,
}

impl Default for CommandOptions {
    fn default() -> Self {
        Self {
            profile: BuildProfile::Debug,
            bin: None,
            locked: false,
            features: Vec::new(),
            all_features: false,
            all_targets: false,
            no_default_features: false,
            offline: false,
            jobs: None,
        }
    }
}

pub fn init_workspace(path: &Path) -> anyhow::Result<()> {
    workspace::init(path)
}

pub fn new_workspace(path: &Path) -> anyhow::Result<()> {
    workspace::new(path)
}

pub fn is_workspace_root(path: &Path) -> bool {
    workspace::is_workspace_root(path).unwrap_or(false)
}

pub fn is_virtual_workspace_root(path: &Path) -> bool {
    manifest::is_virtual_workspace_root(path).unwrap_or(false)
}

pub fn workspace_members(path: &Path) -> anyhow::Result<Vec<PathBuf>> {
    Ok(workspace::Workspace::load(path)?.members().to_vec())
}

#[derive(Clone)]
pub struct ProjectAnalysis {
    pub entry: PathBuf,
    pub source: riddlec::pipeline::LoadedSource,
    pub result: Arc<riddlec::pipeline::CompileResult>,
    pub macro_occurrences: Vec<riddlec::proc_macro::ProcMacroOccurrence>,
    pub macro_source_map: riddlec::pipeline::SourceMap,
    pub kind: ProjectKind,
    build_target: Option<String>,
    runtime_source: Option<PathBuf>,
    gc_enabled: bool,
    package_name: String,
    target_name: Option<String>,
    package_index: usize,
    manifest_fingerprint: String,
    package_version: semver::Version,
    library_types: Vec<model::LibraryType>,
}

#[derive(Default)]
pub struct ProjectSession {
    checker: riddlec::pipeline::CheckSession,
    cached: Option<CachedProject>,
    expanded: Option<CachedExpansion>,
    analysis: Option<CachedAnalysis>,
    /// Analyses keyed by the exact inputs that produced them.
    ///
    /// `analysis` is a single slot invalidated by every overlay change, so a
    /// caller that alternates between two versions of the same file — the
    /// language server does exactly this when it analyses a completion marker
    /// and then the repaired source — recompiles the whole project on every
    /// switch. This keeps a few recent results so those alternations hit.
    recent_analyses: Vec<CachedAnalysis>,
    proc_macros: Option<CachedProcMacroProvider>,
    revision: u64,
}

impl ProjectSession {
    /// Reuse counters from this session's most recent check.
    ///
    /// An editor edit should re-check the one function that changed; if this
    /// reports hundreds of checked bodies, the incremental state stopped
    /// matching and the whole standard library is being re-analysed.
    #[must_use]
    pub const fn last_check_stats(&self) -> type_checker::IncrementalStats {
        self.checker.last_stats()
    }
}

/// How many distinct input fingerprints a session remembers.
///
/// Three covers the shapes the language server produces in a row: the marked
/// source, its semicolon-repaired variant, and the unmodified buffer.
const RECENT_ANALYSIS_LIMIT: usize = 3;

struct CachedProject {
    package: project::LoadedPackage,
    overlays: BTreeMap<PathBuf, String>,
    disk: DiskStamps,
}

/// Size and modification time per watched file, as recorded by a load.
type DiskStamps = BTreeMap<PathBuf, Option<(u64, Option<SystemTime>)>>;

struct CachedExpansion {
    revision: u64,
    package: project::LoadedPackage,
    analysis: MacroAnalysis,
}

struct CachedAnalysis {
    revision: u64,
    /// Hash of the package path, compile options and every relevant overlay.
    fingerprint: u64,
    options: riddlec::pipeline::CompileOptions,
    depth: ProjectAnalysisDepth,
    analysis: ProjectAnalysis,
}

struct CachedProcMacroProvider {
    fingerprint: u64,
    provider: proc_macro::ClueProcMacroProvider,
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
enum ProjectAnalysisDepth {
    Resolve,
    Infer,
    Check,
}

impl ProjectSession {
    fn load<S: BuildHasher>(
        &mut self,
        path: &Path,
        overlays: &HashMap<PathBuf, String, S>,
    ) -> anyhow::Result<project::LoadedPackage> {
        let normalized_overlays = normalized_overlays(overlays);
        let tracing = phase_timing_enabled();
        let topology_changed = if let Some(cached) = &self.cached {
            let relevant_overlays =
                relevant_overlays(&normalized_overlays, &cached.package.watched_files);
            if relevant_overlays == cached.overlays
                && file_stamps(&cached.package.watched_files, &normalized_overlays) == cached.disk
            {
                if tracing {
                    eprintln!("[proj] load: unchanged, reusing package");
                }
                return Ok(cached.package.clone());
            }
            relevant_overlays.keys().ne(cached.overlays.keys())
        } else {
            false
        };

        if tracing {
            eprintln!(
                "[proj] load: reloading (topology_changed={topology_changed}), revision {} -> {}",
                self.revision,
                self.revision.wrapping_add(1)
            );
        }
        let package = project::load_with_overlays(path, overlays)?;
        if topology_changed {
            self.checker = riddlec::pipeline::CheckSession::default();
        }
        self.revision = self.revision.wrapping_add(1).max(1);
        self.expanded = None;
        self.analysis = None;
        self.cached = Some(CachedProject {
            overlays: relevant_overlays(&normalized_overlays, &package.watched_files),
            disk: file_stamps(&package.watched_files, &normalized_overlays),
            package: package.clone(),
        });
        Ok(package)
    }

    fn cached_analysis(
        &self,
        options: riddlec::pipeline::CompileOptions,
        depth: ProjectAnalysisDepth,
    ) -> Option<ProjectAnalysis> {
        self.analysis.as_ref().and_then(|cached| {
            (cached.revision == self.revision && cached.options == options && cached.depth >= depth)
                .then(|| cached.analysis.clone())
        })
    }

    /// Looks up an analysis whose inputs were byte-identical to these ones.
    fn cached_analysis_for(
        &self,
        fingerprint: u64,
        options: riddlec::pipeline::CompileOptions,
        depth: ProjectAnalysisDepth,
    ) -> Option<ProjectAnalysis> {
        self.recent_analyses
            .iter()
            .find(|cached| {
                cached.fingerprint == fingerprint
                    && cached.options == options
                    && cached.depth >= depth
            })
            .map(|cached| cached.analysis.clone())
    }

    fn remember_analysis(
        &mut self,
        fingerprint: u64,
        options: riddlec::pipeline::CompileOptions,
        depth: ProjectAnalysisDepth,
        analysis: &ProjectAnalysis,
    ) {
        self.recent_analyses
            .retain(|cached| !(cached.fingerprint == fingerprint && cached.options == options));
        self.recent_analyses.push(CachedAnalysis {
            revision: self.revision,
            fingerprint,
            options,
            depth,
            analysis: analysis.clone(),
        });
        if self.recent_analyses.len() > RECENT_ANALYSIS_LIMIT {
            self.recent_analyses.remove(0);
        }
    }

    fn cache_analysis(
        &mut self,
        options: riddlec::pipeline::CompileOptions,
        depth: ProjectAnalysisDepth,
        analysis: ProjectAnalysis,
    ) {
        self.analysis = Some(CachedAnalysis {
            revision: self.revision,
            // The single slot is only consulted when the revision still
            // matches, so its fingerprint is never compared.
            fingerprint: 0,
            options,
            depth,
            analysis,
        });
    }

    fn expand(
        &mut self,
        mut package: project::LoadedPackage,
    ) -> anyhow::Result<(project::LoadedPackage, MacroAnalysis)> {
        if let Some(cached) = &self.expanded
            && cached.revision == self.revision
        {
            return Ok((cached.package.clone(), cached.analysis.clone()));
        }

        let fingerprint = proc_macro_fingerprint(&package.proc_macros);
        if self
            .proc_macros
            .as_ref()
            .is_none_or(|cached| cached.fingerprint != fingerprint)
        {
            // Windows keeps a loaded proc-macro DLL locked until its worker exits.
            self.proc_macros = None;
            let provider = proc_macro::ClueProcMacroProvider::build(&package.proc_macros)?;
            self.proc_macros = Some(CachedProcMacroProvider {
                fingerprint,
                provider,
            });
        }
        let provider = &mut self
            .proc_macros
            .as_mut()
            .expect("proc-macro provider was initialized")
            .provider;
        let analysis = expand_proc_macros_with_provider(&mut package, provider)?;
        self.expanded = Some(CachedExpansion {
            revision: self.revision,
            package: package.clone(),
            analysis: analysis.clone(),
        });
        Ok((package, analysis))
    }

    #[must_use]
    pub const fn revision(&self) -> u64 {
        self.revision
    }

    #[must_use]
    pub fn inputs_are_current<S: BuildHasher>(
        &self,
        overlays: &HashMap<PathBuf, String, S>,
    ) -> bool {
        let normalized_overlays = normalized_overlays(overlays);
        self.cached.as_ref().is_some_and(|cached| {
            relevant_overlays(&normalized_overlays, &cached.package.watched_files)
                == cached.overlays
                && file_stamps(&cached.package.watched_files, &normalized_overlays) == cached.disk
        })
    }
}

fn proc_macro_fingerprint(packages: &[project::ProcMacroPackage]) -> u64 {
    let mut hasher = DefaultHasher::new();
    for package in packages {
        package.root.hash(&mut hasher);
        package.alias.hash(&mut hasher);
        package.name.hash(&mut hasher);
        package.entry.hash(&mut hasher);
        package.source.source.hash(&mut hasher);
        package.manifest_fingerprint.hash(&mut hasher);
    }
    hasher.finish()
}

fn normalized_overlays<S: BuildHasher>(
    overlays: &HashMap<PathBuf, String, S>,
) -> HashMap<PathBuf, String> {
    overlays
        .iter()
        .map(|(path, source)| (normalized_path(path), source.clone()))
        .collect()
}

fn relevant_overlays(
    overlays: &HashMap<PathBuf, String>,
    files: &[PathBuf],
) -> BTreeMap<PathBuf, String> {
    files
        .iter()
        .filter_map(|path| {
            overlays
                .get(path)
                .map(|source| (path.clone(), source.clone()))
        })
        .collect()
}

fn file_stamps(
    files: &[PathBuf],
    overlays: &HashMap<PathBuf, String>,
) -> BTreeMap<PathBuf, Option<(u64, Option<SystemTime>)>> {
    files
        .iter()
        .filter(|path| !overlays.contains_key(*path))
        .map(|path| {
            let stamp = std::fs::metadata(path)
                .ok()
                .map(|metadata| (metadata.len(), metadata.modified().ok()));
            (path.clone(), stamp)
        })
        .collect()
}

/// Whether the pipeline's phase tracing is on, under `RIDDLEC_PHASE_TIMING`.
///
/// Read once per call so the tracing costs nothing on the hot path.
fn phase_timing_enabled() -> bool {
    std::env::var_os("RIDDLEC_PHASE_TIMING").is_some()
}

pub fn find_project_root(path: &Path) -> Option<PathBuf> {
    let start = if path.is_dir() { path } else { path.parent()? };
    start
        .ancestors()
        .find(|path| {
            path.join(manifest::CLUE_PROJECT_FILE_NAME).is_file()
                && !manifest::is_virtual_workspace_root(path).unwrap_or(false)
        })
        .map(normalized_path)
}

fn normalized_path(path: &Path) -> PathBuf {
    std::fs::canonicalize(path).unwrap_or_else(|_| {
        path.parent()
            .and_then(|parent| std::fs::canonicalize(parent).ok())
            .and_then(|parent| path.file_name().map(|name| parent.join(name)))
            .unwrap_or_else(|| path.to_path_buf())
    })
}

/// Analyzes a project using the default compiler options.
///
/// # Errors
///
/// Returns an error when the project cannot be loaded or process macros cannot be expanded.
pub fn analyze_project<S: BuildHasher>(
    path: &Path,
    overlays: &HashMap<PathBuf, String, S>,
) -> anyhow::Result<ProjectAnalysis> {
    analyze_project_with_options(path, overlays, riddlec::pipeline::CompileOptions::default())
}

/// Analyzes a project using explicit compiler options.
///
/// # Errors
///
/// Returns an error when the project cannot be loaded or process macros cannot be expanded.
pub fn analyze_project_with_options<S: BuildHasher>(
    path: &Path,
    overlays: &HashMap<PathBuf, String, S>,
    options: riddlec::pipeline::CompileOptions,
) -> anyhow::Result<ProjectAnalysis> {
    analyze_project_impl(path, overlays, options, true)
}

/// Checks a project without generating code.
///
/// # Errors
///
/// Returns an error when the project cannot be loaded or process macros cannot be expanded.
pub fn check_project_with_options<S: BuildHasher>(
    path: &Path,
    overlays: &HashMap<PathBuf, String, S>,
    options: riddlec::pipeline::CompileOptions,
) -> anyhow::Result<ProjectAnalysis> {
    analyze_project_impl(path, overlays, options, false)
}

/// Resolves a project using an incremental session.
///
/// # Errors
///
/// Returns an error when project loading, macro expansion, or analysis fails.
pub fn resolve_project_with_session<S: BuildHasher>(
    path: &Path,
    overlays: &HashMap<PathBuf, String, S>,
    options: riddlec::pipeline::CompileOptions,
    session: &mut ProjectSession,
) -> anyhow::Result<ProjectAnalysis> {
    resolve_project_with_session_cancellable(path, overlays, options, session, || false)?
        .ok_or_else(|| anyhow::anyhow!("project analysis cancelled"))
}

/// Resolves a project using an incremental session and cancellation callback.
///
/// # Errors
///
/// Returns an error when project loading, macro expansion, or analysis fails.
pub fn resolve_project_with_session_cancellable<S: BuildHasher>(
    path: &Path,
    overlays: &HashMap<PathBuf, String, S>,
    options: riddlec::pipeline::CompileOptions,
    session: &mut ProjectSession,
    cancelled: impl Fn() -> bool,
) -> anyhow::Result<Option<ProjectAnalysis>> {
    analyze_project_with_session_cancellable(
        path,
        overlays,
        options,
        session,
        ProjectAnalysisDepth::Resolve,
        cancelled,
    )
}

/// Infers project types using an incremental session without running ownership analysis.
///
/// # Errors
///
/// Returns an error when project loading, macro expansion, or type inference fails.
pub fn infer_project_with_session_cancellable<S: BuildHasher>(
    path: &Path,
    overlays: &HashMap<PathBuf, String, S>,
    options: riddlec::pipeline::CompileOptions,
    session: &mut ProjectSession,
    cancelled: impl Fn() -> bool,
) -> anyhow::Result<Option<ProjectAnalysis>> {
    analyze_project_with_session_cancellable(
        path,
        overlays,
        options,
        session,
        ProjectAnalysisDepth::Infer,
        cancelled,
    )
}

/// Checks a project using an incremental session.
///
/// # Errors
///
/// Returns an error when project loading, macro expansion, or analysis fails.
pub fn check_project_with_session<S: BuildHasher>(
    path: &Path,
    overlays: &HashMap<PathBuf, String, S>,
    options: riddlec::pipeline::CompileOptions,
    session: &mut ProjectSession,
) -> anyhow::Result<ProjectAnalysis> {
    check_project_with_session_cancellable(path, overlays, options, session, || false)?
        .ok_or_else(|| anyhow::anyhow!("project analysis cancelled"))
}

/// Checks a project using an incremental session and cancellation callback.
///
/// # Errors
///
/// Returns an error when project loading, macro expansion, or analysis fails.
pub fn check_project_with_session_cancellable<S: BuildHasher>(
    path: &Path,
    overlays: &HashMap<PathBuf, String, S>,
    options: riddlec::pipeline::CompileOptions,
    session: &mut ProjectSession,
    cancelled: impl Fn() -> bool,
) -> anyhow::Result<Option<ProjectAnalysis>> {
    analyze_project_with_session_cancellable(
        path,
        overlays,
        options,
        session,
        ProjectAnalysisDepth::Check,
        cancelled,
    )
}

fn analyze_project_with_session_cancellable<S: BuildHasher>(
    path: &Path,
    overlays: &HashMap<PathBuf, String, S>,
    options: riddlec::pipeline::CompileOptions,
    session: &mut ProjectSession,
    depth: ProjectAnalysisDepth,
    cancelled: impl Fn() -> bool,
) -> anyhow::Result<Option<ProjectAnalysis>> {
    if cancelled() {
        return Ok(None);
    }
    let package = session.load(path, overlays)?;
    if let Some(analysis) = session.cached_analysis(options, depth) {
        return Ok(Some(analysis));
    }
    // The single-slot cache above is invalidated by any overlay change. This
    // one survives an alternating input pattern, which is what the language
    // server produces when it analyses a completion marker and the repaired
    // source in the same keystroke.
    let disk = session.cached.as_ref().map(|cached| cached.disk.clone());
    let fingerprint = analysis_fingerprint(path, overlays, disk.as_ref(), options);
    if let Some(analysis) = session.cached_analysis_for(fingerprint, options, depth) {
        return Ok(Some(analysis));
    }
    let (package, macro_analysis) = session.expand(package)?;
    if cancelled() {
        return Ok(None);
    }
    let result = match (depth, package.macro_parse.as_ref()) {
        (ProjectAnalysisDepth::Resolve, Some(parse)) => session
            .checker
            .resolve_parsed_package_with_options_and_gc_cancellable(
                &package.source.source,
                parse,
                &package.package_ranges,
                options,
                package.gc_enabled,
                &cancelled,
            ),
        (ProjectAnalysisDepth::Resolve, None) => session
            .checker
            .resolve_package_with_options_and_gc_cancellable(
                &package.source.source,
                &package.package_ranges,
                options,
                package.gc_enabled,
                &cancelled,
            ),
        (ProjectAnalysisDepth::Infer, Some(parse)) => session
            .checker
            .infer_parsed_package_with_options_and_gc_cancellable(
                &package.source.source,
                parse,
                &package.package_ranges,
                options,
                package.gc_enabled,
                &cancelled,
            ),
        (ProjectAnalysisDepth::Infer, None) => session
            .checker
            .infer_package_with_options_and_gc_cancellable(
                &package.source.source,
                &package.package_ranges,
                options,
                package.gc_enabled,
                &cancelled,
            ),
        (ProjectAnalysisDepth::Check, Some(parse)) => session
            .checker
            .check_parsed_package_with_options_and_gc_cancellable(
                &package.source.source,
                parse,
                &package.package_ranges,
                options,
                package.gc_enabled,
                &cancelled,
            ),
        (ProjectAnalysisDepth::Check, None) => session
            .checker
            .check_package_with_options_and_gc_cancellable(
                &package.source.source,
                &package.package_ranges,
                options,
                package.gc_enabled,
                &cancelled,
            ),
    };
    let Some(mut result) = result else {
        return Ok(None);
    };
    result.macro_diagnostics = macro_analysis.diagnostics;
    let package_index = package.package_ranges.len().saturating_sub(1);
    let analysis = ProjectAnalysis {
        entry: package.entry,
        source: package.source,
        result: Arc::new(result),
        macro_occurrences: macro_analysis.occurrences,
        macro_source_map: macro_analysis.source_map,
        kind: package.kind,
        build_target: package.build_target,
        runtime_source: package.runtime_source,
        gc_enabled: package.gc_enabled,
        package_name: package.name,
        target_name: package.target_name,
        package_index,
        manifest_fingerprint: package.manifest_fingerprint,
        package_version: package.version,
        library_types: package.library_types,
    };
    session.cache_analysis(options, depth, analysis.clone());
    session.remember_analysis(fingerprint, options, depth, &analysis);
    Ok(Some(analysis))
}

/// Identity of the inputs an analysis was produced from.
///
/// Hashes the caller's overlays *and* the file stamps the session just checked,
/// so an edit to a module that is not open in the editor (visible only as a
/// changed mtime) invalidates the entry too. Anything the language server
/// cannot observe without loading the package would be unsafe to cache on.
fn analysis_fingerprint<S: BuildHasher>(
    path: &Path,
    overlays: &HashMap<PathBuf, String, S>,
    disk: Option<&DiskStamps>,
    options: riddlec::pipeline::CompileOptions,
) -> u64 {
    let mut hasher = std::collections::hash_map::DefaultHasher::new();
    use std::hash::Hash as _;
    path.hash(&mut hasher);
    options.use_std.hash(&mut hasher);
    let mut entries = overlays
        .iter()
        .map(|(path, source)| (normalized_path(path.as_path()), source))
        .collect::<Vec<_>>();
    entries.sort_by(|left, right| left.0.cmp(&right.0));
    entries.len().hash(&mut hasher);
    for (path, source) in entries {
        path.hash(&mut hasher);
        source.hash(&mut hasher);
    }
    if let Some(disk) = disk {
        disk.len().hash(&mut hasher);
        for (path, stamp) in disk {
            path.hash(&mut hasher);
            stamp.hash(&mut hasher);
        }
    }
    hasher.finish()
}

fn analyze_project_impl<S: BuildHasher>(
    path: &Path,
    overlays: &HashMap<PathBuf, String, S>,
    options: riddlec::pipeline::CompileOptions,
    build: bool,
) -> anyhow::Result<ProjectAnalysis> {
    analyze_project_impl_with_load_options(
        path,
        overlays,
        options,
        build,
        &project::LoadOptions::default(),
    )
}

fn analyze_project_impl_with_load_options<S: BuildHasher>(
    path: &Path,
    overlays: &HashMap<PathBuf, String, S>,
    options: riddlec::pipeline::CompileOptions,
    build: bool,
    load_options: &project::LoadOptions,
) -> anyhow::Result<ProjectAnalysis> {
    let mut package = project::load_with_overlays_and_options(path, overlays, load_options)?;
    let macro_analysis = expand_proc_macros(&mut package)?;
    let mut result = match (&package.macro_parse, build) {
        (Some(parse), true) => {
            riddlec::pipeline::compile_parsed_package_with_options_and_gc_and_names(
                &package.source.source,
                parse,
                &package.package_ranges,
                &package.package_names,
                options,
                package.gc_enabled,
            )
        }
        (Some(parse), false) => riddlec::pipeline::check_parsed_package_with_options_and_gc(
            &package.source.source,
            parse,
            &package.package_ranges,
            options,
            package.gc_enabled,
        ),
        (None, true) => riddlec::pipeline::compile_package_with_options_and_gc_and_names(
            &package.source.source,
            &package.package_ranges,
            &package.package_names,
            options,
            package.gc_enabled,
        ),
        (None, false) => riddlec::pipeline::check_package_with_options_and_gc(
            &package.source.source,
            &package.package_ranges,
            options,
            package.gc_enabled,
        ),
    };
    result.macro_diagnostics = macro_analysis.diagnostics;
    let package_index = package.package_ranges.len().saturating_sub(1);
    Ok(ProjectAnalysis {
        entry: package.entry,
        source: package.source,
        result: Arc::new(result),
        macro_occurrences: macro_analysis.occurrences,
        macro_source_map: macro_analysis.source_map,
        kind: package.kind,
        build_target: package.build_target,
        runtime_source: package.runtime_source,
        gc_enabled: package.gc_enabled,
        package_name: package.name,
        target_name: package.target_name,
        package_index,
        manifest_fingerprint: package.manifest_fingerprint,
        package_version: package.version,
        library_types: package.library_types,
    })
}

/// Compile options for a build of `triple`: `usize` and `isize` are measured at
/// the width that build's C compiler will give `size_t`.
pub(crate) fn analysis_options(triple: TargetTriple) -> riddlec::pipeline::CompileOptions {
    riddlec::pipeline::CompileOptions {
        pointer_width_bits: triple.pointer_width_bits(),
        ..riddlec::pipeline::CompileOptions::default()
    }
}

/// Analyzes a project at the pointer width its build will use.
///
/// `build.target` in Clue.toml is only visible once the package is loaded, and
/// loading happens inside the analysis, so a cross-width project is analysed
/// twice: the first pass names the triple, the second re-checks integer
/// literals against the width that will really be compiled for. A `usize`
/// literal that needs 64 bits cannot live in a 32-bit `size_t`, and C truncates
/// it without a word. Building at the host's width — the usual case — costs one
/// analysis.
///
/// # Errors
///
/// Reports the project load and analysis errors, and an unusable target triple.
pub(crate) fn analyze_for_built_target(
    path: &Path,
    explicit_target: Option<TargetTriple>,
    build: bool,
    load_options: &project::LoadOptions,
) -> anyhow::Result<ProjectAnalysis> {
    let assumed = target::resolve(explicit_target, None)?;
    let analysis = analyze_project_impl_with_load_options(
        path,
        &HashMap::new(),
        analysis_options(assumed),
        build,
        load_options,
    )?;
    let built = target::resolve(explicit_target, analysis.build_target.as_deref())?;
    if built.pointer_width_bits() == assumed.pointer_width_bits() {
        return Ok(analysis);
    }
    analyze_project_impl_with_load_options(
        path,
        &HashMap::new(),
        analysis_options(built),
        build,
        load_options,
    )
}

#[derive(Clone)]
struct MacroAnalysis {
    diagnostics: Vec<type_checker::Diagnostic>,
    occurrences: Vec<riddlec::proc_macro::ProcMacroOccurrence>,
    source_map: riddlec::pipeline::SourceMap,
}

fn expand_proc_macros(package: &mut project::LoadedPackage) -> anyhow::Result<MacroAnalysis> {
    let mut provider = proc_macro::ClueProcMacroProvider::build(&package.proc_macros)?;
    expand_proc_macros_with_provider(package, &mut provider)
}

fn expand_proc_macros_with_provider(
    package: &mut project::LoadedPackage,
    provider: &mut proc_macro::ClueProcMacroProvider,
) -> anyhow::Result<MacroAnalysis> {
    let source_map = package.source.source_map.clone();
    let host_exports = (package.kind == ProjectKind::ProcMacro)
        .then(|| proc_macro::discover_exports(&package.source.source))
        .transpose()?;
    let expansion = riddlec::proc_macro::expand_source(&package.source.source, provider);
    let riddlec::proc_macro::ExpandedSource {
        source,
        parse,
        mappings,
        macro_occurrences,
        mut diagnostics,
        ..
    } = expansion;
    remap_package_ranges(&mut package.package_ranges, &mappings);
    package.source.apply_expansion(source, &mappings);
    package.macro_parse = parse;
    if let Some(exports) = host_exports {
        prepare_proc_macro_analysis(package, &exports, &mut diagnostics);
    }
    Ok(MacroAnalysis {
        diagnostics,
        occurrences: macro_occurrences,
        source_map,
    })
}

fn prepare_proc_macro_analysis(
    package: &mut project::LoadedPackage,
    exports: &[proc_macro::HostMacroExport],
    diagnostics: &mut [type_checker::Diagnostic],
) {
    let prefix = proc_macro::host_prefix();
    let suffix = proc_macro::host_suffix(exports);
    let offset = prefix.len() + 1;
    let mut source = String::with_capacity(offset + package.source.source.len() + suffix.len());
    source.push_str(prefix);
    source.push('\n');
    source.push_str(&package.source.source);
    source.push_str(&suffix);

    let mut prefix_parser = frontend::incremental::IncrementalParser::new();
    let prefix_parse = prefix_parser.set_source(prefix).clone();
    let source_parse = package
        .macro_parse
        .as_ref()
        .expect("macro expansion always returns a parse");
    let parse = frontend::tree_builder::append_parse(&prefix_parse, "\n", source_parse);
    let mut suffix_parser = frontend::incremental::IncrementalParser::new();
    let suffix_parse = suffix_parser.set_source(&suffix).clone();
    package.macro_parse = Some(frontend::tree_builder::append_parse(
        &parse,
        "",
        &suffix_parse,
    ));

    let mut shifted_map = riddlec::pipeline::SourceMap::default();
    shifted_map.extend(std::mem::take(&mut package.source.source_map), offset);
    package.source.source_map = shifted_map;
    for range in &mut package.package_ranges {
        range.start += offset;
        range.end += offset;
    }
    if let Some(own_package) = package.package_ranges.last_mut() {
        own_package.start = 0;
        own_package.end = source.len();
    }
    package.source.source = source;

    let offset = rowan::TextSize::from(
        u32::try_from(offset).expect("combined package offset should fit in u32"),
    );
    for diagnostic in diagnostics {
        for label in &mut diagnostic.labels {
            label.range =
                rowan::TextRange::new(label.range.start() + offset, label.range.end() + offset);
        }
    }
}

fn remap_package_ranges(
    ranges: &mut [std::ops::Range<usize>],
    mappings: &[riddlec::proc_macro::ExpandedTokenMapping],
) {
    for range in ranges {
        let generated = mappings
            .iter()
            .filter(|mapping| {
                mapping.original.start < range.end && range.start < mapping.original.end
            })
            .map(|mapping| mapping.generated.clone())
            .collect::<Vec<_>>();
        if let (Some(start), Some(end)) = (
            generated.iter().map(|range| range.start).min(),
            generated.iter().map(|range| range.end).max(),
        ) {
            *range = start..end;
        } else {
            let anchor = mappings
                .iter()
                .filter(|mapping| mapping.original.end <= range.start)
                .map(|mapping| mapping.generated.end)
                .max()
                .unwrap_or(0);
            *range = anchor..anchor;
        }
    }
}

/// Checks a package for the host target.
///
/// # Errors
///
/// Returns an error when loading, target resolution, or compilation fails.
pub fn check(path: &Path) -> anyhow::Result<()> {
    check_for_target(path, None)
}

/// Checks a package for an optional explicit target.
///
/// # Errors
///
/// Returns an error when loading, target resolution, or compilation fails.
pub fn check_for_target(path: &Path, explicit_target: Option<TargetTriple>) -> anyhow::Result<()> {
    check_for_target_with_selection(path, explicit_target, None, false)
}

pub fn check_for_target_with_selection(
    path: &Path,
    explicit_target: Option<TargetTriple>,
    package: Option<&str>,
    workspace_flag: bool,
) -> anyhow::Result<()> {
    check_for_target_with_options(
        path,
        explicit_target,
        package,
        workspace_flag,
        &CommandOptions::default(),
    )
}

pub fn check_for_target_with_options(
    path: &Path,
    explicit_target: Option<TargetTriple>,
    package: Option<&str>,
    workspace_flag: bool,
    command_options: &CommandOptions,
) -> anyhow::Result<()> {
    if let Some((workspace, members)) = selected_workspace(
        path,
        package,
        workspace_flag,
        command_options,
        command_options.bin.as_deref(),
    )? {
        parallel_workspace_packages(&workspace, &members, command_options.jobs, |member| {
            check_package(member, explicit_target, command_options)
        })?;
        return Ok(());
    }
    if package.is_some() || workspace_flag {
        bail!("`--package` and `--workspace` require a workspace root");
    }
    let features = command_features(path, command_options)?;
    package::prepare(
        path,
        command_options.locked,
        command_options.offline.then_some(true),
        false,
        &features,
    )?;
    check_package(path, explicit_target, command_options)
}

fn check_package(
    path: &Path,
    explicit_target: Option<TargetTriple>,
    options: &CommandOptions,
) -> anyhow::Result<()> {
    let features = command_features(path, options)?;
    for bin in command_bins(
        path,
        options.bin.as_deref(),
        &features,
        options.no_default_features,
    )? {
        let analysis = analyze_for_built_target(
            path,
            explicit_target,
            false,
            &project::LoadOptions {
                bin: bin.clone(),
                features: features.clone(),
                no_default_features: options.no_default_features,
                require_bin: true,
                ..project::LoadOptions::default()
            },
        )?;
        let errors = riddlec::diagnostics::report_mapped(
            &analysis.result,
            &analysis.source,
            &analysis.entry.display().to_string(),
        );
        if errors > 0 || !analysis.result.success() {
            bail!("check failed");
        }
        println!("clue: checked {}", analysis.entry.display());
    }
    if options.all_targets {
        for kind in [
            model::TargetKind::Test,
            model::TargetKind::Example,
            model::TargetKind::Bench,
        ] {
            check_manifest_targets(path, kind, options, &features)?;
        }
    }
    Ok(())
}

fn check_manifest_targets(
    root: &Path,
    kind: model::TargetKind,
    options: &CommandOptions,
    features: &[String],
) -> anyhow::Result<()> {
    let manifest = manifest::read(root, ProjectKind::Binary)?;
    let enabled = project::enabled_features(&manifest, features, options.no_default_features)?;
    let targets = targets_for_kind(&manifest, kind)?;
    for target in targets {
        if !target
            .required_features
            .iter()
            .all(|feature| enabled.contains(feature))
        {
            continue;
        }
        if let Some(workspace) = workspace::load_for_path(root)? {
            package::prepare_workspace_target(
                &workspace.root,
                workspace.members(),
                options.locked,
                options.offline.then_some(true),
                true,
                workspace.member_for_path(root).as_deref(),
                features,
            )?;
        } else {
            package::prepare(
                root,
                options.locked,
                options.offline.then_some(true),
                true,
                features,
            )?;
        }
        let analysis = analyze_for_built_target(
            root,
            None,
            false,
            &project::LoadOptions {
                entry: Some(target.path.clone()),
                target_name: Some(target.name.clone()),
                features: features.to_vec(),
                no_default_features: options.no_default_features,
                include_dev: true,
                require_bin: false,
                ..project::LoadOptions::default()
            },
        )?;
        let errors = riddlec::diagnostics::report_mapped(
            &analysis.result,
            &analysis.source,
            &analysis.entry.display().to_string(),
        );
        if errors > 0 || !analysis.result.success() {
            bail!("{} target check failed", target_kind_name(kind));
        }
        println!("clue: checked {} {}", target_kind_name(kind), target.name);
    }
    Ok(())
}

/// Builds a package for the host target.
///
/// # Errors
///
/// Returns an error when project analysis, code generation, or linking fails.
pub fn build(path: &Path) -> anyhow::Result<()> {
    build_for_target(path, None)
}

/// Builds a package for an optional explicit target.
///
/// # Errors
///
/// Returns an error when project analysis, code generation, or linking fails.
pub fn build_for_target(path: &Path, explicit_target: Option<TargetTriple>) -> anyhow::Result<()> {
    build_for_target_with_selection(path, explicit_target, None, false)
}

pub fn build_for_target_with_selection(
    path: &Path,
    explicit_target: Option<TargetTriple>,
    package: Option<&str>,
    workspace_flag: bool,
) -> anyhow::Result<()> {
    build_for_target_with_profile(
        path,
        explicit_target,
        package,
        workspace_flag,
        BuildProfile::Debug,
    )
}

pub fn build_for_target_with_options(
    path: &Path,
    explicit_target: Option<TargetTriple>,
    package: Option<&str>,
    workspace_flag: bool,
    command_options: &CommandOptions,
) -> anyhow::Result<()> {
    if let Some((workspace, members)) = selected_workspace(
        path,
        package,
        workspace_flag,
        command_options,
        command_options.bin.as_deref(),
    )? {
        parallel_workspace_packages(&workspace, &members, command_options.jobs, |member| {
            build_package(member, explicit_target, command_options)
        })?;
        return Ok(());
    }
    if package.is_some() || workspace_flag {
        bail!("`--package` and `--workspace` require a workspace root");
    }
    let features = command_features(path, command_options)?;
    package::prepare(
        path,
        command_options.locked,
        command_options.offline.then_some(true),
        false,
        &features,
    )?;
    build_package(path, explicit_target, command_options)
}

fn build_package(
    path: &Path,
    explicit_target: Option<TargetTriple>,
    options: &CommandOptions,
) -> anyhow::Result<()> {
    let features = command_features(path, options)?;
    for bin in command_bins(
        path,
        options.bin.as_deref(),
        &features,
        options.no_default_features,
    )? {
        build::run_with_options(
            path,
            explicit_target,
            options.profile,
            bin.as_deref(),
            &features,
            options.no_default_features,
        )?;
    }
    if options.all_targets {
        for kind in [
            model::TargetKind::Test,
            model::TargetKind::Example,
            model::TargetKind::Bench,
        ] {
            run_manifest_targets_collect(path, kind, None, &[], true, options)?;
        }
    }
    Ok(())
}

fn command_bins(
    path: &Path,
    selected: Option<&str>,
    features: &[String],
    no_default_features: bool,
) -> anyhow::Result<Vec<Option<String>>> {
    if let Some(selected) = selected {
        return Ok(vec![Some(selected.to_owned())]);
    }
    let manifest = manifest::read(path, ProjectKind::Binary)?;
    if manifest.kind == ProjectKind::Binary && !manifest.binaries.is_empty() {
        let enabled = project::enabled_features(&manifest, features, no_default_features)?;
        let binaries = manifest
            .binaries
            .iter()
            .filter(|target| {
                target
                    .required_features
                    .iter()
                    .all(|feature| enabled.contains(feature))
            })
            .map(|target| Some(target.name.clone()))
            .collect::<Vec<_>>();
        if manifest.binaries.len() > 1 || binaries.len() != manifest.binaries.len() {
            // Every binary was filtered out by unsatisfied required-features:
            // building nothing must not look like success.
            if binaries.is_empty() {
                let missing = manifest
                    .binaries
                    .iter()
                    .flat_map(|target| target.required_features.iter().cloned())
                    .collect::<std::collections::BTreeSet<_>>();
                anyhow::bail!(
                    "no binary target has its required features enabled: {}",
                    missing.into_iter().collect::<Vec<_>>().join(", ")
                );
            }
            return Ok(binaries);
        }
    }
    Ok(vec![None])
}

fn command_features(root: &Path, options: &CommandOptions) -> anyhow::Result<Vec<String>> {
    if !options.all_features {
        return Ok(options.features.clone());
    }
    let manifest = manifest::read(root, ProjectKind::Binary)?;
    Ok(manifest.features.keys().cloned().collect())
}

pub fn build_for_target_with_profile(
    path: &Path,
    explicit_target: Option<TargetTriple>,
    package: Option<&str>,
    workspace_flag: bool,
    profile: BuildProfile,
) -> anyhow::Result<()> {
    let options = CommandOptions {
        profile,
        ..CommandOptions::default()
    };
    build_for_target_with_options(path, explicit_target, package, workspace_flag, &options)
}

/// Builds and runs a package for the host target.
///
/// # Errors
///
/// Returns an error when the package cannot be built or the executable cannot be started.
pub fn run(path: &Path, args: &[OsString]) -> anyhow::Result<ExitStatus> {
    run_for_target(path, args, None)
}

/// Builds and runs a package for an optional explicit target.
///
/// # Errors
///
/// Returns an error when the package cannot be built, cannot run on the host, or cannot start.
pub fn run_for_target(
    path: &Path,
    args: &[OsString],
    explicit_target: Option<TargetTriple>,
) -> anyhow::Result<ExitStatus> {
    run_for_target_with_selection(path, args, explicit_target, None, false)
}

pub fn run_for_target_with_selection(
    path: &Path,
    args: &[OsString],
    explicit_target: Option<TargetTriple>,
    package: Option<&str>,
    workspace_flag: bool,
) -> anyhow::Result<ExitStatus> {
    run_for_target_with_options(
        path,
        args,
        explicit_target,
        package,
        workspace_flag,
        &CommandOptions::default(),
    )
}

pub fn run_for_target_with_options(
    path: &Path,
    args: &[OsString],
    explicit_target: Option<TargetTriple>,
    package: Option<&str>,
    workspace_flag: bool,
    command_options: &CommandOptions,
) -> anyhow::Result<ExitStatus> {
    if let Some((_, members)) = selected_workspace(
        path,
        package,
        workspace_flag,
        command_options,
        command_options.bin.as_deref(),
    )? {
        if members.len() != 1 {
            bail!("workspace run requires `--package` when multiple crates are selected");
        }
        return run_package(&members[0], args, explicit_target, command_options);
    }
    if package.is_some() || workspace_flag {
        bail!("`--package` and `--workspace` require a workspace root");
    }
    let features = command_features(path, command_options)?;
    package::prepare(
        path,
        command_options.locked,
        command_options.offline.then_some(true),
        false,
        &features,
    )?;
    run_package(path, args, explicit_target, command_options)
}

fn run_package(
    path: &Path,
    args: &[OsString],
    explicit_target: Option<TargetTriple>,
    options: &CommandOptions,
) -> anyhow::Result<ExitStatus> {
    let features = command_features(path, options)?;
    let artifact = build::run_with_options(
        path,
        explicit_target,
        options.profile,
        options.bin.as_deref(),
        &features,
        options.no_default_features,
    )?;
    let build::BuildArtifact::Executable {
        path: executable,
        target,
    } = artifact
    else {
        bail!("cannot run a library package");
    };
    if target != TargetTriple::host().map_err(anyhow::Error::msg)? {
        bail!(
            "cannot run target `{target}` on host `{}`; run the built program on the target system",
            TargetTriple::host().map_err(anyhow::Error::msg)?
        );
    }
    Command::new(&executable)
        .args(args)
        .current_dir(path)
        .status()
        .map_err(|error| anyhow::anyhow!("failed to run `{}`: {error}", executable.display()))
}

fn selected_workspace(
    path: &Path,
    package: Option<&str>,
    workspace_flag: bool,
    options: &CommandOptions,
    bin: Option<&str>,
) -> anyhow::Result<Option<(Workspace, Vec<PathBuf>)>> {
    if workspace_flag && package.is_some() {
        bail!("`--workspace` cannot be combined with `--package`");
    }
    let Some(workspace) = workspace::load_for_path(path)? else {
        return Ok(None);
    };
    let mut selected = if let Some(name) = package {
        let package = workspace
            .package_by_name(name)
            .ok_or_else(|| anyhow::anyhow!("package `{name}` is not a workspace crate"))?;
        vec![package.root.clone()]
    } else if workspace_flag || workspace.member_for_path(path).is_none() {
        workspace.ordered_members()
    } else {
        vec![
            workspace
                .member_for_path(path)
                .expect("member path was checked"),
        ]
    };
    if let Some(bin) = bin
        && package.is_none()
        && (workspace_flag || workspace.member_for_path(path).is_none())
    {
        selected.retain(|member| {
            workspace.package(member).is_some_and(|package| {
                package
                    .manifest
                    .binaries
                    .iter()
                    .any(|target| target.name == bin)
            })
        });
        if selected.is_empty() {
            bail!("binary target `{bin}` was not found in the selected workspace");
        }
    }
    if selected.is_empty() {
        bail!("workspace has no registered crates");
    }
    let ordered = workspace
        .ordered_members()
        .into_iter()
        .filter(|member| selected.contains(member))
        .collect::<Vec<_>>();
    let selections = ordered
        .iter()
        .map(|member| Ok((member.clone(), command_features(member, options)?)))
        .collect::<anyhow::Result<Vec<_>>>()?;
    package::prepare_workspace_targets(
        &workspace.root,
        workspace.members(),
        options.locked,
        options.offline.then_some(true),
        false,
        &selections,
    )?;
    Ok(Some((workspace, ordered)))
}

fn parallel_packages<F>(
    members: &[PathBuf],
    jobs: Option<usize>,
    operation: F,
) -> anyhow::Result<()>
where
    F: Fn(&Path) -> anyhow::Result<()> + Sync,
{
    if cancellation_requested() {
        bail!("operation cancelled");
    }
    let jobs = jobs
        .unwrap_or_else(|| std::thread::available_parallelism().map_or(1, usize::from))
        .max(1)
        .min(members.len().max(1));
    if jobs == 1 {
        for member in members {
            operation(member)?;
        }
        return Ok(());
    }
    let errors = std::sync::Mutex::new(Vec::new());
    let cancelled = std::sync::atomic::AtomicBool::new(false);
    std::thread::scope(|scope| {
        for worker in 0..jobs {
            let errors = &errors;
            let cancelled = &cancelled;
            let operation = &operation;
            scope.spawn(move || {
                for member in members.iter().skip(worker).step_by(jobs) {
                    if cancelled.load(std::sync::atomic::Ordering::Acquire)
                        || cancellation_requested()
                    {
                        break;
                    }
                    if let Err(error) = operation(member) {
                        errors.lock().expect("error mutex poisoned").push(error);
                        cancelled.store(true, std::sync::atomic::Ordering::Release);
                        break;
                    }
                }
            });
        }
    });
    if cancellation_requested() {
        bail!("operation cancelled");
    }
    let mut errors = errors.into_inner().expect("error mutex poisoned");
    errors.pop().map_or(Ok(()), Err)
}

fn parallel_workspace_packages<F>(
    workspace: &Workspace,
    members: &[PathBuf],
    jobs: Option<usize>,
    operation: F,
) -> anyhow::Result<()>
where
    F: Fn(&Path) -> anyhow::Result<()> + Sync,
{
    let jobs = match jobs {
        Some(jobs) => Some(jobs),
        None => Some(package::configured_jobs(&workspace.root)?),
    };
    for batch in workspace.ordered_batches(members) {
        parallel_packages(&batch, jobs, &operation)?;
    }
    Ok(())
}

/// Generates HTML API documentation for the current package (`clue doc`).
///
/// # Errors
///
/// Returns an error when the project cannot be loaded or fails to check.
pub fn generate_doc(
    path: &Path,
    package: Option<&str>,
    open: bool,
    document_private_items: bool,
    no_std: bool,
) -> anyhow::Result<()> {
    doc::generate(path, package, document_private_items, open, no_std).map(|_| ())
}
