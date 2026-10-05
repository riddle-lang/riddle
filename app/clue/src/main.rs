use clap::{Args, Parser, Subcommand, ValueEnum};
use clue::{
    BuildProfile, CommandOptions, DependencyAddOptions, InstallOptions, ProjectKind, TargetTriple,
    add_dependency, bench_targets_with_selection, build_examples_with_selection,
    build_for_target_with_options, check_for_target_with_options, clean, dependency_tree,
    dependency_tree_with_features, fetch_packages, init, init_workspace, install_package, new,
    new_workspace, package_archive, package_contents, package_metadata, publish_package,
    publish_package_dry_run, remove_dependency, run_example_with_selection,
    run_for_target_with_options, test_targets_with_selection, uninstall_package, update_packages,
};
use std::ffi::OsString;
use std::num::NonZeroUsize;
use std::path::PathBuf;

#[derive(Parser)]
#[command(name = "clue", version, about = "Riddle's package manager and builder")]
struct Cli {
    #[arg(long, global = true)]
    offline: bool,
    #[arg(short = 'j', long, global = true, value_name = "N")]
    jobs: Option<NonZeroUsize>,
    #[command(subcommand)]
    commands: Commands,
}

#[derive(Clone, Copy, ValueEnum)]
enum TreeEdges {
    Normal,
    Features,
}

#[derive(Subcommand)]
enum Commands {
    Init(ProjectArgs),
    New(ProjectArgs),
    Check {
        path: Option<PathBuf>,
        #[arg(short = 'p', long)]
        package: Option<String>,
        #[arg(long, conflicts_with = "package")]
        workspace: bool,
        #[arg(long, value_name = "TRIPLE")]
        target: Option<TargetTriple>,
        #[arg(long, value_name = "NAME")]
        bin: Option<String>,
        #[arg(long)]
        locked: bool,
        #[arg(long = "all-features")]
        all_features: bool,
        #[arg(long = "all-targets")]
        all_targets: bool,
        #[arg(long = "no-default-features")]
        no_default_features: bool,
        #[arg(long = "features", value_delimiter = ',')]
        features: Vec<String>,
    },
    Build {
        path: Option<PathBuf>,
        #[arg(short = 'p', long)]
        package: Option<String>,
        #[arg(long, conflicts_with = "package")]
        workspace: bool,
        #[arg(long, value_name = "TRIPLE")]
        target: Option<TargetTriple>,
        #[arg(long, value_name = "NAME", conflicts_with = "example")]
        bin: Option<String>,
        #[arg(long, value_name = "NAME", conflicts_with = "bin")]
        example: Option<String>,
        #[arg(long)]
        release: bool,
        #[arg(long)]
        locked: bool,
        #[arg(long = "all-features")]
        all_features: bool,
        #[arg(long = "all-targets")]
        all_targets: bool,
        #[arg(long = "no-default-features")]
        no_default_features: bool,
        #[arg(long = "features", value_delimiter = ',')]
        features: Vec<String>,
    },
    Run {
        path: Option<PathBuf>,
        #[arg(short = 'p', long)]
        package: Option<String>,
        #[arg(long, value_name = "TRIPLE")]
        target: Option<TargetTriple>,
        #[arg(long, value_name = "NAME", conflicts_with = "example")]
        bin: Option<String>,
        #[arg(long, value_name = "NAME", conflicts_with = "bin")]
        example: Option<String>,
        #[arg(long)]
        release: bool,
        #[arg(long)]
        locked: bool,
        #[arg(long = "all-features")]
        all_features: bool,
        #[arg(long = "no-default-features")]
        no_default_features: bool,
        #[arg(long = "features", value_delimiter = ',')]
        features: Vec<String>,
        #[arg(last = true)]
        args: Vec<OsString>,
    },
    Add {
        name: String,
        #[arg(long)]
        version: Option<String>,
        #[arg(long, conflicts_with_all = ["git", "registry"])]
        path: Option<PathBuf>,
        #[arg(long, conflicts_with_all = ["path", "registry"])]
        git: Option<String>,
        #[arg(long, requires = "git", conflicts_with_all = ["tag", "rev"])]
        branch: Option<String>,
        #[arg(long, requires = "git", conflicts_with_all = ["branch", "rev"])]
        tag: Option<String>,
        #[arg(long, requires = "git", conflicts_with_all = ["branch", "tag"])]
        rev: Option<String>,
        #[arg(long, conflicts_with_all = ["path", "git"])]
        registry: Option<String>,
        #[arg(long)]
        package: Option<String>,
        #[arg(long, value_delimiter = ',')]
        features: Vec<String>,
        #[arg(long = "no-default-features")]
        no_default_features: bool,
        #[arg(long)]
        optional: bool,
        #[arg(long)]
        dev: bool,
        #[arg(long, default_value = ".")]
        project: PathBuf,
    },
    Remove {
        name: String,
        #[arg(long)]
        dev: bool,
        #[arg(long, default_value = ".")]
        project: PathBuf,
    },
    Fetch {
        path: Option<PathBuf>,
        #[arg(long)]
        locked: bool,
        #[arg(long)]
        dev: bool,
        #[arg(long, value_delimiter = ',')]
        features: Vec<String>,
    },
    Update {
        path: Option<PathBuf>,
    },
    Tree {
        path: Option<PathBuf>,
        #[arg(short = 'e', long, value_enum, default_value = "normal")]
        edges: TreeEdges,
    },
    Metadata {
        path: Option<PathBuf>,
    },
    Package {
        path: Option<PathBuf>,
        #[arg(long)]
        list: bool,
    },
    Publish {
        path: Option<PathBuf>,
        #[arg(long)]
        registry: Option<String>,
        #[arg(long)]
        dry_run: bool,
    },
    Install {
        package: Option<String>,
        #[arg(long, conflicts_with_all = ["package", "git"])]
        path: Option<PathBuf>,
        #[arg(long, conflicts_with_all = ["package", "path", "registry"])]
        git: Option<String>,
        #[arg(long, requires = "git", conflicts_with_all = ["tag", "rev"])]
        branch: Option<String>,
        #[arg(long, requires = "git", conflicts_with_all = ["branch", "rev"])]
        tag: Option<String>,
        #[arg(long, requires = "git", conflicts_with_all = ["branch", "tag"])]
        rev: Option<String>,
        #[arg(long)]
        version: Option<String>,
        #[arg(long, requires = "package")]
        registry: Option<String>,
        #[arg(long)]
        release: bool,
    },
    Uninstall {
        name: String,
        #[arg(long, default_value = ".")]
        path: PathBuf,
    },
    Clean {
        path: Option<PathBuf>,
    },
    /// Generate HTML API documentation.
    Doc {
        path: Option<PathBuf>,
        #[arg(short = 'p', long)]
        package: Option<String>,
        #[arg(long)]
        open: bool,
        /// Document private items too.
        #[arg(long = "document-private-items")]
        document_private_items: bool,
        /// Compile without the bundled standard library (for documenting
        /// `std` itself).
        #[arg(long = "no-std")]
        no_std: bool,
    },
    Test {
        path: Option<PathBuf>,
        #[arg(short = 'p', long)]
        package: Option<String>,
        #[arg(long, conflicts_with = "package")]
        workspace: bool,
        #[arg(long, value_name = "NAME")]
        test: Option<String>,
        #[arg(long)]
        no_run: bool,
        #[arg(long)]
        release: bool,
        #[arg(long)]
        locked: bool,
        #[arg(long = "all-features")]
        all_features: bool,
        #[arg(long = "no-default-features")]
        no_default_features: bool,
        #[arg(long, value_delimiter = ',')]
        features: Vec<String>,
    },
    Bench {
        path: Option<PathBuf>,
        #[arg(short = 'p', long)]
        package: Option<String>,
        #[arg(long, conflicts_with = "package")]
        workspace: bool,
        #[arg(long, value_name = "NAME")]
        bench: Option<String>,
        #[arg(long)]
        locked: bool,
        #[arg(long = "all-features")]
        all_features: bool,
        #[arg(long = "no-default-features")]
        no_default_features: bool,
        #[arg(long, value_delimiter = ',')]
        features: Vec<String>,
        #[arg(last = true)]
        args: Vec<OsString>,
    },
}

#[derive(Args)]
struct ProjectArgs {
    path: PathBuf,

    #[arg(long, conflicts_with = "bin")]
    lib: bool,

    #[arg(long, conflicts_with = "lib")]
    bin: bool,

    #[arg(long, conflicts_with_all = ["lib", "bin"])]
    workspace: bool,
}

impl ProjectArgs {
    const fn kind(&self) -> ProjectKind {
        if self.lib {
            ProjectKind::Library
        } else {
            ProjectKind::Binary
        }
    }
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    #[cfg(not(target_arch = "wasm32"))]
    ctrlc::set_handler(clue::request_cancellation)
        .map_err(|error| format!("failed to install Ctrl+C handler: {error}"))?;
    let cli = Cli::parse();
    let offline = cli.offline;
    let jobs = cli.jobs.map(NonZeroUsize::get);
    match cli.commands {
        Commands::Init(args) => {
            if args.workspace {
                init_workspace(&args.path)?;
            } else {
                init(&args.path, args.kind())?;
            }
            println!("clue: initialized {}", args.path.display());
        }
        Commands::New(args) => {
            if args.workspace {
                new_workspace(&args.path)?;
            } else {
                new(&args.path, args.kind())?;
            }
            println!("clue: created {}", args.path.display());
        }
        Commands::Check {
            path,
            package,
            workspace,
            target,
            bin,
            locked,
            all_features,
            all_targets,
            no_default_features,
            features,
        } => {
            check_for_target_with_options(
                path.as_deref().unwrap_or_else(|| std::path::Path::new(".")),
                target,
                package.as_deref(),
                workspace,
                &CommandOptions {
                    bin,
                    locked,
                    features,
                    all_features,
                    all_targets,
                    no_default_features,
                    offline,
                    jobs,
                    ..CommandOptions::default()
                },
            )?;
        }
        Commands::Build {
            path,
            package,
            workspace,
            target,
            bin,
            example,
            release,
            locked,
            all_features,
            all_targets,
            no_default_features,
            features,
        } => {
            let root = path.as_deref().unwrap_or_else(|| std::path::Path::new("."));
            let options = CommandOptions {
                profile: if release {
                    BuildProfile::Release
                } else {
                    BuildProfile::Debug
                },
                bin,
                locked,
                features,
                all_features,
                all_targets,
                no_default_features,
                offline,
                jobs,
            };
            if example.is_some() {
                build_examples_with_selection(
                    root,
                    example.as_deref(),
                    package.as_deref(),
                    workspace,
                    &options,
                )?;
            } else {
                build_for_target_with_options(
                    root,
                    target,
                    package.as_deref(),
                    workspace,
                    &options,
                )?;
            }
        }
        Commands::Run {
            path,
            package,
            target,
            bin,
            example,
            release,
            locked,
            all_features,
            no_default_features,
            features,
            args,
        } => {
            let root = path.as_deref().unwrap_or_else(|| std::path::Path::new("."));
            let options = CommandOptions {
                profile: if release {
                    BuildProfile::Release
                } else {
                    BuildProfile::Debug
                },
                bin,
                locked,
                features,
                all_features,
                all_targets: false,
                no_default_features,
                offline,
                jobs,
            };
            let status = if example.is_some() {
                run_example_with_selection(
                    root,
                    example.as_deref(),
                    &args,
                    package.as_deref(),
                    &options,
                )?
            } else {
                run_for_target_with_options(
                    root,
                    &args,
                    target,
                    package.as_deref(),
                    false,
                    &options,
                )?
            };
            std::process::exit(status.code().unwrap_or(1));
        }
        Commands::Add {
            name,
            version,
            path,
            git,
            branch,
            tag,
            rev,
            registry,
            package,
            features,
            no_default_features,
            optional,
            dev,
            project,
        } => {
            add_dependency(
                &project,
                &DependencyAddOptions {
                    name: name.clone(),
                    version,
                    path,
                    git,
                    branch,
                    tag,
                    rev,
                    registry,
                    package,
                    features,
                    default_features: !no_default_features,
                    optional,
                    dev,
                },
            )?;
            println!("clue: added {name}");
        }
        Commands::Remove { name, dev, project } => {
            remove_dependency(&project, &name, dev)?;
            println!("clue: removed {name}");
        }
        Commands::Fetch {
            path,
            locked,
            dev,
            features,
        } => {
            let path = path.as_deref().unwrap_or_else(|| std::path::Path::new("."));
            fetch_packages(path, locked, offline, dev, &features)?;
            println!("clue: fetched dependencies");
        }
        Commands::Update { path } => {
            let path = path.as_deref().unwrap_or_else(|| std::path::Path::new("."));
            update_packages(path, offline)?;
            println!("clue: updated dependencies");
        }
        Commands::Tree { path, edges } => {
            let path = path.as_deref().unwrap_or_else(|| std::path::Path::new("."));
            let tree = match edges {
                TreeEdges::Normal => dependency_tree(path)?,
                TreeEdges::Features => dependency_tree_with_features(path, true)?,
            };
            print!("{tree}");
        }
        Commands::Metadata { path } => {
            let path = path.as_deref().unwrap_or_else(|| std::path::Path::new("."));
            println!("{}", package_metadata(path)?);
        }
        Commands::Package { path, list } => {
            let path = path.as_deref().unwrap_or_else(|| std::path::Path::new("."));
            if list {
                for path in package_contents(path)? {
                    println!("{}", path.display());
                }
            } else {
                println!("clue: packaged {}", package_archive(path)?.display());
            }
        }
        Commands::Publish {
            path,
            registry,
            dry_run,
        } => {
            let path = path.as_deref().unwrap_or_else(|| std::path::Path::new("."));
            let archive = if dry_run {
                publish_package_dry_run(path, registry.as_deref())?
            } else {
                publish_package(path, registry.as_deref())?
            };
            println!(
                "clue: {} {}",
                if dry_run {
                    "publish dry run"
                } else {
                    "published"
                },
                archive.display()
            );
        }
        Commands::Install {
            package,
            path,
            git,
            branch,
            tag,
            rev,
            version,
            registry,
            release,
        } => {
            println!(
                "clue: installed {}",
                install_package(
                    std::path::Path::new("."),
                    &InstallOptions {
                        package,
                        path,
                        git,
                        branch,
                        tag,
                        rev,
                        version,
                        registry,
                        release,
                        offline,
                    }
                )?
                .display()
            );
        }
        Commands::Uninstall { name, path } => {
            println!(
                "clue: removed {}",
                uninstall_package(&path, &name)?.display()
            );
        }
        Commands::Clean { path } => {
            let path = path.as_deref().unwrap_or_else(|| std::path::Path::new("."));
            clean(path)?;
            println!("clue: cleaned {}", path.display());
        }
        Commands::Doc {
            path,
            package,
            open,
            document_private_items,
            no_std,
        } => {
            let path = path.as_deref().unwrap_or_else(|| std::path::Path::new("."));
            clue::generate_doc(
                path,
                package.as_deref(),
                open,
                document_private_items,
                no_std,
            )?;
        }
        Commands::Test {
            path,
            package,
            workspace,
            test,
            no_run,
            release,
            locked,
            all_features,
            no_default_features,
            features,
        } => {
            let path = path.as_deref().unwrap_or_else(|| std::path::Path::new("."));
            test_targets_with_selection(
                path,
                test.as_deref(),
                no_run,
                package.as_deref(),
                workspace,
                &CommandOptions {
                    profile: if release {
                        BuildProfile::Release
                    } else {
                        BuildProfile::Debug
                    },
                    locked,
                    features,
                    all_features,
                    no_default_features,
                    offline,
                    jobs,
                    ..CommandOptions::default()
                },
            )?;
        }
        Commands::Bench {
            path,
            package,
            workspace,
            bench,
            locked,
            all_features,
            no_default_features,
            features,
            args,
        } => {
            let path = path.as_deref().unwrap_or_else(|| std::path::Path::new("."));
            bench_targets_with_selection(
                path,
                bench.as_deref(),
                &args,
                package.as_deref(),
                workspace,
                &CommandOptions {
                    profile: BuildProfile::Release,
                    locked,
                    features,
                    all_features,
                    no_default_features,
                    offline,
                    jobs,
                    ..CommandOptions::default()
                },
            )?;
        }
    }

    Ok(())
}
