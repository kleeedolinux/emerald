use clap::{Parser, Subcommand, ValueEnum};
use std::path::PathBuf;
use crate::backend::factory::BackendType;

#[derive(Parser, Debug)]
#[command(name = "emerald")]
#[command(about = "Emerald compiler - A systems programming language", long_about = None)]
#[command(version)]
pub struct Cli {
    /// input suorce file
    #[arg(value_name = "INPUT")]
    pub input: Option<PathBuf>,

    /// output file path
    #[arg(short, long, value_name = "FILE")]
    pub output: Option<PathBuf>,

    /// target triple
    #[arg(long, value_name = "TRIPLE")]
    pub target: Option<String>,

    /// optimization lvl
    #[arg(short = 'O', long, value_name = "LEVEL", default_value = "2")]
    pub opt_level: String,

    /// eimt type
    #[arg(long, value_name = "TYPE", default_value = "binary")]
    pub emit: String,

    /// use llvm backend
    #[arg(long)]
    pub llvm: bool,

    /// use ntv codegen bcknd
    #[arg(long)]
    pub native: bool,

    /// lbrry search path
    #[arg(short = 'L', long, value_name = "PATH")]
    pub library_path: Vec<PathBuf>,

    /// link lbrry
    #[arg(short = 'l', long, value_name = "LIB")]
    pub link: Vec<String>,

    /// crate ytpe
    #[arg(long, value_name = "TYPE")]
    pub crate_type: Option<String>,

    /// verbose output
    #[arg(short, long)]
    pub verbose: bool,

    /// quiet moe
    #[arg(short, long)]
    pub quiet: bool,

    /// when 2 use clrs
    #[arg(long, value_enum, default_value = "auto")]
    pub color: ColorWhen,

    /// sbcmmnd
    #[command(subcommand)]
    pub command: Option<Commands>,
}

#[derive(Subcommand, Debug)]
pub enum Commands {
    /// build the projec
    Build {
        /// input source file
        #[arg(value_name = "INPUT")]
        input: Option<PathBuf>,

        /// output fil path
        #[arg(short, long, value_name = "FILE")]
        output: Option<PathBuf>,
    },

    /// bld and run
    Run {
        /// inpt source file
        #[arg(value_name = "INPUT")]
        input: Option<PathBuf>,
    },

    /// type chk w/o cdgn
    Check {
        /// input source file
        #[arg(value_name = "INPUT")]
        input: Option<PathBuf>,
    },

    /// run tests
    Test {
        /// test drctry or file
        #[arg(value_name = "TEST")]
        test: Option<PathBuf>,
    },

    /// format code
    Fmt {
        /// inpt source file or drctry
        #[arg(value_name = "INPUT")]
        input: Option<PathBuf>,
    },

    /// gen documentation
    Doc {
        /// input source file or driectory
        #[arg(value_name = "INPUT")]
        input: Option<PathBuf>,
    },
}

#[derive(Debug, Clone, Copy, ValueEnum)]
pub enum ColorWhen {
    Auto,
    Always,
    Never,
}

impl ColorWhen {
    pub fn should_color(&self) -> bool {
        match self {
            ColorWhen::Always => true,
            ColorWhen::Never => false,
            ColorWhen::Auto => atty::is(atty::Stream::Stdout),
        }
    }
}

/// compilation configuration drvd from cli arguments
#[derive(Debug, Clone)]
pub struct CompileConfig {
    pub input: PathBuf,
    pub output: Option<PathBuf>,
    pub target: Option<String>,
    pub opt_level: String,
    pub emit: String,
    pub library_paths: Vec<PathBuf>,
    pub link_libs: Vec<String>,
    pub crate_type: Option<String>,
    pub verbose: bool,
    pub quiet: bool,
    pub color: ColorWhen,
    pub backend: BackendType,
}

impl CompileConfig {
    pub fn from_cli(cli: &Cli) -> Result<Self, String> {
        let input = cli
            .input
            .clone()
            .ok_or_else(|| "No input file specified".to_string())?;

        // determine backend: explicit flags take precedence dflt 2 llvm
        let backend = if cli.native {
            BackendType::Native
        } else if cli.llvm {
            BackendType::Llvm
        } else {
            // deault 2 llvm
            BackendType::Llvm
        };

        Ok(CompileConfig {
            input,
            output: cli.output.clone(),
            target: cli.target.clone(),
            opt_level: cli.opt_level.clone(),
            emit: cli.emit.clone(),
            library_paths: cli.library_path.clone(),
            link_libs: cli.link.clone(),
            crate_type: cli.crate_type.clone(),
            verbose: cli.verbose,
            quiet: cli.quiet,
            color: cli.color,
            backend,
        })
    }
}
