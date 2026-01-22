use clap::Parser;
use emc::cli::args::{Cli, Commands};
use emc::cli::compiler::{display_results, Compiler};
use emc::cli::output::Output;
use emc::backend::factory::BackendType;
use std::process;

fn main() {
    let cli = Cli::parse();

    // handle subcommands
    if let Some(command) = &cli.command {
        match command {
            Commands::Build { input, output } => {
                handle_build(input.as_ref().or(cli.input.as_ref()), output.as_ref().or(cli.output.as_ref()));
            }
            Commands::Run { input } => {
                handle_run(input.as_ref().or(cli.input.as_ref()));
            }
            Commands::Check { input } => {
                handle_check(input.as_ref().or(cli.input.as_ref()));
            }
            Commands::Test { test: _ } => {
                Output::info("Test command not yet implemented");
                process::exit(1);
            }
            Commands::Fmt { input: _ } => {
                Output::info("Format command not yet implemented");
                process::exit(1);
            }
            Commands::Doc { input: _ } => {
                Output::info("Documentation generation not yet implemented");
                process::exit(1);
            }
        }
        return;
    }

    // default: compile the input file
    match emc::cli::args::CompileConfig::from_cli(&cli) {
        Ok(config) => {
            let mut compiler = Compiler::new(config.clone());
            match compiler.compile() {
                Ok(result) => {
                    display_results(&result, &config);
                    if !result.success {
                        process::exit(1);
                    }
                }
                Err(e) => {
                    Output::error(&format!("Compilation failed: {}", e));
                    process::exit(1);
                }
            }
        }
        Err(e) => {
            Output::error(&e);
            process::exit(1);
        }
    }
}

fn handle_build(input: Option<&std::path::PathBuf>, output: Option<&std::path::PathBuf>) {
    let input = match input {
        Some(i) => i.clone(),
        None => {
            Output::error("No input file specified for build command");
            process::exit(1);
        }
    };

    let config = emc::cli::args::CompileConfig {
        input,
        output: output.cloned(),
        target: None,
        opt_level: "2".to_string(),
        emit: "binary".to_string(),
        library_paths: vec![],
        link_libs: vec![],
        crate_type: None,
        verbose: false,
        quiet: false,
        color: emc::cli::args::ColorWhen::Auto,
        backend: BackendType::Llvm, // default 2 llvm
    };

    let mut compiler = Compiler::new(config.clone());
    match compiler.compile() {
        Ok(result) => {
            display_results(&result, &config);
            if !result.success {
                process::exit(1);
            }
        }
        Err(e) => {
            Output::error(&format!("Build failed: {}", e));
            process::exit(1);
        }
    }
}

fn handle_run(input: Option<&std::path::PathBuf>) {
    Output::info("Run command not yet implemented (backend codegen required)");
    if let Some(input) = input {
        Output::info(&format!("Would run: {}", input.display()));
    }
    process::exit(1);
}

fn handle_check(input: Option<&std::path::PathBuf>) {
    let input = match input {
        Some(i) => i.clone(),
        None => {
            Output::error("No input file specified for check command");
            process::exit(1);
        }
    };

    let config = emc::cli::args::CompileConfig {
        input,
        output: None,
        target: None,
        opt_level: "0".to_string(),
        emit: "binary".to_string(),
        library_paths: vec![],
        link_libs: vec![],
        crate_type: None,
        verbose: false,
        quiet: false,
        color: emc::cli::args::ColorWhen::Auto,
        backend: BackendType::Llvm, // dflt 2 llvm
    };

    let mut compiler = Compiler::new(config.clone());
    match compiler.compile() {
        Ok(result) => {
            display_results(&result, &config);
            if !result.success {
                process::exit(1);
            } else {
                Output::success("Type checking passed!");
            }
        }
        Err(e) => {
            Output::error(&format!("Type checking failed: {}", e));
            process::exit(1);
        }
    }
}
