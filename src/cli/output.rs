use owo_colors::OwoColorize;

/// clr coded output utilities
pub struct Output;

impl Output {
    /// print an err msg in red
    pub fn error(msg: &str) {
        eprintln!("{} {}", "error:".red().bold(), msg);
    }

    /// print a warning msg in yellow
    pub fn warning(msg: &str) {
        eprintln!("{} {}", "warning:".yellow().bold(), msg);
    }

    /// prnt an info msg in blue
    pub fn info(msg: &str) {
        println!("{} {}", "info:".blue().bold(), msg);
    }

    /// prnt a sccss msg in green
    pub fn success(msg: &str) {
        println!("{} {}", "success:".green().bold(), msg);
    }

    /// prnt a note msg
    pub fn note(msg: &str) {
        println!("{} {}", "note:".cyan(), msg);
    }

    /// print a ehlp msg
    pub fn help(msg: &str) {
        println!("{} {}", "help:".bright_blue(), msg);
    }

    /// prnt cmpltn phs
    pub fn phase(phase: &str) {
        println!("{} {}", "→".bright_blue(), phase.bright_white());
    }

    /// print file being prcssd
    pub fn processing_file(file: &str) {
        println!("{} {}", "Processing:".bright_cyan(), file.bright_white());
    }

    /// print summary statistics
    pub fn summary(stats: &CompileStats) {
        println!("\n{}", "Compilation Summary".bold().underline());
        println!("  {} {}", "Files compiled:".bright_white(), stats.files_compiled.to_string().bright_green());
        println!("  {} {}", "Errors:".bright_white(), stats.errors.to_string().red());
        println!("  {} {}", "Warnings:".bright_white(), stats.warnings.to_string().yellow());
        if let Some(time) = stats.time_taken {
            println!("  {} {}ms", "Time:".bright_white(), time.to_string().bright_green());
        }
    }

    /// print build success msg
    pub fn build_success(output: &str) {
        println!("\n{} {}", "✓".green().bold(), "Build successful!".green().bold());
        println!("  {}", format!("Output: {}", output).bright_white());
    }

    /// print build flr msg
    pub fn build_failure() {
        println!("\n{} {}", "✗".red().bold(), "Build failed!".red().bold());
    }
}

#[derive(Debug, Default)]
pub struct CompileStats {
    pub files_compiled: usize,
    pub errors: usize,
    pub warnings: usize,
    pub time_taken: Option<u64>,
}
