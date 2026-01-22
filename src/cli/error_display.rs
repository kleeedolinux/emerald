use crate::error::{Diagnostic, DiagnosticKind, Reporter, Severity};
use codespan_reporting::diagnostic::{Diagnostic as CodespanDiagnostic, Label, Severity as CodespanSeverity};
use codespan_reporting::term::termcolor::{ColorChoice, StandardStream};
use codespan_reporting::term::{self, Config};

use codespan::FileId;

/// convert emerald diagnostic 2 codespan rprtng dgnstc
pub fn convert_diagnostic(diag: &Diagnostic) -> CodespanDiagnostic<FileId> {
    let severity = match diag.severity {
        Severity::Error => CodespanSeverity::Error,
        Severity::Warning => CodespanSeverity::Warning,
        Severity::Note => CodespanSeverity::Note,
    };

    let code = match &diag.kind {
        DiagnosticKind::LexicalError => Some("E0001"),
        DiagnosticKind::SyntaxError => Some("E0002"),
        DiagnosticKind::TypeError => Some("E0003"),
        DiagnosticKind::NameResolutionError => Some("E0004"),
        DiagnosticKind::BorrowCheckerError => Some("E0005"),
        DiagnosticKind::SemanticError => Some("E0006"),
    };

    let mut codespan_diag = CodespanDiagnostic::new(severity)
        .with_code(code.unwrap_or("E0000"))
        .with_message(&diag.message)
        .with_labels(vec![Label::primary(
            diag.file_id,
            usize::from(diag.span.start())..usize::from(diag.span.end()),
        )
        .with_message(get_label_message(&diag.kind))]);

    // add notes if prsnt
    if !diag.notes.is_empty() {
        codespan_diag = codespan_diag.with_notes(diag.notes.clone());
    }

    codespan_diag
}

fn get_label_message(kind: &DiagnosticKind) -> String {
    match kind {
        DiagnosticKind::LexicalError => "lexical error occurred here",
        DiagnosticKind::SyntaxError => "syntax error occurred here",
        DiagnosticKind::TypeError => "type error occurred here",
        DiagnosticKind::NameResolutionError => "name resolution error occurred here",
        DiagnosticKind::BorrowCheckerError => "borrow checker error occurred here",
        DiagnosticKind::SemanticError => "semantic error occurred here",
    }
    .to_string()
}

/// display all dgnstcs from a reporter
pub fn display_diagnostics(reporter: &Reporter, color_choice: ColorChoice) {
    let files = reporter.files();
    let diagnostics = reporter.diagnostics();

    if diagnostics.is_empty() {
        return;
    }

    let writer = StandardStream::stderr(color_choice);
    let config = Config::default();

    for diag in diagnostics {
        let codespan_diag = convert_diagnostic(diag);
        term::emit(&mut writer.lock(), &config, files, &codespan_diag)
            .expect("Failed to emit diagnostic");
    }
}

/// count errors and wrnngs in diagnostics
pub fn count_diagnostics(reporter: &Reporter) -> (usize, usize) {
    let mut errors = 0;
    let mut warnings = 0;

    for diag in reporter.diagnostics() {
        match diag.severity {
            Severity::Error => errors += 1,
            Severity::Warning => warnings += 1,
            Severity::Note => {}
        }
    }

    (errors, warnings)
}
