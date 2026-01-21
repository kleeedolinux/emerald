use codespan::{FileId, Span};
use thiserror::Error;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Severity {
    Error,
    Warning,
    Note,
}

#[derive(Debug, Clone)]
pub struct Diagnostic {
    pub severity: Severity,
    pub kind: DiagnosticKind,
    pub span: Span,
    pub file_id: FileId,
    pub message: String,
    pub notes: Vec<String>,
}

#[derive(Debug, Clone, Error)]
pub enum DiagnosticKind {
    #[error("lexical error")]
    LexicalError,
    #[error("syntax error")]
    SyntaxError,
    #[error("type error")]
    TypeError,
    #[error("name resolution error")]
    NameResolutionError,
    #[error("borrow checker error")]
    BorrowCheckerError,
    #[error("semantic error")]
    SemanticError,
}

impl Diagnostic {
    pub fn new(
        severity: Severity,
        kind: DiagnosticKind,
        span: Span,
        file_id: FileId,
        message: String,
    ) -> Self {
        Self {
            severity,
            kind,
            span,
            file_id,
            message,
            notes: Vec::new(),
        }
    }

    pub fn with_note(mut self, note: String) -> Self {
        self.notes.push(note);
        self
    }

    pub fn error(kind: DiagnosticKind, span: Span, file_id: FileId, message: String) -> Self {
        Self::new(Severity::Error, kind, span, file_id, message)
    }

    pub fn warning(kind: DiagnosticKind, span: Span, file_id: FileId, message: String) -> Self {
        Self::new(Severity::Warning, kind, span, file_id, message)
    }
}
