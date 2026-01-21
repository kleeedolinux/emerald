use crate::error::Diagnostic;
use codespan::{FileId, Files};

pub struct Reporter {
    files: Files<String>,
    diagnostics: Vec<Diagnostic>,
}

impl Reporter {
    pub fn new() -> Self {
        Self {
            files: Files::new(),
            diagnostics: Vec::new(),
        }
    }

    pub fn add_file(&mut self, name: String, source: String) -> FileId {
        self.files.add(name, source)
    }

    pub fn add_diagnostic(&mut self, diagnostic: Diagnostic) {
        self.diagnostics.push(diagnostic);
    }

    pub fn has_errors(&self) -> bool {
        self.diagnostics
            .iter()
            .any(|d| matches!(d.severity, crate::error::Severity::Error))
    }

    pub fn diagnostics(&self) -> &[Diagnostic] {
        &self.diagnostics
    }

    pub fn files(&self) -> &Files<String> {
        &self.files
    }

    pub fn files_mut(&mut self) -> &mut Files<String> {
        &mut self.files
    }
}

impl Default for Reporter {
    fn default() -> Self {
        Self::new()
    }
}
