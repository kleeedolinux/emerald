use crate::error::Reporter;
use crate::frontend::lexer::Lexer;
use crate::frontend::parser::Parser;
use crate::frontend::semantic::SemanticAnalyzer;
use crate::middle::HirLowerer;
use codespan::Files;

fn print_diagnostics(reporter: &Reporter, files: &Files<String>) {
    for diag in reporter.diagnostics() {
        eprintln!("[{:?}] {:?}: {}", diag.kind, diag.severity, diag.message);
        let source = files.source(diag.file_id);
        let start = diag.span.start().to_usize();
        let end = diag.span.end().to_usize();
        if start < source.len() && end <= source.len() {
            let snippet = &source[start..end];
            eprintln!("  at: {:?}", snippet);
        }
        for note in &diag.notes {
            eprintln!("  note: {}", note);
        }
    }
}

fn lower_to_hir(source: &str) -> (crate::core::hir::Hir, Reporter) {
    let mut files = Files::new();
    let file_id = files.add("test.em", source.to_string());
    let mut reporter = Reporter::new();
    let source_str = files.source(file_id).to_string();
    let mut lexer = Lexer::new(&source_str, file_id, &mut reporter);
    let tokens = lexer.tokenize();
    let mut parser = Parser::new(tokens, file_id, &mut reporter);
    let ast = parser.parse();
    
    let symbol_table = if !reporter.has_errors() {
        let mut analyzer = SemanticAnalyzer::new(&mut reporter, file_id);
        analyzer.analyze(&ast)
    } else {
        crate::frontend::semantic::symbol_table::SymbolTable::new()
    };
    
    let mut lowerer = HirLowerer::new(symbol_table);
    let hir = lowerer.lower(&ast);
    
    if reporter.has_errors() {
        print_diagnostics(&reporter, &files);
    }
    
    (hir, reporter)
}

#[test]
fn test_hir_lowering() {
    let source = r#"
def add(a : int, b : int) returns int
  return a + b
end
"#;
    let (hir, reporter) = lower_to_hir(source);
    assert!(!reporter.has_errors());
    assert_eq!(hir.items.len(), 1);
}

#[test]
fn test_hir_preserves_types() {
    let source = r#"
def test
  x : int = 42
  y : float = 3.14
end
"#;
    let (_hir, reporter) = lower_to_hir(source);
    assert!(!reporter.has_errors());
}

#[test]
fn test_hir_binary_operations() {
    let source = r#"
def test
  a = 10 + 20
  b = 5 * 3
  c = 10 == 20
end
"#;
    let (_hir, reporter) = lower_to_hir(source);
    assert!(!reporter.has_errors());
}
