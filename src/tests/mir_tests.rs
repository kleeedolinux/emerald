use crate::error::Reporter;
use crate::frontend::lexer::Lexer;
use crate::frontend::parser::Parser;
use crate::frontend::semantic::SemanticAnalyzer;
use crate::middle::{HirLowerer, MirLowerer};
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

fn lower_to_mir(source: &str) -> (Vec<crate::core::mir::MirFunction>, Reporter) {
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
    
    let mut hir_lowerer = HirLowerer::new(symbol_table);
    let hir = hir_lowerer.lower(&ast);
    
    let mut mir_lowerer = MirLowerer::new();
    let mir_functions = mir_lowerer.lower(&hir);
    
    if reporter.has_errors() {
        print_diagnostics(&reporter, &files);
    }
    
    (mir_functions, reporter)
}

#[test]
fn test_mir_lowering() {
    let source = r#"
def add(a : int, b : int) returns int
  return a + b
end
"#;
    let (mir_funcs, reporter) = lower_to_mir(source);
    assert!(!reporter.has_errors());
    assert_eq!(mir_funcs.len(), 1);
}

#[test]
fn test_mir_ssa_form() {
    let source = r#"
def test
  x = 10
  y = x + 5
  z = y * 2
end
"#;
    let (mir_funcs, reporter) = lower_to_mir(source);
    assert!(!reporter.has_errors());
    assert!(!mir_funcs.is_empty());
}

#[test]
fn test_mir_control_flow() {
    let source = r#"
def test
  if true
    x = 10
  else
    x = 20
  end
end
"#;
    let (mir_funcs, reporter) = lower_to_mir(source);
    assert!(!reporter.has_errors());
    if let Some(func) = mir_funcs.first() {
        assert!(func.basic_blocks.len() > 1); // shuold have multiple blocks
    }
}
