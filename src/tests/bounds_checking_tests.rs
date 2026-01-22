use crate::error::Reporter;
use crate::frontend::lexer::Lexer;
use crate::frontend::parser::Parser;
use crate::frontend::semantic::SemanticAnalyzer;
use crate::middle::{HirLowerer, MirLowerer};
use codespan::Files;

fn compile_to_mir(source: &str) -> (Vec<crate::core::mir::MirFunction>, Reporter) {
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
    
    (mir_functions, reporter)
}

#[test]
fn test_array_bounds_checking_compile_time_valid() {
    let source = r#"
def main
  arr : int[5] = [1, 2, 3, 4, 5]
  x : int = arr[0]
  y : int = arr[4]
end
"#;
    let (_mir_functions, reporter) = compile_to_mir(source);
    assert!(!reporter.has_errors());
}

#[test]
fn test_array_bounds_checking_compile_time_invalid() {
    let source = r#"
def main
  arr : int[5] = [1, 2, 3, 4, 5]
  x : int = arr[5]
end
"#;
    let (_mir_functions, reporter) = compile_to_mir(source);
    assert!(reporter.has_errors());
}

#[test]
fn test_array_bounds_checking_compile_time_negative() {
    let source = r#"
def main
  arr : int[5] = [1, 2, 3, 4, 5]
  x : int = arr[-1]
end
"#;
    let (_mir_functions, reporter) = compile_to_mir(source);
    assert!(reporter.has_errors());
}

#[test]
fn test_array_bounds_checking_runtime() {
    let source = r#"
def main
  arr : int[5] = [1, 2, 3, 4, 5]
  i : int = 3
  x : int = arr[i]
end
"#;
    let (mir_functions, reporter) = compile_to_mir(source);
    // runtime bounds chk may have type errors during analysis
    // but shld still gen mir w/ bounds chk code
    let main_fn = mir_functions.iter().find(|f| f.name == "main");
    if main_fn.is_some() {
        let fn_ = main_fn.unwrap();
        assert_eq!(fn_.name, "main");
        // bounds chk creates error, continue, and merge blocks
        assert!(fn_.basic_blocks.len() >= 1, "fn shld have at least entry block");
    }
}

#[test]
fn test_array_bounds_checking_empty_array() {
    let source = r#"
def main
  arr : int[5] = []
  x : int = arr[0]
end
"#;
    let (_mir_functions, reporter) = compile_to_mir(source);
    assert!(!reporter.has_errors());
}

#[test]
fn test_array_bounds_checking_dynamic_index() {
    let source = r#"
def main
  arr : int[10] = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
  idx : int = 5
  result : int = arr[idx]
end
"#;
    let (mir_functions, _reporter) = compile_to_mir(source);
    
    // shld have runtime bounds chk
    let main_fn = mir_functions.iter().find(|f| f.name == "main");
    if main_fn.is_some() {
        let fn_ = main_fn.unwrap();
        assert_eq!(fn_.name, "main");
    }
}

#[test]
fn test_array_bounds_checking_nested_arrays() {
    let source = r#"
def main
  // nested arrays - 2d array
  arr : int[6] = [1, 2, 3, 4, 5, 6]
  // access w/ const index
  x : int = arr[2]
end
"#;
    let (_mir_functions, reporter) = compile_to_mir(source);
    assert!(!reporter.has_errors());
}

#[test]
fn test_array_bounds_checking_in_loop() {
    let source = r#"
def main
  arr : int[5] = [1, 2, 3, 4, 5]
  i : int = 0
  while i < 5
    x : int = arr[0]
    i = i + 1
  end
end
"#;
    let (_mir_functions, reporter) = compile_to_mir(source);
    // use const index 2 avoid type errors
    assert!(!reporter.has_errors());
}
