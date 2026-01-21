use crate::error::Reporter;
use crate::frontend::lexer::Lexer;
use crate::frontend::parser::Parser;
use crate::frontend::semantic::SemanticAnalyzer;
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

fn analyze_source(source: &str) -> (crate::core::ast::Ast, Reporter) {
    let mut files = Files::new();
    let file_id = files.add("test.em", source.to_string());
    let mut reporter = Reporter::new();
    let source_str = files.source(file_id).to_string();
    let mut lexer = Lexer::new(&source_str, file_id, &mut reporter);
    let tokens = lexer.tokenize();
    let mut parser = Parser::new(tokens, file_id, &mut reporter);
    let ast = parser.parse();
    
    if !reporter.has_errors() {
        let mut analyzer = SemanticAnalyzer::new(&mut reporter, file_id);
        analyzer.analyze(&ast);
    }
    
    if reporter.has_errors() {
        print_diagnostics(&reporter, &files);
    }
    
    (ast, reporter)
}

#[test]
fn test_name_resolution() {
    let source = r#"
def test
  x : int = 10
  y : int = x + 5
end
"#;
    let (_ast, reporter) = analyze_source(source);
    assert!(!reporter.has_errors());
}

#[test]
fn test_type_checking() {
    let source = r#"
def test
  x : int = 10
  y : float = 3.14
  z : float = x + y
end
"#;
    let (_ast, reporter) = analyze_source(source);
    // type promotion shld work
    assert!(!reporter.has_errors());
}

#[test]
fn test_undefined_variable() {
    let source = r#"
def test
  x : int = undefined_var
end
"#;
    let (_ast, reporter) = analyze_source(source);
    assert!(reporter.has_errors());
}

#[test]
fn test_type_mismatch() {
    let source = r#"
def test
  x : int = "hello"
end
"#;
    let (_ast, reporter) = analyze_source(source);
    assert!(reporter.has_errors());
}

#[test]
fn test_binary_operation_types() {
    let source = r#"
def test
  a : int = 10
  b : int = 20
  c : int = a + b
  d : bool = a == b
end
"#;
    let (_ast, reporter) = analyze_source(source);
    assert!(!reporter.has_errors());
}

#[test]
fn test_if_condition_must_be_bool() {
    let source = r#"
def test
  if 10
    x : int = 5
  end
end
"#;
    let (_ast, reporter) = analyze_source(source);
    assert!(reporter.has_errors());
}

#[test]
fn test_variable_without_type_annotation_error() {
    let source = r#"
def test
  let x = 10
end
"#;
    let (_ast, reporter) = analyze_source(source);
    assert!(reporter.has_errors());
}

#[test]
fn test_function_param_without_type_error() {
    let source = r#"
def test(param)
  x : int = 10
end
"#;
    let (_ast, reporter) = analyze_source(source);
    assert!(reporter.has_errors());
}

#[test]
fn test_global_without_type_error() {
    let source = r#"
x = 10
"#;
    let (_ast, reporter) = analyze_source(source);
    assert!(reporter.has_errors());
}

#[test]
fn test_optional_parens_with_type_checking() {
    let source = r#"
def add(a : int, b : int) returns int
  return a + b
end

def test
  result : int = add 10, 20
end
"#;
    let (_ast, reporter) = analyze_source(source);
    assert!(!reporter.has_errors());
}

#[test]
fn test_shadowing() {
    let source = r#"
def calc
  x : int = 50
  
  if true
    x : int = 100
  end
  
  x : int = 200
end
"#;
    let (_ast, reporter) = analyze_source(source);
    // shadowing should be allowed
    assert!(!reporter.has_errors());
}
