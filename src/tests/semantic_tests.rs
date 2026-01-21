use crate::error::Reporter;
use crate::frontend::lexer::Lexer;
use crate::frontend::parser::Parser;
use crate::frontend::semantic::SemanticAnalyzer;
use codespan::Files;

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
    
    (ast, reporter)
}

#[test]
fn test_name_resolution() {
    let source = r#"
def test
  x : int = 10
  y = x + 5
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
  z = x + y
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
  x = undefined_var
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
  c = a + b
  d = a == b
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
    x = 5
  end
end
"#;
    let (_ast, reporter) = analyze_source(source);
    assert!(reporter.has_errors());
}
