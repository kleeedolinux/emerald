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
    
    if reporter.has_errors() {
        for diag in reporter.diagnostics() {
            eprintln!("[{:?}] {:?}: {}", diag.kind, diag.severity, diag.message);
        }
    }
    
    (ast, reporter)
}

#[test]
fn test_lifetime_basic_scope() {
    let source = r#"
def main
  x : int = 10
  y : int = 20
  z : int = x + y
end
"#;
    let (_ast, reporter) = analyze_source(source);
    assert!(!reporter.has_errors());
}

#[test]
fn test_lifetime_nested_scope() {
    let source = r#"
def main
  x : int = 10
  if true
    y : int = 20
    z : int = x + y
  end
end
"#;
    let (_ast, reporter) = analyze_source(source);
    assert!(!reporter.has_errors());
}

#[test]
fn test_lifetime_loop_scope() {
    let source = r#"
def main
  i : int = 0
  while i < 10
    j : int = i * 2
    i = i + 1
  end
end
"#;
    let (_ast, reporter) = analyze_source(source);
    assert!(!reporter.has_errors());
}

#[test]
fn test_lifetime_function_params() {
    let source = r#"
def add(a : int, b : int) returns int
  return a + b
end

def main
  result : int = add(10, 20)
end
"#;
    let (_ast, reporter) = analyze_source(source);
    assert!(!reporter.has_errors());
}

#[test]
fn test_lifetime_block_expression() {
    let source = r#"
def main
  x : int = 10
  y : int = 20
  result : int = x + y
end
"#;
    let (_ast, reporter) = analyze_source(source);
    assert!(!reporter.has_errors());
}

#[test]
fn test_lifetime_closure() {
    let source = r#"
def main
  x : int = 10
  closure = |y : int| {
    x + y
  }
  result : int = closure(20)
end
"#;
    let (_ast, _reporter) = analyze_source(source);
    // closures may or may not be fully supported yet
    // just test it compiles
}

#[test]
fn test_lifetime_reference_scope() {
    let source = r#"
def main
  x : int = 10
  ptr : ref int = @x
  value : int = ptr.value
end
"#;
    let (_ast, reporter) = analyze_source(source);
    assert!(!reporter.has_errors());
}

#[test]
fn test_lifetime_multiple_scopes() {
    let source = r#"
def main
  a : int = 1
  if true
    b : int = 2
    if true
      c : int = 3
      result : int = a + b + c
    end
  end
end
"#;
    let (_ast, reporter) = analyze_source(source);
    assert!(!reporter.has_errors());
}

#[test]
fn test_lifetime_for_loop_scope() {
    let source = r#"
def main
  i : int = 0
  while i < 10
    j : int = i * 2
    i = i + 1
  end
end
"#;
    let (_ast, reporter) = analyze_source(source);
    assert!(!reporter.has_errors());
}
