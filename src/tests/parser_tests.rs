use crate::error::Reporter;
use crate::frontend::lexer::Lexer;
use crate::frontend::parser::Parser;
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

fn parse_source(source: &str) -> (crate::core::ast::Ast, Reporter) {
    let mut files = Files::new();
    let file_id = files.add("test.em", source.to_string());
    let mut reporter = Reporter::new();
    let source_str = files.source(file_id).to_string();
    
    // Debug: Show source code
    if std::env::var("DEBUG_TESTS").is_ok() {
        eprintln!("=== Parsing source ===");
        eprintln!("{}", source);
    }
    
    let mut lexer = Lexer::new(&source_str, file_id, &mut reporter);
    let tokens = lexer.tokenize();
    
    // Debug: Show tokens
    if std::env::var("DEBUG_TESTS").is_ok() {
        eprintln!("=== Tokens ===");
        for (i, token) in tokens.iter().enumerate() {
            eprintln!("  [{}] {:?}", i, token.kind);
        }
    }
    
    let mut parser = Parser::new(tokens, file_id, &mut reporter);
    let ast = parser.parse();
    
    // Debug: Show AST
    if std::env::var("DEBUG_TESTS").is_ok() {
        eprintln!("=== AST ===");
        eprintln!("{:#?}", ast);
    }
    
    if reporter.has_errors() {
        print_diagnostics(&reporter, &files);
    }
    (ast, reporter)
}

#[test]
fn test_parse_function() {
    let source = r#"
def add(a : int, b : int) returns int
  return a + b
end
"#;
    let (ast, reporter) = parse_source(source);
    assert!(!reporter.has_errors());
    assert_eq!(ast.items.len(), 1);
}

#[test]
fn test_parse_struct() {
    let source = r#"
struct Point
  x : int
  y : int
end
"#;
    let (ast, reporter) = parse_source(source);
    assert!(!reporter.has_errors());
    assert_eq!(ast.items.len(), 1);
}

#[test]
fn test_parse_binary_expression() {
    let source = r#"
def test
  x : int = 10 + 20
  y : int = 5 * 3
end
"#;
    let (_ast, reporter) = parse_source(source);
    assert!(!reporter.has_errors());
}

#[test]
fn test_parse_if_statement() {
    let source = r#"
def test
  if true
    x : int = 10
  else
    x : int = 20
  end
end
"#;
    let (_ast, reporter) = parse_source(source);
    assert!(!reporter.has_errors());
}

#[test]
fn test_parse_while_loop() {
    let source = r#"
def test
  mut i : int = 0
  while i < 10
    i = i + 1
  end
end
"#;
    let (_ast, reporter) = parse_source(source);
    assert!(!reporter.has_errors());
}

#[test]
fn test_parse_pointer_operations() {
    let source = r#"
def test
  val : int = 100
  ptr : ref int = at val
  ptr.value = 200
end
"#;
    let (_ast, reporter) = parse_source(source);
    assert!(!reporter.has_errors());
}

#[test]
fn test_parse_nullable_pointer() {
    let source = r#"
def test(target : ref? int)
  if target.exists?
    target.value = 0
  end
end
"#;
    let (_ast, reporter) = parse_source(source);
    assert!(!reporter.has_errors());
}

#[test]
fn test_parse_block_closure() {
    let source = r#"
def test
  10.times do |i : int|
    print i
  end
end
"#;
    let (_ast, reporter) = parse_source(source);
    assert!(!reporter.has_errors());
}

#[test]
fn test_parse_generic_struct() {
    let source = r#"
struct Box [ Type T ]
  item : T
end
"#;
    let (_ast, reporter) = parse_source(source);
    assert!(!reporter.has_errors());
}

#[test]
fn test_parse_trait() {
    let source = r#"
trait Shape
  def area(self : ref Shape) returns float
end
"#;
    let (_ast, reporter) = parse_source(source);
    assert!(!reporter.has_errors());
}

#[test]
fn test_parse_forward_declare() {
    let source = r#"
declare struct Parent
struct Child
  dad : ref Parent
end
struct Parent
  favorite : Child
end
"#;
    let (ast, reporter) = parse_source(source);
    assert!(!reporter.has_errors());
    assert_eq!(ast.items.len(), 3);
    match &ast.items[0] {
        crate::core::ast::item::Item::ForwardDecl(f) => {
            assert_eq!(f.name, "Parent");
        }
        _ => panic!("Expected ForwardDecl"),
    }
}

#[test]
fn test_parse_function_call_without_parens_single_arg() {
    let source = r#"
def print(msg : string)
end

def test
  print "Hello"
end
"#;
    let (_ast, reporter) = parse_source(source);
    assert!(!reporter.has_errors());
}

#[test]
fn test_parse_function_call_without_parens_multi_arg() {
    let source = r#"
def add(a : int, b : int) returns int
  return a + b
end

def test
  add 10, 20
end
"#;
    let (_ast, reporter) = parse_source(source);
    assert!(!reporter.has_errors());
}

#[test]
fn test_parse_function_call_with_parens() {
    let source = r#"
def add(a : int, b : int) returns int
  return a + b
end

def test
  add(10, 20)
end
"#;
    let (_ast, reporter) = parse_source(source);
    assert!(!reporter.has_errors());
}

#[test]
fn test_parse_function_def_without_parens() {
    let source = r#"
def add a : int, b : int returns int
  return a + b
end
"#;
    let (ast, reporter) = parse_source(source);
    assert!(!reporter.has_errors());
    assert_eq!(ast.items.len(), 1);
}

#[test]
fn test_parse_function_def_with_parens() {
    let source = r#"
def add(a : int, b : int) returns int
  return a + b
end
"#;
    let (ast, reporter) = parse_source(source);
    assert!(!reporter.has_errors());
    assert_eq!(ast.items.len(), 1);
}

#[test]
fn test_parse_function_def_empty_args_requires_parens() {
    let source = r#"
def main()
  print "Hello"
end
"#;
    let (ast, reporter) = parse_source(source);
    assert!(!reporter.has_errors());
    assert_eq!(ast.items.len(), 1);
}

#[test]
fn test_parse_method_call_without_parens() {
    let source = r#"
def test
  obj.method arg1, arg2
end
"#;
    let (_ast, reporter) = parse_source(source);
    // This might need method call support, but test the parsing
    // Note: This may fail if method calls aren't fully supported yet
    assert!(!reporter.has_errors());
}
