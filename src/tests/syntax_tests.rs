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
fn test_optional_parens_function_call_single_arg() {
    let source = r#"
def say(msg : string)
end

def test
  say "Hello World"
end
"#;
    let (_ast, reporter) = analyze_source(source);
    assert!(!reporter.has_errors());
}

#[test]
fn test_optional_parens_function_call_multi_arg() {
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
fn test_optional_parens_function_call_with_parens() {
    let source = r#"
def add(a : int, b : int) returns int
  return a + b
end

def test
  result : int = add(10, 20)
end
"#;
    let (_ast, reporter) = analyze_source(source);
    assert!(!reporter.has_errors());
}

#[test]
fn test_optional_parens_function_def_without_parens() {
    let source = r#"
def add a : int, b : int returns int
  return a + b
end
"#;
    let (ast, reporter) = analyze_source(source);
    assert!(!reporter.has_errors());
    assert_eq!(ast.items.len(), 1);
}

#[test]
fn test_optional_parens_function_def_with_parens() {
    let source = r#"
def add(a : int, b : int) returns int
  return a + b
end
"#;
    let (ast, reporter) = analyze_source(source);
    assert!(!reporter.has_errors());
    assert_eq!(ast.items.len(), 1);
}

#[test]
fn test_empty_args_require_parens() {
    let source = r#"
def main()
  print "Hello"
end
"#;
    let (ast, reporter) = analyze_source(source);
    assert!(!reporter.has_errors());
    assert_eq!(ast.items.len(), 1);
}

#[test]
fn test_required_types_everywhere() {
    let source = r#"
def test
  x : int = 42
  y : float = 3.14
  z : bool = true
  msg : string = "Hello"
end
"#;
    let (_ast, reporter) = analyze_source(source);
    assert!(!reporter.has_errors());
}

#[test]
fn test_shadowing_example() {
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
    assert!(!reporter.has_errors());
}

#[test]
fn test_trait_implementation() {
    let source = r#"
trait Shape
  def area(self : ref Shape) returns float
end

struct Circle
  radius : float
end

implement Shape for Circle
  def area(self : ref Circle) returns float
    return 3.14 * self.radius * self.radius
  end
end
"#;
    let (_ast, reporter) = analyze_source(source);
    assert!(!reporter.has_errors());
}

#[test]
fn test_generic_struct() {
    let source = r#"
struct Box [ Type T ]
  item : T
end

def test
  b : Box[int]
  b.item = 42
end
"#;
    let (_ast, reporter) = analyze_source(source);
    assert!(!reporter.has_errors());
}

#[test]
fn test_pointer_operations() {
    let source = r#"
def main
  val : int = 100
  ptr : ref int = at val
  ptr.value = 200
end
"#;
    let (_ast, reporter) = analyze_source(source);
    assert!(!reporter.has_errors());
}

#[test]
fn test_nullable_pointer() {
    let source = r#"
def reset(target : ref? int)
  if target.exists?
    target.value = 0
  end
end
"#;
    let (_ast, reporter) = analyze_source(source);
    assert!(!reporter.has_errors());
}

#[test]
fn test_forward_declare() {
    let source = r#"
declare struct Parent

struct Child
  dad : ref Parent
end

struct Parent
  favorite : Child
end
"#;
    let (_ast, reporter) = analyze_source(source);
    assert!(!reporter.has_errors());
}

#[test]
fn test_module() {
    let source = r#"
module Network
  TIMEOUT : int = 30
  
  struct Packet
    id : int
  end
end
"#;
    let (_ast, reporter) = analyze_source(source);
    assert!(!reporter.has_errors());
}

#[test]
fn test_one_liner_function() {
    let source = r#"
def sub(a : int, b : int) returns int = a - b
"#;
    let (ast, reporter) = analyze_source(source);
    assert!(!reporter.has_errors());
    assert_eq!(ast.items.len(), 1);
}

#[test]
fn test_function_with_uses() {
    let source = r#"
def main uses IO, Memory
  x : int = 10
end
"#;
    let (_ast, reporter) = analyze_source(source);
    assert!(!reporter.has_errors());
}
