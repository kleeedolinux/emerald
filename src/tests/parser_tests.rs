use crate::error::Reporter;
use crate::frontend::lexer::Lexer;
use crate::frontend::parser::Parser;
use codespan::Files;

fn parse_source(source: &str) -> (crate::core::ast::Ast, Reporter) {
    let mut files = Files::new();
    let file_id = files.add("test.em", source.to_string());
    let mut reporter = Reporter::new();
    let source_str = files.source(file_id).to_string();
    let mut lexer = Lexer::new(&source_str, file_id, &mut reporter);
    let tokens = lexer.tokenize();
    let mut parser = Parser::new(tokens, file_id, &mut reporter);
    let ast = parser.parse();
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
  x = 10 + 20
  y = 5 * 3
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
    x = 10
  else
    x = 20
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
  mut i = 0
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
  10.times do |i|
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
  def area(self) returns float
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
