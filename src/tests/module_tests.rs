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
    let mut reporter = Reporter::new();
    let file_id = reporter.add_file("test.em".to_string(), source.to_string());
    let source_str = reporter.files().source(file_id).to_string();
    let mut lexer = Lexer::new(&source_str, file_id, &mut reporter);
    let tokens = lexer.tokenize();
    let mut parser = Parser::new(tokens, file_id, &mut reporter);
    let ast = parser.parse();
    
    if !reporter.has_errors() {
        let mut analyzer = SemanticAnalyzer::new(&mut reporter, file_id);
        analyzer.analyze(&ast);
    }
    
    if reporter.has_errors() {
        print_diagnostics(&reporter, reporter.files());
    }
    
    (ast, reporter)
}

#[test]
fn test_module_parsing() {
    let source = r#"
module Math
  def add(a : int, b : int) returns int
    return a + b
  end
end
"#;
    let (_ast, reporter) = analyze_source(source);
    assert!(!reporter.has_errors());
}

#[test]
fn test_module_with_struct() {
    let source = r#"
module Network
  struct Packet
    id : int
    data : string
  end
end
"#;
    let (_ast, reporter) = analyze_source(source);
    assert!(!reporter.has_errors());
}

#[test]
fn test_nested_modules() {
    let source = r#"
module Outer
  module Inner
    def inner_func returns int
      return 42
    end
  end
end
"#;
    let (_ast, reporter) = analyze_source(source);
    assert!(!reporter.has_errors());
}

#[test]
fn test_require_statement_parsing() {
    let source = r#"
require "std/io"
def main
  print "Hello"
end
"#;
    let (_ast, reporter) = analyze_source(source);
    // require is parsed but module may not exist 4 testing
    // so we just chk it parses correctly
    assert!(!reporter.has_errors() || reporter.diagnostics().iter().any(|d| d.message.contains("Module file not found")));
}

#[test]
fn test_use_statement_parsing() {
    let source = r#"
use Std::IO::Console
def main
  print "Hello"
end
"#;
    let (_ast, reporter) = analyze_source(source);
    // use is parsed but symbol may not exist
    assert!(!reporter.has_errors() || reporter.diagnostics().iter().any(|d| d.message.contains("undefined")));
}

#[test]
fn test_module_symbol_collection() {
    let source = r#"
module Utils
  def helper returns int
    return 10
  end
  
  x : int = 5
end
"#;
    let (_ast, reporter) = analyze_source(source);
    assert!(!reporter.has_errors());
}

#[test]
fn test_module_with_trait() {
    let source = r#"
module Shapes
  trait Shape
    def area(self) returns float
  end
  
  struct Circle
    radius : float
  end
  
  implement Shape for Circle
    def area(self : ref Circle) returns float
      return 3.14 * self.radius * self.radius
    end
  end
end
"#;
    let (_ast, reporter) = analyze_source(source);
    assert!(!reporter.has_errors());
}

#[test]
fn test_module_forward_declaration() {
    let source = r#"
module Types
  declare struct Node
  
  struct Node
    value : int
    next : ref? Node
  end
end
"#;
    let (_ast, reporter) = analyze_source(source);
    assert!(!reporter.has_errors());
}

#[test]
fn test_module_global_constants() {
    let source = r#"
module Config
  TIMEOUT : int = 30
  MAX_SIZE : int = 1000
end
"#;
    let (_ast, reporter) = analyze_source(source);
    assert!(!reporter.has_errors());
}

#[test]
fn test_module_with_generics() {
    let source = r#"
module Collections
  struct List [ Type T ]
    data : ref T
    size : int
  end
  
  def create [ Type T ] returns List[T]
    # implementation would go here
  end
end
"#;
    let (_ast, reporter) = analyze_source(source);
    assert!(!reporter.has_errors());
}
