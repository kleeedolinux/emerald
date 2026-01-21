use crate::error::Reporter;
use crate::frontend::lexer::Lexer;
use crate::frontend::parser::Parser;
use crate::frontend::semantic::SemanticAnalyzer;
use crate::middle::{HirLowerer, MirLowerer};
use codespan::Files;

fn compile_to_hir_mir(source: &str, test_name: &str) -> (crate::core::hir::Hir, Vec<crate::core::mir::MirFunction>, Reporter) {
    let mut files = Files::new();
    let file_id = files.add(format!("{}.em", test_name), source.to_string());
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
    
    (hir, mir_functions, reporter)
}

#[test]
fn test_function_call_basic() {
    let source = r#"
def add(a : int, b : int) returns int
  return a + b
end

def main
  result = add(10, 20)
end
"#;
    let (hir, mir_functions, reporter) = compile_to_hir_mir(source, "test_function_call_basic");
    
    assert!(!reporter.has_errors());
    assert!(!hir.items.is_empty());
    assert!(!mir_functions.is_empty());
}

#[test]
fn test_function_call_with_expressions() {
    let source = r#"
def multiply(a : int, b : int) returns int
  return a * b
end

def calculate
  x = multiply(5, 6)
  y = multiply(x, 2)
  return y
end
"#;
    let (hir, mir_functions, reporter) = compile_to_hir_mir(source, "test_function_call_with_expressions");
    
    assert!(!reporter.has_errors());
    assert_eq!(hir.items.len(), 2);
    assert!(mir_functions.len() >= 2);
}

#[test]
fn test_nested_function_calls() {
    let source = r#"
def add(a : int, b : int) returns int
  return a + b
end

def triple(x : int) returns int
  return add(x, add(x, x))
end
"#;
    let (hir, mir_functions, reporter) = compile_to_hir_mir(source, "test_nested_function_calls");
    
    assert!(!reporter.has_errors());
    assert_eq!(hir.items.len(), 2);
}

#[test]
fn test_complex_code_generates_hir_mir() {
    let source = r#"
struct Point
  x : int
  y : int
end

def distance(p1 : ref Point, p2 : ref Point) returns float
  dx = p1.x - p2.x
  dy = p1.y - p2.y
  return dx * dx + dy * dy
end

def main
  p1 : Point
  p1.x = 0
  p1.y = 0
  
  p2 : Point
  p2.x = 3
  p2.y = 4
  
  dist = distance(at p1, at p2)
end
"#;
    let (hir, mir_functions, reporter) = compile_to_hir_mir(source, "test_complex_code");
    
    assert!(!reporter.has_errors());
    assert!(!hir.items.is_empty());
    assert!(!mir_functions.is_empty());
    
    let hir_output = format!("{:#?}", hir);
    let mir_output: String = mir_functions.iter()
        .map(|f| format!("{:#?}", f))
        .collect::<Vec<_>>()
        .join("\n\n");
    
    assert!(!hir_output.is_empty());
    assert!(!mir_output.is_empty());
}

#[test]
fn test_closure_with_captures() {
    let source = r#"
def create_adder(x : int) returns int
  closure = do |y|
    return x + y
  end
  return closure(5)
end
"#;
    let (hir, mir_functions, reporter) = compile_to_hir_mir(source, "test_closure");
    
    assert!(!reporter.has_errors());
    assert!(!hir.items.is_empty());
}

#[test]
fn test_control_flow_complex() {
    let source = r#"
def factorial(n : int) returns int
  if n <= 1
    return 1
  else
    return n * factorial(n - 1)
  end
end

def fibonacci(n : int) returns int
  if n <= 1
    return n
  end
  return fibonacci(n - 1) + fibonacci(n - 2)
end
"#;
    let (hir, mir_functions, reporter) = compile_to_hir_mir(source, "test_control_flow");
    
    assert!(!reporter.has_errors());
    assert_eq!(hir.items.len(), 2);
    assert!(mir_functions.len() >= 2);
}

#[test]
fn test_loops_with_functions() {
    let source = r#"
def sum_array(arr : ref int, size : int) returns int
  total = 0
  i = 0
  while i < size
    total = total + arr[i]
    i = i + 1
  end
  return total
end
"#;
    let (hir, mir_functions, reporter) = compile_to_hir_mir(source, "test_loops");
    
    assert!(!reporter.has_errors());
    assert!(!hir.items.is_empty());
}

#[test]
fn test_shadowing_in_functions() {
    let source = r#"
def test_shadow
  x : int = 10
  if true
    x : int = 20
    if true
      x : int = 30
    end
  end
  return x
end
"#;
    let (hir, mir_functions, reporter) = compile_to_hir_mir(source, "test_shadowing_func");
    
    assert!(!reporter.has_errors());
}

#[test]
fn test_comptime_in_functions() {
    let source = r#"
def get_constant returns int
  comptime value = 5 + 3
  return value
end
"#;
    let (hir, mir_functions, reporter) = compile_to_hir_mir(source, "test_comptime_func");
    
    assert!(!reporter.has_errors());
}
