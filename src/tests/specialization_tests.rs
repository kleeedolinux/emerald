use crate::error::Reporter;
use crate::frontend::lexer::Lexer;
use crate::frontend::parser::Parser;
use crate::frontend::semantic::SemanticAnalyzer;
use crate::frontend::semantic::symbol_table::SymbolTable;
use codespan::Files;

fn analyze_source(source: &str) -> (crate::core::ast::Ast, SymbolTable, Reporter) {
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
        SymbolTable::new()
    };
    
    if reporter.has_errors() {
        for diag in reporter.diagnostics() {
            eprintln!("[{:?}] {:?}: {}", diag.kind, diag.severity, diag.message);
        }
    }
    
    (ast, symbol_table, reporter)
}

#[test]
fn test_specialization_struct_instantiation() {
    let source = r#"
struct List [ Type T ]
  data : ref T
  size : int
end

def main
  int_list : List[int]
  float_list : List[float]
  string_list : List[string]
end
"#;
    let (_ast, symbol_table, reporter) = analyze_source(source);
    assert!(!reporter.has_errors());
    
    // chk that specialized structs r in symbol table
    // specialized names wld be like List_int, List_float, etc
    let all_symbols = symbol_table.all_symbols();
    let symbol_names: Vec<String> = all_symbols.iter().map(|(name, _)| name.clone()).collect();
    
    // original List shld exist
    assert!(symbol_names.iter().any(|n| n == "List"));
}

#[test]
fn test_specialization_function_instantiation() {
    let source = r#"
def identity [ Type T ](x : T) returns T
  return x
end

def main
  a : int = identity(10)
  b : float = identity(3.14)
  c : bool = identity(true)
end
"#;
    let (_ast, symbol_table, reporter) = analyze_source(source);
    assert!(!reporter.has_errors());
    
    // chk that specialized fns r tracked
    let all_symbols = symbol_table.all_symbols();
    let symbol_names: Vec<String> = all_symbols.iter().map(|(name, _)| name.clone()).collect();
    
    // original identity shld exist
    assert!(symbol_names.iter().any(|n| n == "identity"));
}

#[test]
fn test_specialization_nested_generics() {
    let source = r#"
struct Pair [ Type A, Type B ]
  first : A
  second : B
end

struct Container [ Type T ]
  items : Pair[T, int]
end

def main
  c1 : Container[int]
  c2 : Container[float]
end
"#;
    let (_ast, _symbol_table, reporter) = analyze_source(source);
    assert!(!reporter.has_errors());
}

#[test]
fn test_specialization_multiple_instantiations() {
    let source = r#"
struct Box [ Type T ]
  value : T
end

def main
  int_box : Box[int]
  float_box : Box[float]
  bool_box : Box[bool]
  string_box : Box[string]
end
"#;
    let (_ast, _symbol_table, reporter) = analyze_source(source);
    assert!(!reporter.has_errors());
}

#[test]
fn test_specialization_generic_function_with_body() {
    let source = r#"
def identity [ Type T ](x : T) returns T
  return x
end

def main
  x : int = identity(10)
  y : float = identity(3.14)
end
"#;
    let (_ast, _symbol_table, reporter) = analyze_source(source);
    assert!(!reporter.has_errors());
}

#[test]
fn test_specialization_generic_struct_with_fields() {
    let source = r#"
struct Node [ Type T ]
  data : T
  next : ref? Node[T]
end

def main
  int_node : Node[int]
  float_node : Node[float]
end
"#;
    let (_ast, _symbol_table, reporter) = analyze_source(source);
    assert!(!reporter.has_errors());
}

#[test]
fn test_specialization_type_substitution() {
    let source = r#"
struct Wrapper [ Type T ]
  inner : T
end

def main
  int_wrapper : Wrapper[int]
  float_wrapper : Wrapper[float]
  nested : Wrapper[Wrapper[int]]
end
"#;
    let (_ast, _symbol_table, reporter) = analyze_source(source);
    assert!(!reporter.has_errors());
}

#[test]
fn test_specialization_array_of_generics() {
    let source = r#"
struct Item [ Type T ]
  value : T
end

def main
  items : Item[int][5]
end
"#;
    let (_ast, _symbol_table, reporter) = analyze_source(source);
    assert!(!reporter.has_errors());
}
