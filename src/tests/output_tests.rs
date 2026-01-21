use crate::error::Reporter;
use crate::frontend::lexer::Lexer;
use crate::frontend::parser::Parser;
use crate::frontend::semantic::SemanticAnalyzer;
use crate::middle::{HirLowerer, MirLowerer};
use crate::core::optimizations::HirOptimizer;
use codespan::Files;
use std::fs;

fn format_hir(hir: &crate::core::hir::Hir) -> String {
    let mut output = String::new();
    output.push_str("=== HIR (High-Level Intermediate Representation) ===\n\n");
    
    for item in &hir.items {
        match item {
            crate::core::hir::HirItem::Function(f) => {
                output.push_str(&format!("function {}(", f.name));
                for (i, param) in f.params.iter().enumerate() {
                    if i > 0 {
                        output.push_str(", ");
                    }
                    output.push_str(&format!("{}: {:?}", param.name, param.type_));
                }
                output.push_str(")");
                if let Some(return_type) = &f.return_type {
                    output.push_str(&format!(" -> {:?}", return_type));
                }
                output.push_str(" {\n");
                
                if let Some(body) = &f.body {
                    for stmt in body {
                        output.push_str(&format!("  {:?}\n", stmt));
                    }
                }
                output.push_str("}\n\n");
            }
            crate::core::hir::HirItem::Struct(s) => {
                output.push_str(&format!("struct {} {{\n", s.name));
                for field in &s.fields {
                    output.push_str(&format!("  {}: {:?},\n", field.name, field.type_));
                }
                output.push_str("}\n\n");
            }
            _ => {
                output.push_str(&format!("{:?}\n\n", item));
            }
        }
    }
    
    output
}

fn format_mir(mir_functions: &[crate::core::mir::MirFunction]) -> String {
    let mut output = String::new();
    output.push_str("=== MIR (Mid-Level Intermediate Representation) ===\n\n");
    
    for func in mir_functions {
        output.push_str(&format!("function {}(", func.name));
        for (i, param) in func.params.iter().enumerate() {
            if i > 0 {
                output.push_str(", ");
            }
            output.push_str(&format!("{}: {:?} (local {:?})", param.name, param.type_, param.local));
        }
        output.push_str(")");
        if let Some(return_type) = &func.return_type {
            output.push_str(&format!(" -> {:?}", return_type));
        }
        output.push_str(" {\n");
        output.push_str(&format!("  entry_block: {}\n", func.entry_block));
        output.push_str(&format!("  locals: {}\n", func.locals.len()));
        output.push_str("\n");
        
        for (bb_id, bb) in func.basic_blocks.iter().enumerate() {
            output.push_str(&format!("  bb{}:\n", bb_id));
            for instr in &bb.instructions {
                output.push_str(&format!("    {:?}\n", instr));
            }
            if !bb.successors.is_empty() {
                output.push_str(&format!("    -> successors: {:?}\n", bb.successors));
            }
            output.push_str("\n");
        }
        
        output.push_str("}\n\n");
    }
    
    output
}

fn compile_and_output(source: &str, test_name: &str) -> Result<(), Box<dyn std::error::Error>> {
    // crt output directory 4 test artifacts
    let output_dir = "test_output";
    fs::create_dir_all(&output_dir)?;
    
    let mut files = Files::new();
    let file_id = files.add(format!("{}.em", test_name), source.to_string());
    let mut reporter = Reporter::new();
    let source_str = files.source(file_id).to_string();
    let mut lexer = Lexer::new(&source_str, file_id, &mut reporter);
    let tokens = lexer.tokenize();
    let mut parser = Parser::new(tokens, file_id, &mut reporter);
    let ast = parser.parse();
    
    // cntn even w/ parsing errrs 2 see what we can gen
    let symbol_table = if !reporter.has_errors() {
        let mut analyzer = SemanticAnalyzer::new(&mut reporter, file_id);
        analyzer.analyze(&ast)
    } else {
        crate::frontend::semantic::symbol_table::SymbolTable::new()
    };
    
    // lower 2 hir
    let mut hir_lowerer = HirLowerer::new(symbol_table);
    let mut hir = hir_lowerer.lower(&ast);
    
    // optimize hir
    let mut hir_optimizer = HirOptimizer::new();
    hir_optimizer.optimize(&mut hir);
    
    // lwr 2 mir
    let mut mir_lowerer = MirLowerer::new();
    let mir_functions = mir_lowerer.lower(&hir);
    
    // frmt and write hir
    let hir_output = format_hir(&hir);
    let hir_path = format!("{}/{}.hir", output_dir, test_name);
    fs::write(&hir_path, &hir_output)?;
    println!("Generated: {}", hir_path);
    
    // frmt and wrt mir
    let mir_output = format_mir(&mir_functions);
    let mir_path = format!("{}/{}.mir", output_dir, test_name);
    fs::write(&mir_path, &mir_output)?;
    println!("Generated: {}", mir_path);
    
    // add err info 2 files if thr were errrs
    if reporter.has_errors() {
        let mut error_info = "\n\n=== ERRORS ===\n".to_string();
        for diag in reporter.diagnostics() {
            error_info.push_str(&format!("  {} at {:?}\n", diag.message, diag.span));
        }
        let error_path = format!("{}/{}.errors", output_dir, test_name);
        fs::write(&error_path, &error_info)?;
        println!("Generated: {}", error_path);
    }
    
    Ok(())
}

#[test]
fn test_generate_complex_hir_mir() {
    let source = r#"
def fibonacci(n : int) returns int
  if n <= 1
    return n
  end
  return fibonacci(n - 1) + fibonacci(n - 2)
end

def process_numbers(count : int) returns int
  sum = 0
  i = 0
  while i < count
    sum = sum + i
    i = i + 1
  end
  return sum
end

def main
  fib_result = fibonacci(10)
  
  x : int = 10
  if true
    x_str : string = "shadowed"
  end
  
  computed = comptime 2 + 3 * 4
  
  if fib_result > 0
    x = 42
  else
    x = 24
  end
  
  counter = 0
  while counter < 10
    counter = counter + 1
    if counter == 5
      break
    end
  end
  
  result = process_numbers(100)
end
"#;
    
    let _ = compile_and_output(source, "complex_example");
    assert_file_generated("complex_example");
}

#[test]
fn test_basic_arithmetic() {
    let source = r#"
def test_arithmetic
  a = 10
  b = 20
  c = a + b
  d = a - b
  e = a * b
  f = b / a
  g = b % a
  h = -a
  i = a == b
  j = a != b
  k = a < b
  l = a <= b
  m = a > b
  n = a >= b
end
"#;
    
    let _ = compile_and_output(source, "basic_arithmetic");
    assert_file_generated("basic_arithmetic");
}

#[test]
fn test_control_flow() {
    let source = r#"
def test_control_flow
  x = 5
  
  if x > 0
    y = 10
  end
  
  if x < 0
    z = -1
  else
    z = 1
  end
  
  counter = 0
  while counter < 5
    counter = counter + 1
  end
  
  i = 0
  while i < 10
    i = i + 1
    if i == 5
      break
    end
  end
end
"#;
    
    let _ = compile_and_output(source, "control_flow");
    assert_file_generated("control_flow");
}

#[test]
fn test_functions() {
    let source = r#"
def add(a : int, b : int) returns int
  return a + b
end

def multiply(a : int, b : int) returns int
  return a * b
end

def factorial(n : int) returns int
  if n <= 1
    return 1
  end
  return n * factorial(n - 1)
end

def no_return
  x = 10
  y = 20
end

def main
  result1 = add(5, 3)
  result2 = multiply(4, 7)
  result3 = factorial(5)
  no_return()
end
"#;
    
    let _ = compile_and_output(source, "functions");
    assert_file_generated("functions");
}

#[test]
fn test_types_and_literals() {
    let source = r#"
def test_types
  i : int = 42
  f : float = 3.14
  b : bool = true
  c : char = 'A'
  s : string = "hello world"
  
  neg_int = -100
  zero = 0
  one = 1
  
  true_val = true
  false_val = false
  
  empty_string = ""
  long_string = "this is a very long string with multiple words"
end
"#;
    
    let _ = compile_and_output(source, "types_literals");
    assert_file_generated("types_literals");
}

#[test]
fn test_comptime_expressions() {
    let source = r#"
def test_comptime
  simple = comptime 2 + 2
  complex = comptime (10 + 5) * 3 - 7
  nested = comptime 2 * (3 + comptime 4 * 5)
  comparison = comptime 10 > 5
  arithmetic = comptime 100 / 4 + 25
end
"#;
    
    let _ = compile_and_output(source, "comptime");
    assert_file_generated("comptime");
}

#[test]
fn test_logical_operators() {
    let source = r#"
def test_logical
  a = true
  b = false
  
  and_result = a && b
  or_result = a || b
  not_a = not a
  not_b = not b
  
  complex = (a && b) || ((not a) && (not b))
  nested = (not (a || b)) && (a && b)
end
"#;
    
    let _ = compile_and_output(source, "logical");
    assert_file_generated("logical");
}

#[test]
fn test_variable_shadowing() {
    let source = r#"
def test_shadowing
  x : int = 10
  
  if true
    x : string = "inner"
    y = x
  end
  
  z = x
  
  if false
    x : float = 3.14
  else
    x : bool = true
  end
end
"#;
    
    let _ = compile_and_output(source, "shadowing");
    assert_file_generated("shadowing");
}

#[test]
fn test_nested_control_flow() {
    let source = r#"
def test_nested
  x = 0
  y = 0
  
  if x > 0
    if y > 0
      z = 1
    else
      z = 2
    end
  else
    if y < 0
      z = 3
    else
      z = 4
    end
  end
  
  i = 0
  while i < 5
    j = 0
    while j < 3
      k = i + j
      j = j + 1
    end
    i = i + 1
  end
end
"#;
    
    let _ = compile_and_output(source, "nested_control");
    assert_file_generated("nested_control");
}

#[test]
fn test_expression_precedence() {
    let source = r#"
def test_precedence
  a = 2 + 3 * 4
  b = (2 + 3) * 4
  c = 10 - 5 + 3
  d = 10 - (5 + 3)
  e = 2 * 3 + 4 * 5
  f = 2 * (3 + 4) * 5
  g = 10 / 2 + 3
  h = 10 / (2 + 3)
  i = 10 % 3 + 5
  j = (10 % 3) + 5
end
"#;
    
    let _ = compile_and_output(source, "precedence");
    assert_file_generated("precedence");
}

#[test]
fn test_mutability_and_assignments() {
    let source = r#"
def test_mutability
  x = 10
  x = 20
  x = x + 5
  
  y = 1
  y = y * 2
  y = y - 1
  y = y / 1
  
  z = true
  z = false
  z = !z
end
"#;
    
    let _ = compile_and_output(source, "mutability");
    assert_file_generated("mutability");
}

#[test]
fn test_complex_expressions() {
    let source = r#"
def test_complex
  a = 10
  b = 20
  c = 30
  
  result1 = a + b * c
  result2 = (a + b) * c
  result3 = a * b + c * a
  result4 = a == b && c > a
  result5 = a != b || c < a
  result6 = (not (a > b)) && c >= a
  result7 = a + b - c * a / b
end
"#;
    
    let _ = compile_and_output(source, "complex_expressions");
    assert_file_generated("complex_expressions");
}

#[test]
fn test_early_returns() {
    let source = r#"
def early_return1(x : int) returns int
  if x < 0
    return -1
  end
  return x * 2
end

def early_return2(x : int) returns int
  if x == 0
    return 0
  end
  if x == 1
    return 1
  end
  return x * x
end

def main
  result1 = early_return1(5)
  result2 = early_return1(-5)
  result3 = early_return2(0)
  result4 = early_return2(1)
  result5 = early_return2(10)
end
"#;
    
    let _ = compile_and_output(source, "early_returns");
    assert_file_generated("early_returns");
}

#[test]
fn test_while_loops() {
    let source = r#"
def test_while
  i = 0
  sum = 0
  
  while i < 10
    sum = sum + i
    i = i + 1
  end
  
  count = 0
  while count < 5
    count = count + 1
    if count == 3
      break
    end
  end
  
  j = 0
  while j < 100
    j = j + 1
    if j > 50
      break
    end
  end
end
"#;
    
    let _ = compile_and_output(source, "while_loops");
    assert_file_generated("while_loops");
}

#[test]
fn test_function_calls() {
    let source = r#"
def helper1(x : int) returns int
  return x + 1
end

def helper2(x : int, y : int) returns int
  return x + y
end

def helper3
  z = 42
end

def main
  a = helper1(5)
  b = helper2(10, 20)
  helper3()
  c = helper1(helper2(1, 2))
  d = helper2(helper1(5), helper1(10))
end
"#;
    
    let _ = compile_and_output(source, "function_calls");
    assert_file_generated("function_calls");
}

#[test]
fn test_edge_cases() {
    let source = r#"
def test_edges
  zero = 0
  one = 1
  negative = -1
  large = 1000000
  
  true_val = true
  false_val = false
  
  empty = ""
  single_char = "a"
  
  if true
    x = 1
  end
  
  if false
    y = 2
  else
    y = 3
  end
  
  i = 0
  while i < 0
    i = i + 1
  end
end
"#;
    
    let _ = compile_and_output(source, "edge_cases");
    assert_file_generated("edge_cases");
}

// helper fn 2 chk if fls were generated
fn assert_file_generated(test_name: &str) {
    let output_dir = "test_output";
    let hir_exists = std::path::Path::new(&format!("{}/{}.hir", output_dir, test_name)).exists();
    let mir_exists = std::path::Path::new(&format!("{}/{}.mir", output_dir, test_name)).exists();
    
    println!("{} - HIR file exists: {}", test_name, hir_exists);
    println!("{} - MIR file exists: {}", test_name, mir_exists);
    
    assert!(hir_exists || mir_exists, "Neither HIR nor MIR files were generated for {}", test_name);
}
