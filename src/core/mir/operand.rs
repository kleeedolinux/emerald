
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Operand {
    Constant(Constant),
    Local(Local),
    Function(FunctionRef),
}

impl Eq for Constant {}

impl std::hash::Hash for Constant {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        match self {
            Constant::Int(i) => {
                state.write_u8(0);
                i.hash(state);
            }
            Constant::Float(f) => {
                state.write_u8(1);
                f.to_bits().hash(state);
            }
            Constant::Bool(b) => {
                state.write_u8(2);
                b.hash(state);
            }
            Constant::Char(c) => {
                state.write_u8(3);
                (*c as u32).hash(state);
            }
            Constant::String(s) => {
                state.write_u8(4);
                s.hash(state);
            }
            Constant::Null => {
                state.write_u8(5);
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Constant {
    Int(i64),
    Float(f64),
    Bool(bool),
    Char(char),
    String(String),
    Null,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Local {
    pub id: usize,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FunctionRef {
    pub name: String,
}

impl Local {
    pub fn new(id: usize) -> Self {
        Self { id }
    }
}
