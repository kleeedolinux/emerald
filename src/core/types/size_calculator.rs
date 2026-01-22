use crate::core::types::composite::StructType;
use crate::core::types::ty::Type;
use std::collections::HashMap;

pub struct SizeCalculator {
    struct_sizes: HashMap<String, usize>,
    struct_aligns: HashMap<String, usize>,
    calculating: Vec<String>, // 4 cycl detection
}

impl SizeCalculator {
    pub fn new() -> Self {
        Self {
            struct_sizes: HashMap::new(),
            struct_aligns: HashMap::new(),
            calculating: Vec::new(),
        }
    }

    pub fn calculate_size(&mut self, struct_type: &StructType) -> Result<usize, String> {
        if let Some(size) = self.struct_sizes.get(&struct_type.name) {
            return Ok(*size);
        }

        // cycle dtctn
        if self.calculating.contains(&struct_type.name) {
            return Err(format!(
                "Circular dependency detected for struct '{}'",
                struct_type.name
            ));
        }

        self.calculating.push(struct_type.name.clone());

        let mut total_size = 0;
        let mut max_align = 1;

        for field in &struct_type.fields {
            let field_size = self.type_size(&field.type_)?;
            let field_align = self.type_align(&field.type_);

            // align current offset
            total_size = align_to(total_size, field_align);
            
            max_align = max_align.max(field_align);
            total_size += field_size;
        }

        // align total size
        total_size = align_to(total_size, max_align);

        self.calculating.pop();
        self.struct_sizes.insert(struct_type.name.clone(), total_size);
        self.struct_aligns.insert(struct_type.name.clone(), max_align);

        Ok(total_size)
    }

    fn type_size(&mut self, type_: &Type) -> Result<usize, String> {
        match type_ {
            Type::Primitive(p) => Ok(p.size_in_bytes()),
            Type::Struct(s) => self.calculate_size(s),
            Type::Array(a) => {
                let element_size = self.type_size(&a.element)?;
                Ok(element_size * a.size)
            }
            Type::Pointer(_) => Ok(std::mem::size_of::<usize>()),
            Type::Generic(_) => Err("Cannot calculate size of generic type".to_string()),
            Type::Function(_) => Err("Functions don't have a size".to_string()),
            Type::TraitObject(_) => Ok(std::mem::size_of::<usize>() * 2), // data ptr + vtable ptr
            Type::String => Ok(std::mem::size_of::<usize>() * 2), // ptr + length
        }
    }

    fn type_align(&self, type_: &Type) -> usize {
        type_.align()
    }
}

fn align_to(value: usize, align: usize) -> usize {
    if align == 0 {
        return value;
    }
    (value + align - 1) & !(align - 1)
}

impl Default for SizeCalculator {
    fn default() -> Self {
        Self::new()
    }
}
