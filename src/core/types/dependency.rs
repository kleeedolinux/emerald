use crate::core::types::ty::Type;
use std::collections::{HashMap, HashSet, VecDeque};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum DependencyKind {
    ByValue,    // direct containment
    ByPointer,  // ptr ref
    ByNullablePointer, // nllbl ptr
}

#[derive(Debug, Clone)]
pub struct Dependency {
    pub target: String,
    pub kind: DependencyKind,
}

#[derive(Debug, Clone)]
pub struct DependencyGraph {
    // map from struct name 2 its dependencies
    dependencies: HashMap<String, Vec<Dependency>>,
    // track frwrd dclrtns
    forward_decls: HashSet<String>,
    // trck fully defined structs
    defined: HashSet<String>,
}

impl DependencyGraph {
    pub fn new() -> Self {
        Self {
            dependencies: HashMap::new(),
            forward_decls: HashSet::new(),
            defined: HashSet::new(),
        }
    }

    pub fn add_forward_decl(&mut self, name: String) {
        self.forward_decls.insert(name);
    }

    pub fn add_struct(&mut self, name: String, deps: Vec<Dependency>) {
        self.defined.insert(name.clone());
        self.dependencies.insert(name, deps);
    }

    pub fn is_forward_decl(&self, name: &str) -> bool {
        self.forward_decls.contains(name)
    }

    pub fn is_defined(&self, name: &str) -> bool {
        self.defined.contains(name)
    }

    /// detect cycles in the dpndncy grph
    /// returns some if a cycle is found none otherwise
    pub fn detect_cycles(&self) -> Option<Vec<String>> {
        let mut visited = HashSet::new();
        let mut rec_stack = HashSet::new();
        let mut cycle_path = Vec::new();

        for node in self.dependencies.keys() {
            if !visited.contains(node) {
                if self.detect_cycle_dfs(
                    node,
                    &mut visited,
                    &mut rec_stack,
                    &mut cycle_path,
                ) {
                    return Some(cycle_path);
                }
            }
        }

        None
    }

    fn detect_cycle_dfs(
        &self,
        node: &str,
        visited: &mut HashSet<String>,
        rec_stack: &mut HashSet<String>,
        cycle_path: &mut Vec<String>,
    ) -> bool {
        visited.insert(node.to_string());
        rec_stack.insert(node.to_string());
        cycle_path.push(node.to_string());

        if let Some(deps) = self.dependencies.get(node) {
            for dep in deps {
                // only consider by value dependenceis 4 cycle dtctn
                // pntrs break cycles
                if matches!(dep.kind, DependencyKind::ByValue) {
                    // skip if target is not in dpndncs
                    // also skip if target is only forward dclrd
                    if !self.dependencies.contains_key(&dep.target) {
                        continue;
                    }
                    // skip if trgt is forward declared but not yet defined
                    if self.is_forward_decl(&dep.target) && !self.is_defined(&dep.target) {
                        continue;
                    }
                    if !visited.contains(&dep.target) {
                        if self.detect_cycle_dfs(&dep.target, visited, rec_stack, cycle_path) {
                            return true;
                        }
                    } else if rec_stack.contains(&dep.target) {
                        // cycl detected
                        cycle_path.push(dep.target.clone());
                        return true;
                    }
                }
            }
        }

        rec_stack.remove(node);
        cycle_path.pop();
        false
    }

    /// topological sort 2 dtrmn resolution order
    /// rtrns structs in ordr they should be rslvd
    pub fn topological_sort(&self) -> Result<Vec<String>, Vec<String>> {
        let mut in_degree = HashMap::new();
        
        // intlz in dgr 4 all nds
        for node in self.dependencies.keys() {
            in_degree.insert(node.clone(), 0);
        }

        // calculate in degrees
        for (_, deps) in &self.dependencies {
            for dep in deps {
                if matches!(dep.kind, DependencyKind::ByValue) {
                    *in_degree.entry(dep.target.clone()).or_insert(0) += 1;
                }
            }
        }

        // khns algrthm
        let mut queue = VecDeque::new();
        for (node, degree) in &in_degree {
            if *degree == 0 {
                queue.push_back(node.clone());
            }
        }

        let mut result = Vec::new();
        while let Some(node) = queue.pop_front() {
            result.push(node.clone());

            if let Some(deps) = self.dependencies.get(&node) {
                for dep in deps {
                    if matches!(dep.kind, DependencyKind::ByValue) {
                        if let Some(degree) = in_degree.get_mut(&dep.target) {
                            *degree -= 1;
                            if *degree == 0 {
                                queue.push_back(dep.target.clone());
                            }
                        }
                    }
                }
            }
        }

        // chk 4 cycls
        let cycles: Vec<String> = in_degree
            .into_iter()
            .filter(|(_, degree)| *degree > 0)
            .map(|(node, _)| node)
            .collect();

        if cycles.is_empty() {
            Ok(result)
        } else {
            Err(cycles)
        }
    }

    /// extrct dependencies from a type
    pub fn extract_dependencies(type_: &Type) -> Vec<Dependency> {
        let mut deps = Vec::new();
        Self::extract_dependencies_recursive(type_, &mut deps);
        deps
    }

    fn extract_dependencies_recursive(type_: &Type, deps: &mut Vec<Dependency>) {
        match type_ {
            Type::Struct(s) => {
                deps.push(Dependency {
                    target: s.name.clone(),
                    kind: DependencyKind::ByValue,
                });
                for field in &s.fields {
                    Self::extract_dependencies_recursive(&field.type_, deps);
                }
            }
            Type::Pointer(p) => {
                let struct_name = Self::get_struct_name(&p.pointee);
                if !struct_name.is_empty() {
                    deps.push(Dependency {
                        target: struct_name,
                        kind: if p.nullable {
                            DependencyKind::ByNullablePointer
                        } else {
                            DependencyKind::ByPointer
                        },
                    });
                }
                // dont recursively extrct from pointee ptr breaks the dpndncy chain
                // only extract if we ned 2 nkow abt nested dependencies
            }
            Type::Array(a) => {
                Self::extract_dependencies_recursive(&a.element, deps);
            }
            Type::Function(f) => {
                for param in &f.params {
                    Self::extract_dependencies_recursive(param, deps);
                }
                Self::extract_dependencies_recursive(&f.return_type, deps);
            }
            Type::TraitObject(_) => {
                // trait objects dont have direct struct dependencies
                // they r dynamically dispatched
            }
            _ => {}
        }
    }

    fn get_struct_name(type_: &Type) -> String {
        match type_ {
            Type::Struct(s) => s.name.clone(),
            _ => String::new(),
        }
    }
}

impl Default for DependencyGraph {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_no_cycle() {
        let mut graph = DependencyGraph::new();
        graph.add_struct("A".to_string(), vec![
            Dependency {
                target: "B".to_string(),
                kind: DependencyKind::ByValue,
            },
        ]);
        graph.add_struct("B".to_string(), vec![]);
        
        assert!(graph.detect_cycles().is_none());
    }

    #[test]
    fn test_cycle_by_value() {
        let mut graph = DependencyGraph::new();
        graph.add_struct("A".to_string(), vec![
            Dependency {
                target: "B".to_string(),
                kind: DependencyKind::ByValue,
            },
        ]);
        graph.add_struct("B".to_string(), vec![
            Dependency {
                target: "A".to_string(),
                kind: DependencyKind::ByValue,
            },
        ]);
        
        assert!(graph.detect_cycles().is_some());
    }

    #[test]
    fn test_cycle_broken_by_pointer() {
        let mut graph = DependencyGraph::new();
        graph.add_struct("A".to_string(), vec![
            Dependency {
                target: "B".to_string(),
                kind: DependencyKind::ByPointer,
            },
        ]);
        graph.add_struct("B".to_string(), vec![
            Dependency {
                target: "A".to_string(),
                kind: DependencyKind::ByValue,
            },
        ]);
        
        // ptr breaks the cycle
        assert!(graph.detect_cycles().is_none());
    }
}
