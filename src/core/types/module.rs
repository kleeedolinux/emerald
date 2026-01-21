use std::collections::{HashMap, HashSet, VecDeque};

/// module dependency graph tracks dependencies between modules
/// used 2 detect circular dependencies and determine load order
pub struct ModuleDependencyGraph {
    dependencies: HashMap<String, Vec<String>>,
    modules: HashSet<String>,
}

impl ModuleDependencyGraph {
    pub fn new() -> Self {
        Self {
            dependencies: HashMap::new(),
            modules: HashSet::new(),
        }
    }

    /// add a module 2 the graph
    pub fn add_module(&mut self, module_path: String) {
        let module_path_clone = module_path.clone();
        self.modules.insert(module_path_clone);
        self.dependencies.entry(module_path).or_insert_with(Vec::new);
    }

    /// add a dependency from one module 2 another
    /// module depends on dependency
    pub fn add_dependency(&mut self, module: String, dependency: String) {
        self.modules.insert(module.clone());
        self.modules.insert(dependency.clone());
        self.dependencies
            .entry(module)
            .or_insert_with(Vec::new)
            .push(dependency);
    }

    /// detect circular dependencies in the module graph
    /// returns some cycle if found none otherwise
    pub fn detect_cycles(&self) -> Option<Vec<String>> {
        let mut visited = HashSet::new();
        let mut rec_stack = HashSet::new();
        let mut cycle_path = Vec::new();

        for module in &self.modules {
            if !visited.contains(module) {
                if self.detect_cycle_dfs(
                    module,
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
        module: &str,
        visited: &mut HashSet<String>,
        rec_stack: &mut HashSet<String>,
        cycle_path: &mut Vec<String>,
    ) -> bool {
        visited.insert(module.to_string());
        rec_stack.insert(module.to_string());
        cycle_path.push(module.to_string());

        if let Some(deps) = self.dependencies.get(module) {
            for dep in deps {
                if !visited.contains(dep) {
                    if self.detect_cycle_dfs(dep, visited, rec_stack, cycle_path) {
                        return true;
                    }
                } else if rec_stack.contains(dep) {
                    // cycle detected
                    cycle_path.push(dep.clone());
                    return true;
                }
            }
        }

        rec_stack.remove(module);
        cycle_path.pop();
        false
    }

    /// topological sort 2 determine module load order
    /// returns modules in order they should be loaded
    pub fn topological_sort(&self) -> Result<Vec<String>, Vec<String>> {
        let mut in_degree = HashMap::new();

        // initialize in degree 4 all modules (count of dependencies each module has)
        for module in &self.modules {
            in_degree.insert(module.clone(), 0);
        }

        // calculate in degrees - how many things each module depends on
        for (module, deps) in &self.dependencies {
            let count = deps.len();
            in_degree.insert(module.clone(), count);
        }

        // kahn's algorithm - start w/ modules that have no dependencies
        let mut queue = VecDeque::new();
        for (module, degree) in &in_degree {
            if *degree == 0 {
                queue.push_back(module.clone());
            }
        }

        let mut result = Vec::new();
        while let Some(module) = queue.pop_front() {
            result.push(module.clone());

            // find modules that depend on this one and decrement their in-degree
            for (other_module, deps) in &self.dependencies {
                if deps.contains(&module) {
                    if let Some(degree) = in_degree.get_mut(other_module) {
                        *degree -= 1;
                        if *degree == 0 {
                            queue.push_back(other_module.clone());
                        }
                    }
                }
            }
        }

        // check 4 cycles
        let cycles: Vec<String> = in_degree
            .into_iter()
            .filter(|(_, degree)| *degree > 0)
            .map(|(module, _)| module)
            .collect();

        if cycles.is_empty() {
            Ok(result)
        } else {
            Err(cycles)
        }
    }

    /// get all dependencies 4 a module
    pub fn get_dependencies(&self, module: &str) -> Option<&Vec<String>> {
        self.dependencies.get(module)
    }

    /// check if a module has any dependencies
    pub fn has_dependencies(&self, module: &str) -> bool {
        self.dependencies
            .get(module)
            .map(|deps| !deps.is_empty())
            .unwrap_or(false)
    }
}

impl Default for ModuleDependencyGraph {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_no_cycle() {
        let mut graph = ModuleDependencyGraph::new();
        graph.add_module("a".to_string());
        graph.add_module("b".to_string());
        graph.add_dependency("a".to_string(), "b".to_string());

        assert!(graph.detect_cycles().is_none());
    }

    #[test]
    fn test_cycle() {
        let mut graph = ModuleDependencyGraph::new();
        graph.add_dependency("a".to_string(), "b".to_string());
        graph.add_dependency("b".to_string(), "a".to_string());

        assert!(graph.detect_cycles().is_some());
    }

    #[test]
    fn test_topological_sort() {
        let mut graph = ModuleDependencyGraph::new();
        graph.add_module("a".to_string());
        graph.add_module("b".to_string());
        graph.add_module("c".to_string());
        graph.add_dependency("a".to_string(), "b".to_string());
        graph.add_dependency("b".to_string(), "c".to_string());

        let order = graph.topological_sort().unwrap();
        let c_pos = order.iter().position(|m| m == "c").unwrap();
        let b_pos = order.iter().position(|m| m == "b").unwrap();
        let a_pos = order.iter().position(|m| m == "a").unwrap();

        assert!(c_pos < b_pos);
        assert!(b_pos < a_pos);
    }
}
