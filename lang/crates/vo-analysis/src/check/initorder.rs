//! Initialization order computation.
//!
//! This module computes the order in which package-level variables
//! must be initialized, detecting and reporting initialization cycles.

use std::collections::{BTreeSet, HashMap, HashSet};

use crate::objects::{DeclInfoKey, ObjKey};

use super::checker::Checker;
use super::errors::TypeError;
use super::resolver::DeclInfo;
use super::type_info::Initializer;

/// Edges in the dependency graph.
#[derive(Debug)]
struct GraphEdges {
    /// Predecessors - objects that depend on this object.
    pred: HashSet<DeclInfoKey>,
    /// Successors - objects this object depends on.
    succ: HashSet<DeclInfoKey>,
}

impl GraphEdges {
    fn new(succ: HashSet<DeclInfoKey>) -> GraphEdges {
        GraphEdges {
            pred: HashSet::new(),
            succ,
        }
    }
}

/// A node in the dependency graph.
#[derive(Debug)]
struct GraphNode {
    /// The declaration this node represents.
    decl: DeclInfoKey,
    /// A source-ordered representative used for diagnostics and stable sorting.
    obj: ObjKey,
    /// Number of dependencies (successors).
    ndeps: usize,
    /// Position for stable sorting.
    pos: usize,
}

impl Checker {
    /// Computes and records the initialization order for package-level variables.
    pub(crate) fn init_order(&mut self) {
        let (mut nodes, edges, representatives) = self.dependency_graph();
        let source_order: HashMap<DeclInfoKey, u32> = representatives
            .iter()
            .map(|(&decl, &obj)| (decl, self.lobj(obj).order()))
            .collect();
        nodes.sort_by_key(|node| self.lobj(node.obj).order());
        let indices: HashMap<_, _> = nodes
            .iter()
            .enumerate()
            .map(|(index, node)| (node.decl, index))
            .collect();
        // Indices follow source order. Reconsider the earliest ready declaration
        // after *each* initializer, including declarations made ready by it.
        let mut ready: BTreeSet<usize> = nodes
            .iter()
            .enumerate()
            .filter_map(|(index, node)| (node.ndeps == 0).then_some(index))
            .collect();
        let mut remaining: BTreeSet<usize> = (0..nodes.len()).collect();
        let mut order = Vec::with_capacity(nodes.len());
        while !remaining.is_empty() {
            let index = ready.pop_first().unwrap_or_else(|| {
                // Continue deterministic error recovery through cyclic graphs.
                // A node outside the cycle can be removed without a diagnostic;
                // a cycle member will subsequently report the cycle itself.
                let index = *remaining.first().unwrap();
                let decl = nodes[index].decl;
                if let Some(cycle) =
                    find_path(&edges, &source_order, decl, decl, &mut HashSet::new())
                {
                    let objects: Vec<_> = cycle.iter().map(|key| representatives[key]).collect();
                    self.report_cycle(&objects);
                }
                index
            });
            remaining.remove(&index);
            let decl = nodes[index].decl;
            order.push(decl);
            for dependent in edges[&decl].pred.iter() {
                let index = indices[dependent];
                if remaining.contains(&index) {
                    nodes[index].ndeps -= 1;
                    if nodes[index].ndeps == 0 {
                        ready.insert(index);
                    }
                }
            }
        }

        // record the init order for variables with initializers only
        let init_order: Vec<Initializer> = order
            .into_iter()
            .filter_map(|decl_key| match &self.decl_info(decl_key) {
                DeclInfo::Var(var) => {
                    if var.rhs.is_empty() {
                        return None;
                    }
                    Some(Initializer::new(var.lhs.clone(), var.rhs.clone()))
                }
                _ => None,
            })
            .collect();
        self.result.record_init_order(init_order);
    }

    /// Returns the object dependency graph from the given obj_map,
    /// with any function nodes removed. The resulting graph contains only constants
    /// and variables.
    fn dependency_graph(
        &self,
    ) -> (
        Vec<GraphNode>,
        HashMap<DeclInfoKey, GraphEdges>,
        HashMap<DeclInfoKey, ObjKey>,
    ) {
        // A multi-variable VarSpec is one graph node. Select the earliest
        // source object as its representative and normalize every dependency
        // through obj_map to the declaration that owns it.
        let mut representatives: HashMap<DeclInfoKey, ObjKey> = HashMap::new();
        for (&obj, &decl) in &self.obj_map {
            if !self.lobj(obj).entity_type().is_dependency() {
                continue;
            }
            representatives
                .entry(decl)
                .and_modify(|current| {
                    if self.lobj(obj).order() < self.lobj(*current).order() {
                        *current = obj;
                    }
                })
                .or_insert(obj);
        }

        let direct_dependencies: HashMap<DeclInfoKey, HashSet<DeclInfoKey>> = representatives
            .keys()
            .copied()
            .map(|decl_key| {
                let deps: HashSet<DeclInfoKey> = self
                    .decl_info(decl_key)
                    .deps()
                    .iter()
                    .filter_map(|obj| self.obj_map.get(obj).copied())
                    .filter(|dep| representatives.contains_key(dep))
                    .collect();
                (decl_key, deps)
            })
            .collect();

        let function_decls: HashSet<DeclInfoKey> = representatives
            .iter()
            .filter_map(|(&decl, &obj)| self.lobj(obj).entity_type().is_func().then_some(decl))
            .collect();

        // Resolve shared function paths once, including mutually recursive
        // groups, before projecting them onto each variable declaration.
        let functions = FunctionDependencies::new(&direct_dependencies, &function_decls);
        let map: HashMap<DeclInfoKey, GraphEdges> = representatives
            .keys()
            .copied()
            .filter(|decl| !function_decls.contains(decl))
            .map(|decl| {
                let deps =
                    resolve_non_function_dependencies(&direct_dependencies, &functions, decl);
                (decl, GraphEdges::new(deps))
            })
            .collect();

        // Build reverse edges separately so graph construction needs no shared
        // interior mutability and every set has one owner.
        let reverse: Vec<_> = map
            .iter()
            .flat_map(|(&decl, node)| node.succ.iter().map(move |&dependency| (dependency, decl)))
            .collect();
        let mut map = map;
        for (dependency, dependent) in reverse {
            if let Some(edge) = map.get_mut(&dependency) {
                edge.pred.insert(dependent);
            }
        }

        // Function-only paths have already been collapsed, so the graph now
        // contains constants and variables exclusively.
        let mut nodes: Vec<GraphNode> = map
            .iter()
            .map(|(decl, node)| {
                let obj = representatives[decl];
                GraphNode {
                    decl: *decl,
                    obj,
                    ndeps: node.succ.len(),
                    pos: self.lobj(obj).pos(),
                }
            })
            .collect();

        nodes.sort_by(|a, b| a.pos.cmp(&b.pos)); // sort by pos
        (nodes, map, representatives)
    }

    /// Reports an initialization cycle error.
    fn report_cycle(&self, cycle: &[ObjKey]) {
        if cycle.is_empty() {
            return;
        }
        let first = cycle[0];
        let o = self.lobj(first);
        self.error_code_msg(
            TypeError::InitCycle,
            self.obj_span(first),
            format!("initialization cycle for {}", o.name()),
        );
        self.error_code_msg(
            TypeError::RefersTo,
            self.obj_span(first),
            format!("\t{} refers to", o.name()),
        );
        for okey in cycle[1..].iter().rev() {
            let o = self.lobj(*okey);
            self.error_code_msg(
                TypeError::RefersTo,
                self.obj_span(*okey),
                format!("\t{} refers to", o.name()),
            );
        }
        let o = self.lobj(first);
        self.error_code_msg(
            TypeError::RefersTo,
            self.obj_span(first),
            format!("\t{}", o.name()),
        );
    }
}

/// Cached non-function dependencies for each function strongly connected
/// component. Every entry member of a recursive group shares the same result.
struct FunctionDependencies {
    components: HashMap<DeclInfoKey, usize>,
    resolved: Vec<HashSet<DeclInfoKey>>,
}

impl FunctionDependencies {
    fn new(
        direct: &HashMap<DeclInfoKey, HashSet<DeclInfoKey>>,
        functions: &HashSet<DeclInfoKey>,
    ) -> Self {
        let mut declarations: Vec<_> = functions.iter().copied().collect();
        declarations.sort_by_key(|decl| decl.raw());
        let indices: HashMap<_, _> = declarations
            .iter()
            .enumerate()
            .map(|(index, &decl)| (decl, index))
            .collect();
        let count = declarations.len();
        let mut edges = vec![Vec::new(); count];
        let mut reverse = vec![Vec::new(); count];
        for (index, decl) in declarations.iter().enumerate() {
            for dependency in direct.get(decl).into_iter().flatten() {
                if let Some(&target) = indices.get(dependency) {
                    edges[index].push(target);
                    reverse[target].push(index);
                }
            }
            edges[index].sort_unstable();
        }

        // Iterative Kosaraju: deep function chains must not consume Rust stack.
        let mut visited = vec![false; count];
        let mut finished = Vec::with_capacity(count);
        for root in 0..count {
            if std::mem::replace(&mut visited[root], true) {
                continue;
            }
            let mut stack = vec![(root, 0)];
            while let Some((node, next)) = stack.last_mut() {
                if let Some(&target) = edges[*node].get(*next) {
                    *next += 1;
                    if !std::mem::replace(&mut visited[target], true) {
                        stack.push((target, 0));
                    }
                } else {
                    finished.push(*node);
                    stack.pop();
                }
            }
        }

        let mut component_of = vec![usize::MAX; count];
        let mut component_count = 0;
        for root in finished.into_iter().rev() {
            if component_of[root] != usize::MAX {
                continue;
            }
            component_of[root] = component_count;
            let mut stack = vec![root];
            while let Some(node) = stack.pop() {
                for &target in &reverse[node] {
                    if component_of[target] == usize::MAX {
                        component_of[target] = component_count;
                        stack.push(target);
                    }
                }
            }
            component_count += 1;
        }

        let mut component_edges = vec![HashSet::new(); component_count];
        let mut resolved = vec![HashSet::new(); component_count];
        for (index, decl) in declarations.iter().enumerate() {
            let component = component_of[index];
            for &dependency in direct.get(decl).into_iter().flatten() {
                if let Some(&target) = indices.get(&dependency) {
                    let target = component_of[target];
                    if target != component {
                        // Kosaraju enumerates components from sources to sinks.
                        debug_assert!(target > component);
                        component_edges[component].insert(target);
                    }
                } else {
                    resolved[component].insert(dependency);
                }
            }
        }
        // Dependencies are already resolved when their dependents are visited.
        for component in (0..component_count).rev() {
            let inherited: HashSet<_> = component_edges[component]
                .iter()
                .flat_map(|&target| resolved[target].iter().copied())
                .collect();
            resolved[component].extend(inherited);
        }
        let components = declarations
            .into_iter()
            .enumerate()
            .map(|(index, decl)| (decl, component_of[index]))
            .collect();
        Self {
            components,
            resolved,
        }
    }
}

/// Stop at the next constant/variable to preserve ordinary topological edges.
/// Shared function subgraphs are read from the component cache, never retraced.
fn resolve_non_function_dependencies(
    direct: &HashMap<DeclInfoKey, HashSet<DeclInfoKey>>,
    functions: &FunctionDependencies,
    from: DeclInfoKey,
) -> HashSet<DeclInfoKey> {
    let mut resolved = HashSet::new();
    for &dependency in direct.get(&from).into_iter().flatten() {
        if let Some(&component) = functions.components.get(&dependency) {
            resolved.extend(functions.resolved[component].iter().copied());
        } else {
            resolved.insert(dependency);
        }
    }
    resolved
}

/// find_path returns the (reversed) list of objects Vec<ObjKey>{to, ... from}
/// such that there is a path of object dependencies from 'from' to 'to'.
/// If there is no such path, the result is None.
fn find_path(
    edges: &HashMap<DeclInfoKey, GraphEdges>,
    source_order: &HashMap<DeclInfoKey, u32>,
    from: DeclInfoKey,
    to: DeclInfoKey,
    visited: &mut HashSet<DeclInfoKey>,
) -> Option<Vec<DeclInfoKey>> {
    if !visited.insert(from) {
        return None;
    }

    struct Frame {
        successors: Vec<DeclInfoKey>,
        next: usize,
    }

    let ordered_successors = |decl: DeclInfoKey| -> Option<Vec<DeclInfoKey>> {
        let mut successors: Vec<DeclInfoKey> = edges.get(&decl)?.succ.iter().copied().collect();
        successors.sort_by_key(|successor| {
            (
                source_order.get(successor).copied().unwrap_or(u32::MAX),
                successor.raw(),
            )
        });
        Some(successors)
    };

    let mut path = vec![from];
    let mut stack = vec![Frame {
        successors: ordered_successors(from)?,
        next: 0,
    }];

    loop {
        let frame = stack.last_mut()?;
        if frame.next == frame.successors.len() {
            stack.pop();
            path.pop();
            continue;
        }
        let successor = frame.successors[frame.next];
        frame.next += 1;

        if successor == to {
            let mut reversed = Vec::with_capacity(path.len());
            reversed.push(to);
            reversed.extend(path.iter().skip(1).rev().copied());
            return Some(reversed);
        }
        if !visited.insert(successor) {
            continue;
        }
        path.push(successor);
        stack.push(Frame {
            successors: ordered_successors(successor).unwrap_or_default(),
            next: 0,
        });
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::arena::ArenaKey;
    use crate::importer::NullImporter;
    use crate::objects::PackageKey;
    use crate::Checker;
    use std::path::PathBuf;
    use vo_syntax::parser;

    fn key(index: usize) -> DeclInfoKey {
        DeclInfoKey::from_usize(index)
    }

    fn graph(successors: Vec<Vec<usize>>) -> HashMap<DeclInfoKey, GraphEdges> {
        successors
            .into_iter()
            .enumerate()
            .map(|(index, successors)| {
                (
                    key(index),
                    GraphEdges::new(successors.into_iter().map(key).collect()),
                )
            })
            .collect()
    }

    #[test]
    fn function_scc_closure_reaches_globals_from_every_entry_member() {
        let via_a = key(0);
        let via_b = key(1);
        let function_a = key(2);
        let function_b = key(3);
        let global_a = key(4);
        let global_b = key(5);
        let direct_dependencies = HashMap::from([
            (function_b, HashSet::from([global_b, function_a])),
            (via_b, HashSet::from([function_b])),
            (global_a, HashSet::new()),
            (function_a, HashSet::from([function_b, global_a])),
            (via_a, HashSet::from([function_a])),
            (global_b, HashSet::new()),
        ]);
        let function_decls = HashSet::from([function_a, function_b]);
        let functions = FunctionDependencies::new(&direct_dependencies, &function_decls);
        let expected = HashSet::from([global_a, global_b]);

        assert_eq!(
            resolve_non_function_dependencies(&direct_dependencies, &functions, via_a),
            expected
        );
        assert_eq!(
            resolve_non_function_dependencies(&direct_dependencies, &functions, via_b),
            expected
        );
    }

    #[test]
    fn explicit_cycle_search_handles_a_deep_chain_with_a_tail_cycle() {
        const NODE_COUNT: usize = 20_000;
        let mut successors = vec![Vec::new(); NODE_COUNT];
        for (index, slot) in successors.iter_mut().enumerate().take(NODE_COUNT - 1) {
            slot.push(index + 1);
        }
        successors[NODE_COUNT - 1].push(NODE_COUNT - 2);
        let edges = graph(successors);
        let source_order: HashMap<DeclInfoKey, u32> = (0..NODE_COUNT)
            .map(|index| (key(index), index as u32))
            .collect();

        assert_eq!(
            find_path(&edges, &source_order, key(0), key(0), &mut HashSet::new(),),
            None,
            "the chain head is outside the tail cycle"
        );
        assert_eq!(
            find_path(
                &edges,
                &source_order,
                key(NODE_COUNT - 2),
                key(NODE_COUNT - 2),
                &mut HashSet::new(),
            ),
            Some(vec![key(NODE_COUNT - 2), key(NODE_COUNT - 1)])
        );
    }

    #[test]
    fn cycle_search_chooses_successors_by_source_order() {
        let edges = graph(vec![vec![1, 2], vec![0], vec![0]]);
        let source_order = HashMap::from([(key(0), 0), (key(1), 20), (key(2), 10)]);

        assert_eq!(
            find_path(&edges, &source_order, key(0), key(0), &mut HashSet::new(),),
            Some(vec![key(0), key(2)])
        );
    }

    #[test]
    fn package_initializers_are_stable_across_fresh_hash_states() {
        let source = r#"
package deterministic

func readBase() int { return base }
var late = readBase()
var independent = 1
var base = 2
var pairA, pairB = makePair()
func makePair() (int, int) { return late, independent }
"#;
        let expected = vec![
            vec!["independent".to_string()],
            vec!["base".to_string()],
            vec!["late".to_string()],
            vec!["pairA".to_string(), "pairB".to_string()],
        ];

        for _ in 0..16 {
            let (file, parse_diagnostics, interner) = parser::parse(source, 0);
            assert!(
                !parse_diagnostics.has_errors(),
                "parse diagnostics: {parse_diagnostics:?}"
            );
            let mut checker = Checker::new_with_trace(PackageKey::null(), interner, false);
            let package = checker.tc_objs.new_package(
                "github.com/acme/deterministic".to_string(),
                "github.com/acme/deterministic".to_string(),
            );
            checker.pkg = package;
            let mut importer = NullImporter::new(PathBuf::from("."));
            assert!(
                checker
                    .check_with_importer(std::slice::from_ref(&file), &mut importer)
                    .is_ok(),
                "check diagnostics: {:?}",
                checker.diagnostics.borrow().diagnostics()
            );

            let actual: Vec<Vec<String>> = checker
                .result
                .init_order
                .iter()
                .map(|initializer| {
                    initializer
                        .lhs
                        .iter()
                        .map(|&object| checker.lobj(object).name().to_string())
                        .collect()
                })
                .collect();
            assert_eq!(actual, expected);
        }
    }

    #[test]
    fn cached_function_components_match_reachability_for_all_three_node_graphs() {
        // Exhaust all function edges, including self/mutual recursion. Compare
        // against a direct walk that does not use SCCs or memoization.
        let functions = HashSet::from([key(0), key(1), key(2)]);
        for mask in 0u16..512 {
            let mut direct = HashMap::new();
            for node in 0..3 {
                let mut deps = HashSet::from([key(3 + node)]);
                for target in 0..3 {
                    if mask & (1 << (node * 3 + target)) != 0 {
                        deps.insert(key(target));
                    }
                }
                direct.insert(key(node), deps);
                direct.insert(key(6 + node), HashSet::from([key(node)]));
            }
            let cached = FunctionDependencies::new(&direct, &functions);
            for entry in 0..3 {
                let mut expected = HashSet::new();
                let mut visited = HashSet::new();
                let mut pending = vec![key(entry)];
                while let Some(node) = pending.pop() {
                    if functions.contains(&node) {
                        if visited.insert(node) {
                            pending.extend(direct[&node].iter().copied());
                        }
                    } else {
                        expected.insert(node);
                    }
                }
                assert_eq!(
                    resolve_non_function_dependencies(&direct, &cached, key(6 + entry)),
                    expected,
                    "graph {mask}, entry {entry}"
                );
            }
        }
    }

    #[test]
    fn shared_deep_function_chain_is_resolved_without_recursion() {
        const COUNT: usize = 20_000;
        let functions = (0..COUNT).map(key).collect();
        let mut direct: HashMap<_, _> = (0..COUNT)
            .map(|index| (key(index), HashSet::from([key(index + 1)])))
            .collect();
        let cached = FunctionDependencies::new(&direct, &functions);
        for index in 0..COUNT {
            direct.insert(key(COUNT + index + 1), HashSet::from([key(0)]));
        }
        for index in 0..COUNT {
            assert_eq!(
                resolve_non_function_dependencies(&direct, &cached, key(COUNT + index + 1)),
                HashSet::from([key(COUNT)])
            );
        }
    }
}
