//! Initialization order computation.
//!
//! This module computes the order in which package-level variables
//! must be initialized, detecting and reporting initialization cycles.

use std::cell::RefCell;
use std::collections::{HashMap, HashSet};
use std::rc::Rc;

use crate::objects::{DeclInfoKey, ObjKey};

use super::checker::Checker;
use super::errors::TypeError;
use super::resolver::DeclInfo;
use super::type_info::Initializer;

/// Edges in the dependency graph.
#[derive(Debug)]
struct GraphEdges {
    /// Predecessors - objects that depend on this object.
    pred: Rc<RefCell<HashSet<DeclInfoKey>>>,
    /// Successors - objects this object depends on.
    succ: Rc<RefCell<HashSet<DeclInfoKey>>>,
}

impl GraphEdges {
    fn new(succ: Rc<RefCell<HashSet<DeclInfoKey>>>) -> GraphEdges {
        GraphEdges {
            pred: Rc::new(RefCell::new(HashSet::new())),
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
        nodes.sort_by_key(|node| (node.ndeps, self.lobj(node.obj).order()));
        let len = nodes.len();
        let mut nodes = &mut nodes[0..len];
        let mut order: Vec<DeclInfoKey> = vec![];

        loop {
            if nodes.is_empty() {
                break;
            }
            let mut first_dependant = nodes
                .iter()
                .enumerate()
                .find(|(_, n)| n.ndeps > 0)
                .map_or(nodes.len(), |(i, _)| i);

            if first_dependant == 0 {
                // we have a cycle with the first node
                let visited = &mut HashSet::new();
                let decl = nodes[0].decl;
                // If decl is not part of the cycle (e.g., a->b->c->d->c),
                // cycle will be None. Don't report anything in that case since
                // the cycle is reported when the algorithm gets to an object
                // in the cycle.
                // Furthermore, once an object in the cycle is encountered,
                // the cycle will be broken (dependency count will be reduced
                // below), and so the remaining nodes in the cycle don't trigger
                // another error (unless they are part of multiple cycles).
                if let Some(cycle) = find_path(&edges, &source_order, decl, decl, visited) {
                    let objects: Vec<ObjKey> = cycle
                        .into_iter()
                        .filter_map(|key| representatives.get(&key).copied())
                        .collect();
                    self.report_cycle(&objects);
                }
                // Ok to continue, but the variable initialization order
                // will be incorrect at this point since it assumes no
                // cycle errors.
                // set first_dependant to 1 to remove the first node,
                first_dependant = 1;
            }

            let mut indep: Vec<&GraphNode> = nodes[0..first_dependant].iter().collect();
            indep.sort_by_key(|node| self.lobj(node.obj).order());
            let mut indep: Vec<DeclInfoKey> = indep.into_iter().map(|node| node.decl).collect();
            order.append(&mut indep);

            // reduce dependency count of all dependent nodes
            let to_sub: HashMap<DeclInfoKey, usize> =
                nodes[0..first_dependant]
                    .iter()
                    .fold(HashMap::new(), |mut init, x| {
                        for p in edges[&x.decl].pred.borrow().iter() {
                            *init.entry(*p).or_insert(0) += 1;
                        }
                        init
                    });
            // remove resolved nodes
            nodes = &mut nodes[first_dependant..];
            for n in nodes.iter_mut() {
                n.ndeps -= *to_sub.get(&n.decl).unwrap_or(&0);
            }
            // sort nodes, should be fast as it's almost sorted
            nodes.sort_by_key(|node| (node.ndeps, self.lobj(node.obj).order()));
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

        // Collapse every function-only path into direct declaration edges. A
        // function SCC may be reached through any member, so resolving each
        // non-function declaration against the immutable direct graph makes
        // the result independent of HashMap iteration and function order.
        let map: HashMap<DeclInfoKey, GraphEdges> = representatives
            .keys()
            .copied()
            .filter(|decl| !function_decls.contains(decl))
            .map(|decl| {
                let deps =
                    resolve_non_function_dependencies(&direct_dependencies, &function_decls, decl);
                (decl, GraphEdges::new(Rc::new(RefCell::new(deps))))
            })
            .collect();

        // add the edges for the other direction
        for (decl, node) in map.iter() {
            for s in node.succ.borrow().iter() {
                if let Some(edge) = map.get(s) {
                    edge.pred.borrow_mut().insert(*decl);
                }
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
                    ndeps: node.succ.borrow().len(),
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

/// Resolve the direct dependencies of a constant/variable declaration through
/// any number of function declarations. Traversal stops at the next
/// constant/variable so ordinary graph edges retain their topological meaning.
fn resolve_non_function_dependencies(
    direct_dependencies: &HashMap<DeclInfoKey, HashSet<DeclInfoKey>>,
    function_decls: &HashSet<DeclInfoKey>,
    from: DeclInfoKey,
) -> HashSet<DeclInfoKey> {
    let mut resolved = HashSet::new();
    let mut visited_functions = HashSet::new();
    let mut pending: Vec<DeclInfoKey> = direct_dependencies
        .get(&from)
        .into_iter()
        .flat_map(|deps| deps.iter().copied())
        .collect();

    while let Some(decl) = pending.pop() {
        if !function_decls.contains(&decl) {
            resolved.insert(decl);
            continue;
        }
        if !visited_functions.insert(decl) {
            continue;
        }
        if let Some(deps) = direct_dependencies.get(&decl) {
            pending.extend(deps.iter().copied());
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
        let mut successors: Vec<DeclInfoKey> =
            edges.get(&decl)?.succ.borrow().iter().copied().collect();
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
                    GraphEdges::new(Rc::new(RefCell::new(
                        successors.into_iter().map(key).collect(),
                    ))),
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
        let expected = HashSet::from([global_a, global_b]);

        assert_eq!(
            resolve_non_function_dependencies(&direct_dependencies, &function_decls, via_a),
            expected
        );
        assert_eq!(
            resolve_non_function_dependencies(&direct_dependencies, &function_decls, via_b),
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
}
