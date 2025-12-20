//! Initialization order computation.
//!
//! This module computes the order in which package-level variables
//! must be initialized, detecting and reporting initialization cycles.
//!
//! TODO: Full implementation requires DeclInfo.deps() and proper integration
//! with the resolver. Currently provides stub implementation.

#![allow(dead_code)]

use std::collections::{HashMap, HashSet};

use gox_common::vfs::FileSystem;

use crate::objects::ObjKey;

use super::checker::Checker;

/// A node in the dependency graph.
#[derive(Debug)]
struct GraphNode {
    /// The object this node represents.
    obj: ObjKey,
    /// Number of dependencies (successors).
    ndeps: usize,
    /// Position for stable sorting.
    pos: usize,
}

/// Edges in the dependency graph.
#[derive(Debug, Default)]
struct GraphEdges {
    /// Predecessors - objects that depend on this object.
    pred: HashSet<ObjKey>,
    /// Successors - objects this object depends on.
    succ: HashSet<ObjKey>,
}

impl<F: FileSystem> Checker<F> {
    /// Computes and records the initialization order for package-level variables.
    /// 
    /// This is a stub implementation. Full implementation requires:
    /// - DeclInfo.deps() method for dependency tracking
    /// - Integration with resolver for init expression IDs
    pub fn init_order(&mut self) {
        // TODO: Implement full initialization order computation
        // For now, just check for obvious cycles in obj_map
        
        let order = self.compute_init_order();
        
        // Report any cycles found
        for cycle in &order.cycles {
            self.report_cycle(cycle);
        }
    }

    /// Computes initialization order, detecting cycles.
    fn compute_init_order(&self) -> InitOrderResult {
        let mut result = InitOrderResult::default();
        
        // Collect all package-level vars/consts
        let mut vars: Vec<ObjKey> = self.obj_map.keys()
            .filter(|&&obj| {
                let lobj = &self.tc_objs.lobjs[obj];
                lobj.entity_type().is_const() || lobj.entity_type().is_var()
            })
            .copied()
            .collect();
        
        // Sort by declaration order
        vars.sort_by(|a, b| {
            let a_order = self.tc_objs.lobjs[*a].order();
            let b_order = self.tc_objs.lobjs[*b].order();
            a_order.cmp(&b_order)
        });
        
        result.order = vars;
        result
    }

    /// Reports an initialization cycle error.
    fn report_cycle(&self, cycle: &[ObjKey]) {
        if cycle.is_empty() {
            return;
        }

        let first = &self.tc_objs.lobjs[cycle[0]];
        let mut msg = format!("initialization cycle for {}", first.name());

        // Add cycle path
        for (i, &obj) in cycle.iter().enumerate() {
            let lobj = &self.tc_objs.lobjs[obj];
            if i > 0 {
                msg.push_str(&format!("\n\t{} refers to", lobj.name()));
            }
            if i == cycle.len() - 1 && cycle.len() > 1 {
                let first_name = &self.tc_objs.lobjs[cycle[0]].name();
                msg.push_str(&format!("\n\t{}", first_name));
            }
        }

        self.error(gox_common::span::Span::default(), msg);
    }
}

/// Result of initialization order computation.
#[derive(Debug, Default)]
struct InitOrderResult {
    /// Objects in initialization order.
    order: Vec<ObjKey>,
    /// Detected cycles (each cycle is a list of objects).
    cycles: Vec<Vec<ObjKey>>,
}
