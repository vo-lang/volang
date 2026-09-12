use super::*;

/// Resolve emitted labels using Wasm's block/loop branch rules, independently
/// of the CFG interval construction. Every branch must land on the same guest
/// block, which also preserves the sequence of per-block fuel polls.
fn assert_same_edges(edges: &[Vec<usize>], plan: &StructuredControlPlan) {
    let mut closes = vec![0; plan.events.len()];
    let mut stack = Vec::new();
    for (index, event) in plan.events.iter().enumerate() {
        match event {
            Event::Open(_) => stack.push(index),
            Event::Close => closes[stack.pop().unwrap()] = index,
            Event::Block(_) => {}
        }
    }
    assert!(stack.is_empty());
    let mut labels = Vec::new();
    for (index, event) in plan.events.iter().enumerate() {
        match *event {
            Event::Open(label) => {
                stack.push(index);
                labels.push(label);
            }
            Event::Close => {
                stack.pop().unwrap();
                labels.pop().unwrap();
            }
            Event::Block(source) => {
                for &target in &edges[source as usize] {
                    let landing = if target == source as usize + 1 {
                        index + 1
                    } else {
                        let depth = label_depth(&labels, target as u32).unwrap() as usize;
                        let label_index = stack[stack.len() - 1 - depth];
                        match plan.events[label_index] {
                            Event::Open(Label::Loop(_)) => label_index + 1,
                            Event::Open(Label::Forward(_)) => closes[label_index] + 1,
                            _ => unreachable!(),
                        }
                    };
                    let actual = plan.events[landing..].iter().find_map(|event| match event {
                        Event::Block(block) => Some(*block as usize),
                        _ => None,
                    });
                    assert_eq!(
                        actual,
                        Some(target),
                        "edge {source}->{target}: {:?}",
                        plan.events
                    );
                }
            }
        }
    }
}

#[test]
fn nested_loops_break_continue_and_multiple_latches_preserve_edges() {
    let edges = vec![
        vec![1],
        vec![8, 2],
        vec![3],
        vec![6, 4],
        vec![7, 5],
        vec![3, 6],
        vec![1, 7],
        vec![1, 8],
        vec![],
    ];
    let plan = StructuredControlPlan::for_edges(&edges).expect("nested lexical loops");
    assert_same_edges(&edges, &plan);
    assert_eq!(
        plan.events
            .iter()
            .filter(|event| matches!(event, Event::Open(Label::Loop(_))))
            .count(),
        2
    );
}

#[test]
fn self_loop_and_unreachable_blocks_preserve_edges() {
    for edges in [vec![vec![0, 1], vec![]], vec![vec![], vec![1, 2], vec![]]] {
        assert_same_edges(&edges, &StructuredControlPlan::for_edges(&edges).unwrap());
    }
}

#[test]
fn crossing_loops_and_external_interior_entries_fall_back() {
    for edges in [
        vec![vec![1], vec![2], vec![0, 3], vec![1]],
        vec![vec![2, 1], vec![2], vec![1, 3], vec![]],
    ] {
        assert!(StructuredControlPlan::for_edges(&edges).is_none());
    }
}

#[test]
fn analysis_and_wasm_nesting_are_bounded() {
    assert!(StructuredControlPlan::for_edges(&vec![vec![]; MAX_BLOCKS + 1]).is_none());
    // This CFG fits the block budget but its forward-label nesting exceeds
    // the structured emitter budget. Keep the existing dispatcher available.
    let count = MAX_LABEL_DEPTH + 3;
    let edges = (0..count)
        .map(|i| {
            if i + 2 < count {
                vec![i + 2, i + 1]
            } else {
                vec![]
            }
        })
        .collect::<Vec<_>>();
    assert!(StructuredControlPlan::for_edges(&edges).is_none());
    assert!(StructuredControlPlan::for_edges(&[vec![1]]).is_none());
    let sequential = (0..MAX_BLOCKS)
        .map(|i| {
            if i + 1 < MAX_BLOCKS {
                vec![i + 1]
            } else {
                vec![]
            }
        })
        .collect::<Vec<_>>();
    let plan = StructuredControlPlan::for_edges(&sequential).unwrap();
    assert!(
        plan.events
            .iter()
            .all(|event| matches!(event, Event::Block(_))),
        "natural fallthrough needs no label or dispatcher"
    );
}

#[test]
fn all_small_accepted_cfgs_have_identical_wasm_branch_destinations() {
    fn enumerate(
        edges: &mut [Vec<usize>],
        source: usize,
        accepted: &mut usize,
        rejected: &mut usize,
    ) {
        if source == edges.len() {
            if let Some(plan) = StructuredControlPlan::for_edges(edges) {
                assert_same_edges(edges, &plan);
                *accepted += 1;
            } else {
                *rejected += 1;
            }
            return;
        }
        edges[source].clear();
        enumerate(edges, source + 1, accepted, rejected);
        for target in 0..edges.len() {
            edges[source] = vec![target];
            enumerate(edges, source + 1, accepted, rejected);
            if source + 1 < edges.len() {
                edges[source] = vec![target, source + 1];
                enumerate(edges, source + 1, accepted, rejected);
            }
        }
    }
    let (mut accepted, mut rejected) = (0, 0);
    for size in 1..=4 {
        enumerate(&mut vec![vec![]; size], 0, &mut accepted, &mut rejected);
    }
    assert!(accepted > 1000);
    assert!(rejected > 1000);
}
