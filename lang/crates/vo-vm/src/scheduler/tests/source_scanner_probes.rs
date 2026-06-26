use super::*;

#[test]
fn vm_scheduler_wake_boundary_013_scanner_covers_sender_closed_ufcs_and_queue_mutation_062() {
    let mutation_patterns = scheduler_raw_mutation_patterns_062();
    let probe = r#"
            fn unauthorized(
                scheduler: &mut Scheduler,
                waiter: &QueueWaiter,
                fid: FiberId,
            ) {
                Scheduler::try_wake_fiber(scheduler, fid);
                scheduler.try_wake_fiber /* token whitespace */ (fid);
                Scheduler::wake_queue_waiter /* token whitespace */ (scheduler, waiter);
                scheduler
                    .wake_queue_sender_closed(waiter)
                    .unwrap();
                scheduler.ready_queue.push_back(fid);
                scheduler.ready_queue = VecDeque::new();
                scheduler.ready_queue.extend([fid]);
                scheduler.ready_queue.clear();
                scheduler.ready_queue.retain(|ready| *ready != fid);
                VecDeque::push_back(&mut scheduler.ready_queue, fid);
                call_sched!(scheduler, try_wake_fiber, fid);
                mutate_queue!(scheduler, ready_queue, fid);
                call_sched!(scheduler, r#try_wake_fiber, fid);
                mutate_queue!(scheduler, r#ready_queue, fid);
            }
        "#;

    assert_eq!(
        compact_pattern_occurrences(probe, "Scheduler::try_wake_fiber(").len(),
        1,
        "scanner must catch UFCS raw wake applier calls"
    );
    assert_eq!(
        compact_pattern_occurrences(probe, ".try_wake_fiber(").len(),
        1,
        "scanner must treat comments between method name and call open as token whitespace"
    );
    assert_eq!(
        compact_pattern_occurrences(probe, "Scheduler::wake_queue_waiter(").len(),
        1,
        "scanner must treat comments between UFCS function name and call open as token whitespace"
    );
    assert_eq!(
        compact_pattern_occurrences(probe, ".wake_queue_sender_closed(").len(),
        1,
        "scanner must catch multi-line closed-sender wake calls"
    );
    assert_eq!(
        compact_pattern_occurrences(probe, ".ready_queue.push_back(").len(),
        1,
        "scanner must catch direct run-queue mutation outside scheduler.rs"
    );
    assert_eq!(
        compact_pattern_occurrences(probe, ".ready_queue=").len(),
        1,
        "scanner must catch direct run-queue replacement outside scheduler.rs"
    );
    let owns_ready_queue = mutation_patterns
        .iter()
        .any(|pattern| pattern.compact == ".ready_queue");
    assert!(
        owns_ready_queue,
        "ownership gate must treat direct run-queue field access as scheduler-owned"
    );
    assert_eq!(
        compact_pattern_occurrences(probe, ".ready_queue.extend(").len(),
        1,
        "scanner must catch direct run-queue extension outside scheduler.rs"
    );
    assert_eq!(
        compact_pattern_occurrences(probe, ".ready_queue.clear(").len(),
        1,
        "scanner must catch direct run-queue clearing outside scheduler.rs"
    );
    assert_eq!(
        compact_pattern_occurrences(probe, ".ready_queue.retain(").len(),
        1,
        "scanner must catch direct run-queue filtering outside scheduler.rs"
    );
    assert_eq!(
        compact_pattern_occurrences(probe, "VecDeque::push_back(&mutscheduler.ready_queue").len(),
        1,
        "scanner must catch UFCS run-queue mutation outside scheduler.rs"
    );
    assert!(
        owns_ready_queue,
        "ownership gate must catch UFCS run-queue mutation through direct field ownership"
    );

    let macro_occurrences = scheduler_macro_forwarded_mutation_occurrences_062(probe);
    assert_eq!(
            macro_occurrences.len(),
            4,
            "scanner must catch macro-forwarded raw and raw-identifier scheduler method and run-queue identifiers"
        );
}

#[test]
fn vm_scheduler_wake_boundary_013_scanner_covers_scheduler_alias_and_function_item_062() {
    let probe = r#"
            use crate::scheduler::Scheduler as S;

            fn unauthorized(scheduler: &mut Scheduler, fid: FiberId) {
                S::try_wake_fiber(scheduler, fid);
                S::r#try_wake_fiber(scheduler, fid);
                let wake = Scheduler::try_wake_fiber;
                let aliased_wake = S::try_wake_fiber;
                scheduler.try_wake_fiber(fid);
                scheduler.r#try_wake_fiber(fid);
                wake(scheduler, fid);
                aliased_wake(scheduler, fid);
            }
        "#;

    assert_eq!(
            scheduler_raw_wake_source_occurrences_062(probe).len(),
            6,
            "scanner must reserve raw scheduler wake method identifiers across aliases and function items"
        );
}

#[test]
fn vm_scheduler_wake_boundary_013_scanner_rejects_non_applier_runtime_helpers_062() {
    let probe = r#"
            impl Vm {
                fn apply_runtime_command(&mut self, fid: FiberId) {
                    self.scheduler.try_wake_fiber(fid);
                }
            }

            fn apply_runtime_command(&mut self, fid: FiberId) {
                self.scheduler.try_wake_fiber(fid);
            }

            impl Probe {
                fn apply_runtime_command(&mut self, fid: FiberId) {
                    self.scheduler.try_wake_fiber(fid);
                }
            }

            trait RuntimeWakeProbe {
                fn apply_runtime_command(&mut self, fid: FiberId);
            }

            impl RuntimeWakeProbe for Vm {
                fn apply_runtime_command(&mut self, fid: FiberId) {
                    self.scheduler.try_wake_fiber(fid);
                }
            }

            mod boundary_owner_spoof {
                struct Vm;

                impl Vm {
                    fn apply_runtime_command(real: &mut super::Vm, fid: super::FiberId) {
                        real.scheduler.try_wake_fiber(fid);
                    }
                }

                struct VmState;

                impl VmState {
                    fn wake_waiter(scheduler: &mut super::Scheduler, waiter: &super::QueueWaiter) {
                        scheduler.wake_queue_waiter(waiter);
                    }
                }
            }

            trait VmStateWakeProbe {
                fn wake_waiter(&mut self, scheduler: &mut Scheduler, waiter: &QueueWaiter);
            }

            impl VmStateWakeProbe for VmState {
                fn wake_waiter(&mut self, scheduler: &mut Scheduler, waiter: &QueueWaiter) {
                    scheduler.wake_queue_waiter(waiter);
                }
            }
        "#;
    let occurrences = compact_pattern_occurrences(probe, ".try_wake_fiber(");
    assert_eq!(occurrences.len(), 5);
    let waiter_occurrences = compact_pattern_occurrences(probe, ".wake_queue_waiter(");
    assert_eq!(waiter_occurrences.len(), 2);

    assert!(scheduler_raw_wake_call_site_allowed_062(
        "src/runtime_boundary.rs",
        probe,
        occurrences[0]
    ));
    assert!(
        !scheduler_raw_wake_call_site_allowed_062("src/runtime_boundary.rs", probe, occurrences[1]),
        "runtime_boundary.rs free helpers must not inherit applier authority by name"
    );
    assert!(
        !scheduler_raw_wake_call_site_allowed_062("src/runtime_boundary.rs", probe, occurrences[2]),
        "runtime_boundary.rs non-Vm impls must not inherit applier authority by name"
    );
    assert!(
        !scheduler_raw_wake_call_site_allowed_062("src/runtime_boundary.rs", probe, occurrences[3]),
        "trait impls for Vm must not inherit inherent runtime-boundary authority by name"
    );
    assert!(
        !scheduler_raw_wake_call_site_allowed_062("src/runtime_boundary.rs", probe, occurrences[4]),
        "nested shadow Vm modules must not inherit runtime-boundary authority by name"
    );
    assert!(
        !scheduler_raw_wake_call_site_allowed_062("src/vm/types.rs", probe, waiter_occurrences[0]),
        "trait impls for VmState must not inherit inherent bridge authority by name"
    );
    assert!(
        !scheduler_raw_wake_call_site_allowed_062("src/vm/types.rs", probe, waiter_occurrences[1]),
        "nested shadow VmState modules must not inherit VmState bridge authority by name"
    );
}

#[test]
fn vm_scheduler_wake_boundary_013_scanner_rejects_scheduler_file_free_helpers_062() {
    let probe = r#"
            pub(crate) fn raw_wake_alias(scheduler: &mut Scheduler, fid: FiberId) {
                scheduler.try_wake_fiber(fid);
            }

            use crate::scheduler::Scheduler as S;
            use crate::{scheduler::Scheduler as GroupedS};
            use crate::{scheduler::{Scheduler as NestedGroupedS}};
            use crate::{fiber::FiberId, scheduler::Scheduler as LateGroupedS};
            use crate::{fiber::FiberId, scheduler::{Scheduler as LateNestedGroupedS}};
            use crate::scheduler::r#Scheduler as RawSchedulerAlias;
            use crate::scheduler as sched;
            use crate::scheduler::{self as grouped_sched};
            use crate::{fiber::RawFiberId, scheduler as late_sched};
            use crate::{fiber::RawFiberId, scheduler::{self as late_grouped_sched}};
            use crate::{fiber::RawFiberId as HiddenRawFiberId, scheduler as hidden_sched};
            use self::sched::Scheduler as ModuleImportS;
            use self::sched::{Scheduler as GroupedModuleImportS};
            use self::sched::{FiberWakeKey as HiddenWakeKey, Scheduler as TailGroupedModuleImportS};
            use self::sched::{self as realiased_sched_group};
            use self::{sched::{self as nested_realiased_sched_group}};
            type TypeS = crate::scheduler::Scheduler;
            type RawModuleOwner = crate::r#scheduler::Scheduler;
            type ParenthesizedOwner = (crate::scheduler::Scheduler);
            type RelativeModuleOwner = super::scheduler::Scheduler;
            type WhereOwner where crate::scheduler::Scheduler: Sized = crate::scheduler::Scheduler;
            type r#RawTypeS = Scheduler;
            type SelfS = self::Scheduler;
            type SuperS = super::Scheduler;
            type RunQueueOwner = sched::Scheduler;
            type GroupedRunQueueOwner = grouped_sched::Scheduler;
            type LateRunQueueOwner = late_sched::Scheduler;
            type LateGroupedRunQueueOwner = late_grouped_sched::Scheduler;
            type ChainedRunQueueOwner = RunQueueOwner;
            type BaseRunQueueSource = crate::scheduler::Scheduler;
            type HiddenRunQueueHolder = BaseRunQueueSource;
            type HiddenModuleOwner = hidden_sched::Scheduler;
            type SelfHiddenModuleOwner = self::hidden_sched::Scheduler;
            use self::hidden_sched as hidden_sched_realias;
            type RealiasedOwner = hidden_sched_realias::Scheduler;
            type RealiasedGroupedOwner = realiased_sched_group::Scheduler;
            type NestedRealiasedGroupedOwner = nested_realiased_sched_group::Scheduler;

            pub(crate) fn raw_ready_queue_alias(scheduler: &mut Scheduler, fid: FiberId) {
                let Scheduler { ready_queue, .. } = scheduler;
                ready_queue.push_back(fid);
            }

            pub(crate) fn shorthand_ready_queue_clear(scheduler: &mut Scheduler) {
                let Scheduler { ready_queue, .. } = scheduler;
                ready_queue.clear();
            }

            pub(crate) fn shorthand_ready_queue_ufcs(scheduler: &mut Scheduler, fid: FiberId) {
                let Scheduler { ready_queue, .. } = scheduler;
                VecDeque::push_back(ready_queue, fid);
            }

            pub(crate) fn ref_mut_ready_queue_clear(scheduler: &mut Scheduler) {
                let Scheduler { ref mut ready_queue, .. } = *scheduler;
                ready_queue.clear();
            }

            pub(crate) fn ref_mut_ready_queue_ufcs(scheduler: &mut Scheduler, fid: FiberId) {
                let Scheduler { ref mut ready_queue, .. } = *scheduler;
                VecDeque::push_back(ready_queue, fid);
            }

            pub(crate) fn aliased_ref_mut_ready_queue_clear(scheduler: &mut Scheduler) {
                let S { ref mut ready_queue, .. } = *scheduler;
                ready_queue.clear();
            }

            pub(crate) fn grouped_aliased_ref_mut_ready_queue_clear(scheduler: &mut Scheduler) {
                let GroupedS { ref mut ready_queue, .. } = *scheduler;
                ready_queue.clear();
            }

            pub(crate) fn nested_grouped_aliased_ref_mut_ready_queue_clear(scheduler: &mut Scheduler) {
                let NestedGroupedS { ref mut ready_queue, .. } = *scheduler;
                ready_queue.clear();
            }

            pub(crate) fn late_grouped_aliased_ref_mut_ready_queue_clear(scheduler: &mut Scheduler) {
                let LateGroupedS { ref mut ready_queue, .. } = *scheduler;
                ready_queue.clear();
            }

            pub(crate) fn late_nested_grouped_aliased_ref_mut_ready_queue_clear(scheduler: &mut Scheduler) {
                let LateNestedGroupedS { ref mut ready_queue, .. } = *scheduler;
                ready_queue.clear();
            }

            pub(crate) fn raw_ident_aliased_ref_mut_ready_queue_clear(scheduler: &mut Scheduler) {
                let RawSchedulerAlias { ref mut ready_queue, .. } = *scheduler;
                ready_queue.clear();
            }

            pub(crate) fn module_import_aliased_ref_mut_ready_queue_clear(scheduler: &mut sched::Scheduler) {
                let ModuleImportS { ref mut ready_queue, .. } = *scheduler;
                ready_queue.clear();
            }

            pub(crate) fn grouped_module_import_aliased_ref_mut_ready_queue_clear(scheduler: &mut sched::Scheduler) {
                let GroupedModuleImportS { ref mut ready_queue, .. } = *scheduler;
                ready_queue.clear();
            }

            pub(crate) fn tail_grouped_module_import_aliased_ref_mut_ready_queue_clear(scheduler: &mut sched::Scheduler) {
                let TailGroupedModuleImportS { ref mut ready_queue, .. } = *scheduler;
                ready_queue.clear();
            }

            pub(crate) fn type_aliased_ref_mut_ready_queue_clear(scheduler: &mut Scheduler) {
                let TypeS { ref mut ready_queue, .. } = *scheduler;
                ready_queue.clear();
            }

            pub(crate) fn raw_module_type_aliased_ref_mut_ready_queue_clear(scheduler: &mut Scheduler) {
                let RawModuleOwner { ref mut ready_queue, .. } = *scheduler;
                ready_queue.clear();
            }

            pub(crate) fn parenthesized_type_aliased_ref_mut_ready_queue_clear(scheduler: &mut Scheduler) {
                let ParenthesizedOwner { ref mut ready_queue, .. } = *scheduler;
                ready_queue.clear();
            }

            pub(crate) fn relative_module_type_aliased_ref_mut_ready_queue_clear(scheduler: &mut Scheduler) {
                let RelativeModuleOwner { ref mut ready_queue, .. } = *scheduler;
                ready_queue.clear();
            }

            pub(crate) fn where_clause_type_aliased_ref_mut_ready_queue_clear(scheduler: &mut Scheduler) {
                let WhereOwner { ref mut ready_queue, .. } = *scheduler;
                ready_queue.clear();
            }

            pub(crate) fn raw_type_aliased_ref_mut_ready_queue_clear(scheduler: &mut Scheduler) {
                let r#RawTypeS { ref mut ready_queue, .. } = *scheduler;
                ready_queue.clear();
            }

            pub(crate) fn self_type_aliased_ref_mut_ready_queue_clear(scheduler: &mut Scheduler) {
                let SelfS { ref mut ready_queue, .. } = *scheduler;
                ready_queue.clear();
            }

            pub(crate) fn super_type_aliased_ref_mut_ready_queue_clear(scheduler: &mut Scheduler) {
                let SuperS { ref mut ready_queue, .. } = *scheduler;
                ready_queue.clear();
            }

            pub(crate) fn module_alias_type_aliased_ref_mut_ready_queue_clear(scheduler: &mut sched::Scheduler) {
                let RunQueueOwner { ref mut ready_queue, .. } = *scheduler;
                ready_queue.clear();
            }

            pub(crate) fn grouped_self_module_alias_type_aliased_ref_mut_ready_queue_clear(scheduler: &mut grouped_sched::Scheduler) {
                let GroupedRunQueueOwner { ref mut ready_queue, .. } = *scheduler;
                ready_queue.clear();
            }

            pub(crate) fn late_module_alias_type_aliased_ref_mut_ready_queue_clear(scheduler: &mut late_sched::Scheduler) {
                let LateRunQueueOwner { ref mut ready_queue, .. } = *scheduler;
                ready_queue.clear();
            }

            pub(crate) fn late_grouped_self_module_alias_type_aliased_ref_mut_ready_queue_clear(scheduler: &mut late_grouped_sched::Scheduler) {
                let LateGroupedRunQueueOwner { ref mut ready_queue, .. } = *scheduler;
                ready_queue.clear();
            }

            pub(crate) fn chained_module_alias_type_aliased_ref_mut_ready_queue_clear(scheduler: &mut sched::Scheduler) {
                let ChainedRunQueueOwner { ref mut ready_queue, .. } = *scheduler;
                ready_queue.clear();
            }

            pub(crate) fn non_suffix_chained_type_aliased_ref_mut_ready_queue_clear(scheduler: &mut Scheduler) {
                let HiddenRunQueueHolder { ref mut ready_queue, .. } = *scheduler;
                ready_queue.clear();
            }

            pub(crate) fn late_hidden_module_alias_type_aliased_ref_mut_ready_queue_clear(scheduler: &mut hidden_sched::Scheduler) {
                let HiddenModuleOwner { ref mut ready_queue, .. } = *scheduler;
                ready_queue.clear();
            }

            pub(crate) fn self_qualified_hidden_module_alias_type_aliased_ref_mut_ready_queue_clear(scheduler: &mut hidden_sched::Scheduler) {
                let SelfHiddenModuleOwner { ref mut ready_queue, .. } = *scheduler;
                ready_queue.clear();
            }

            pub(crate) fn realiased_hidden_module_alias_type_aliased_ref_mut_ready_queue_clear(scheduler: &mut hidden_sched_realias::Scheduler) {
                let RealiasedOwner { ref mut ready_queue, .. } = *scheduler;
                ready_queue.clear();
            }

            pub(crate) fn realiased_grouped_module_alias_type_aliased_ref_mut_ready_queue_clear(scheduler: &mut realiased_sched_group::Scheduler) {
                let RealiasedGroupedOwner { ref mut ready_queue, .. } = *scheduler;
                ready_queue.clear();
            }

            pub(crate) fn nested_realiased_grouped_module_alias_type_aliased_ref_mut_ready_queue_clear(scheduler: &mut nested_realiased_sched_group::Scheduler) {
                let NestedRealiasedGroupedOwner { ref mut ready_queue, .. } = *scheduler;
                ready_queue.clear();
            }

            mod outer_alias_scope {
                mod inner_alias_scope {
                    type DeepOwner = super::super::Scheduler;

                    pub(crate) fn deep_relative_type_aliased_ref_mut_ready_queue_clear(scheduler: &mut super::super::Scheduler) {
                        let DeepOwner { ref mut ready_queue, .. } = *scheduler;
                        ready_queue.clear();
                    }
                }
            }

            pub(crate) fn renamed_ready_queue_alias(scheduler: &mut Scheduler, fid: FiberId) {
                let Scheduler { ready_queue: rq, .. } = scheduler;
                rq.push_back(fid);
            }

            pub(crate) fn qualified_ready_queue_alias(scheduler: &mut Scheduler, fid: FiberId) {
                let crate::scheduler::Scheduler { ready_queue: rq, .. } = scheduler;
                rq.push_back(fid);
            }

            pub(crate) fn match_ready_queue_alias(scheduler: &mut Scheduler, fid: FiberId) {
                match scheduler {
                    Scheduler { ready_queue: rq, .. } => rq.push_back(fid),
                }
            }

            impl Scheduler {
                pub(crate) fn owned_wake(&mut self, fid: FiberId) {
                    self.try_wake_fiber(fid);
                }
            }

            mod owner_spoof {
                struct Scheduler;

                impl Scheduler {
                    pub(crate) fn rogue(real: &mut super::Scheduler, fid: super::FiberId) {
                        real.try_wake_fiber(fid);
                    }
                }
            }

            pub(in crate) mod scoped_owner_spoof {
                struct Scheduler;

                impl Scheduler {
                    pub(crate) fn rogue(real: &mut super::Scheduler, fid: super::FiberId) {
                        real.try_wake_fiber(fid);
                    }
                }
            }

            pub(self) mod self_owner_spoof {
                struct Scheduler;

                impl Scheduler {
                    pub(crate) fn rogue(real: &mut super::Scheduler, fid: super::FiberId) {
                        real.try_wake_fiber(fid);
                    }
                }
            }

            #[allow(dead_code)] mod attr_owner_spoof {
                struct Scheduler;

                impl Scheduler {
                    pub(crate) fn rogue(real: &mut super::Scheduler, fid: super::FiberId) {
                        real.try_wake_fiber(fid);
                    }
                }
            }

            mod/**/comment_owner_spoof {
                struct Scheduler;

                impl Scheduler {
                    pub(crate) fn rogue(real: &mut super::Scheduler, fid: super::FiberId) {
                        real.try_wake_fiber(fid);
                    }
                }
            }

            pub(crate)/**/mod visible_comment_owner_spoof {
                struct Scheduler;

                impl Scheduler {
                    pub(crate) fn rogue(real: &mut super::Scheduler, fid: super::FiberId) {
                        real.try_wake_fiber(fid);
                    }
                }
            }

            pub/**/(crate) mod split_scoped_comment_owner_spoof {
                struct Scheduler;

                impl Scheduler {
                    pub(crate) fn rogue(real: &mut super::Scheduler, fid: super::FiberId) {
                        real.try_wake_fiber(fid);
                    }
                }
            }
        "#;
    let occurrences = compact_pattern_occurrences(probe, ".try_wake_fiber(");
    assert_eq!(occurrences.len(), 9);
    let queue_occurrences = compact_pattern_occurrences(probe, "ready_queue.push_back(");
    assert_eq!(queue_occurrences.len(), 1);
    let renamed_queue_occurrences = scheduler_destructured_ready_queue_occurrences_062(probe);
    assert_eq!(renamed_queue_occurrences.len(), 37);

    assert!(
        !scheduler_raw_wake_call_site_allowed_062("src/scheduler.rs", probe, occurrences[0]),
        "scheduler.rs free helpers must not become untracked crate-visible raw wake appliers"
    );
    assert!(
        scheduler_raw_wake_call_site_allowed_062("src/scheduler.rs", probe, occurrences[1]),
        "inherent Scheduler methods own raw scheduler mutation"
    );
    assert!(
        !scheduler_raw_wake_call_site_allowed_062("src/scheduler.rs", probe, occurrences[2]),
        "nested same-name Scheduler types must not spoof raw scheduler ownership"
    );
    assert!(
        !scheduler_raw_wake_call_site_allowed_062("src/scheduler.rs", probe, occurrences[3]),
        "scoped-visibility nested Scheduler modules must not spoof raw scheduler ownership"
    );
    assert!(
        !scheduler_raw_wake_call_site_allowed_062("src/scheduler.rs", probe, occurrences[4]),
        "pub(self) nested Scheduler modules must not spoof raw scheduler ownership"
    );
    assert!(
        !scheduler_raw_wake_call_site_allowed_062("src/scheduler.rs", probe, occurrences[5]),
        "attribute-prefixed nested Scheduler modules must not spoof raw scheduler ownership"
    );
    assert!(
        !scheduler_raw_wake_call_site_allowed_062("src/scheduler.rs", probe, occurrences[6]),
        "comment-separated mod tokens must not hide nested Scheduler owner spoofing"
    );
    assert!(
        !scheduler_raw_wake_call_site_allowed_062("src/scheduler.rs", probe, occurrences[7]),
        "comment-separated visible mod tokens must not hide nested Scheduler owner spoofing"
    );
    assert!(
        !scheduler_raw_wake_call_site_allowed_062("src/scheduler.rs", probe, occurrences[8]),
        "split scoped visibility must not hide nested Scheduler owner spoofing"
    );
    let runqueue_pattern = *scheduler_raw_mutation_patterns_062()
        .iter()
        .find(|pattern| pattern.compact == "ready_queue.push_back(")
        .expect("bare ready_queue mutation pattern");
    assert!(
        !scheduler_mutation_call_site_allowed_062(
            "src/scheduler.rs",
            probe,
            queue_occurrences[0],
            runqueue_pattern,
        ),
        "scheduler.rs free helpers must not mutate destructured ready_queue bindings"
    );
    assert!(
        !scheduler_impl_call_site_allowed_062(
            "src/scheduler.rs",
            probe,
            renamed_queue_occurrences[0].0,
        ),
        "scheduler.rs free helpers must not hide ready_queue ownership behind destructuring"
    );
    for (byte_idx, _label) in renamed_queue_occurrences.iter().skip(1) {
        assert!(
                !scheduler_impl_call_site_allowed_062("src/scheduler.rs", probe, *byte_idx),
                "scheduler.rs free helpers must not hide ready_queue ownership behind shorthand, qualified, or match destructuring"
            );
    }
}

#[test]
fn vm_scheduler_wake_boundary_013_scanner_tracks_inherent_raw_wake_aliases_062() {
    let scheduler_probe = r#"
            macro_rules! wake_it {
                ($scheduler:expr, $fid:expr) => {
                    $scheduler.try_wake_fiber($fid)
                };
            }
            macro_rules! r#wake_it_raw {
                ($scheduler:expr, $fid:expr) => {
                    $scheduler.try_wake_fiber($fid)
                };
            }
            macro_rules! wake_wrapper {
                ($scheduler:expr, $fid:expr) => {
                    wake_it!($scheduler, $fid)
                };
            }
            use wake_it as renamed_wake;
            use crate::wake_it as renamed_crate_wake;
            use crate::{wake_it as renamed_grouped_wake};
            use crate::{other as other_alias, wake_it as renamed_late_grouped_wake};

            impl Scheduler {
                pub(crate) fn raw_wake_alias(&mut self, fid: FiberId) {
                    self.try_wake_fiber(fid);
                }

                pub(crate) fn raw_wake_alias2(&mut self, fid: FiberId) {
                    self.raw_wake_alias(fid);
                }

                pub(crate) fn r#raw_wake_raw_ident_alias(&mut self, fid: FiberId) {
                    self.try_wake_fiber(fid);
                }

                pub(crate) fn macro_raw_wake_alias(&mut self, fid: FiberId) {
                    wake_it!(self, fid);
                }

                pub(crate) fn spawn_alias(&mut self, fid: FiberId) {
                    self.ready_queue.push_back(fid);
                }
            }
        "#;
    let caller_probe = r#"
            fn unauthorized(vm: &mut Vm, fid: FiberId) {
                vm.scheduler.raw_wake_alias(fid);
                vm.scheduler.raw_wake_alias2(fid);
                vm.scheduler.r#raw_wake_raw_ident_alias(fid);
                vm.scheduler.spawn_alias(fid);
            }
        "#;
    let aliases = scheduler_raw_wake_alias_method_names_062(scheduler_probe);
    let occurrences =
        scheduler_raw_wake_source_occurrences_with_aliases_062(caller_probe, &aliases);
    let macro_probe = r#"
            fn unauthorized(vm: &mut Vm, fid: FiberId) {
                call_sched!(vm.scheduler, raw_wake_alias, fid);
                call_sched!(vm.scheduler, raw_wake_alias2, fid);
                wake_it!(vm.scheduler, fid);
                r#wake_it_raw!(vm.scheduler, fid);
                wake_wrapper!(vm.scheduler, fid);
                renamed_wake!(vm.scheduler, fid);
                renamed_crate_wake!(vm.scheduler, fid);
                renamed_grouped_wake!(vm.scheduler, fid);
                renamed_late_grouped_wake!(vm.scheduler, fid);
            }
        "#;
    let macro_occurrences =
        scheduler_macro_forwarded_mutation_occurrences_with_aliases_and_macro_source_062(
            macro_probe,
            &aliases,
            scheduler_probe,
        );

    assert!(
        aliases.contains("raw_wake_alias"),
        "Scheduler methods forwarding raw wake appliers must become reserved raw wake aliases"
    );
    assert!(
        aliases.contains("raw_wake_alias2"),
        "Scheduler methods forwarding raw wake aliases must also become reserved raw wake aliases"
    );
    assert!(
            aliases.contains("raw_wake_raw_ident_alias"),
            "Scheduler raw-identifier method names forwarding raw wake appliers must become reserved aliases"
        );
    assert!(
            aliases.contains("macro_raw_wake_alias"),
            "Scheduler methods forwarding raw wake appliers through local macros must become reserved aliases"
        );
    assert!(
            !aliases.contains("spawn_alias"),
            "ordinary runqueue APIs must not become raw wake aliases unless they forward raw wake appliers"
        );
    assert_eq!(
        occurrences.len(),
        3,
        "external calls to Scheduler raw wake aliases must be scanned as raw wake authority"
    );
    assert_eq!(
            macro_occurrences.len(),
            9,
            "macro-forwarded calls to Scheduler raw wake aliases, raw macro names, wrapper macros, and macro aliases must be scanned as raw wake authority"
        );
}

#[test]
fn vm_scheduler_wake_boundary_013_scanner_ignores_comment_function_names_062() {
    let probe = r#"
            fn rogue_bridge(scheduler: &mut Scheduler, waiter: &QueueWaiter) {
                /* fn wake_waiter */
                scheduler.wake_queue_waiter(waiter);
            }
        "#;
    let occurrences = compact_pattern_occurrences(probe, ".wake_queue_waiter(");
    assert_eq!(occurrences.len(), 1);
    assert_eq!(
        enclosing_function_name_062(probe, occurrences[0]).as_deref(),
        Some("rogue_bridge")
    );
    assert!(
        !scheduler_raw_wake_call_site_allowed_062("src/vm/types.rs", probe, occurrences[0]),
        "vm/types.rs raw wake allowlist must not be spoofed by comments or strings"
    );
}

#[test]
fn vm_scheduler_wake_boundary_013_scanner_ignores_textual_cfg_test_split_062() {
    let probe = r#"
            // #[cfg(test)] in a production comment must not hide later source.
            fn rogue_bridge(scheduler: &mut Scheduler, waiter: &QueueWaiter) {
                scheduler.wake_queue_waiter(waiter);
            }

            #[cfg(test)]
            mod tests {
                fn allowed_test_helper(scheduler: &mut Scheduler, waiter: &QueueWaiter) {
                    scheduler.wake_queue_waiter(waiter);
                }
            }

            #[cfg(test)]
            mod command_tests {
                fn allowed_named_test_helper(scheduler: &mut Scheduler, waiter: &QueueWaiter) {
                    scheduler.ready_queue.clear();
                    scheduler.wake_queue_waiter(waiter);
                }
            }

            fn post_test_rogue_bridge(scheduler: &mut Scheduler, waiter: &QueueWaiter) {
                scheduler.ready_queue.push_back(waiter.wake_key().fiber_id());
            }
        "#;
    let production = production_source_without_test_modules(probe);
    let occurrences = compact_pattern_occurrences(&production, ".wake_queue_waiter(");
    let runqueue_occurrences = compact_pattern_occurrences(&production, ".ready_queue.push_back(");

    assert_eq!(occurrences.len(), 1);
    assert_eq!(
        runqueue_occurrences.len(),
        1,
        "real production code after a test module must remain scanned"
    );
    assert!(
        !scheduler_raw_wake_call_site_allowed_062("src/vm/types.rs", &production, occurrences[0]),
        "textual cfg(test) markers in comments must not hide production raw wake calls"
    );
}
