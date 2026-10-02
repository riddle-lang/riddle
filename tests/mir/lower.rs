use crate::{compile, lower};

#[test]
fn logical_operators_lower_to_short_circuit_control_flow() {
    let module = lower(
        r"
        fun rhs() -> bool { true }
        fun both() -> bool { false && rhs() }
        fun either() -> bool { true || rhs() }
        ",
    );

    for name in ["both", "either"] {
        let function = module
            .functions
            .values()
            .find(|function| function.name == name)
            .unwrap_or_else(|| panic!("missing {name}"));
        assert!(
            function.blocks.iter().any(|(_, block)| matches!(
                block.terminator,
                mir::instr::Terminator::CondBranch(..)
            )),
            "{name} must branch before evaluating its right operand: {function:#?}"
        );
        assert!(
            function.blocks.iter().any(|(_, block)| block
                .insts
                .iter()
                .any(|inst| matches!(inst.kind, mir::instr::InstKind::Phi(..)))),
            "{name} must merge the short-circuit result: {function:#?}"
        );
        assert!(
            !function.blocks[function.entry]
                .insts
                .iter()
                .any(|inst| matches!(
                    &inst.kind,
                    mir::instr::InstKind::Call(mir::value::FuncRef::Local(callee), _)
                        if callee == "rhs"
                )),
            "{name} eagerly evaluates its right operand: {function:#?}"
        );
    }
}

#[test]
fn dyn_trait_vtable_preserves_associated_type_projection() {
    let (_, type_result, analysis, module) = compile(
        r#"
        trait Producer {
            type Item;
            fun get(&self) -> Self::Item;
        }
        struct Number { value: i32 }
        impl Producer for Number {
            type Item = i32;
            fun get(&self) -> i32 { self.value }
        }
        fun read(value: &dyn Producer<Item = i32>) -> i32 { value.get() }
        fun main() -> i32 {
            let number = Number { value: 7 };
            read(&number)
        }
        "#,
    );
    assert_eq!(type_result.diagnostics, vec![]);
    assert_eq!(analysis.diagnostics, vec![]);
    let read = module
        .functions
        .values()
        .find(|function| function.name == "read")
        .expect("missing read");
    assert!(
        read.blocks.iter().any(|(_, block)| {
            block.insts.iter().any(|inst| {
                matches!(inst.kind, mir::instr::InstKind::CallIndirect(..))
                    && inst.ty == mir::types::Type::Int(mir::types::IntTy::I32)
            })
        }),
        "dyn associated-type call must retain its i32 return type: {read:#?}"
    );
}

#[test]
fn discarded_temporaries_drop_moved_and_remaining_fields() {
    let (_, types, moves, module) = compile(
        r#"
        #[lang = "drop"]
        trait Drop { fun drop(&mut self); }

        struct Guard { id: i32 }
        struct Pair { first: Guard, second: Guard }

        impl Drop for Guard { fun drop(&mut self) {} }

        fun make_pair() -> Pair {
            Pair { first: Guard { id: 1 }, second: Guard { id: 2 } }
        }

        fun main() {
            make_pair().first;
            make_pair();
        }
        "#,
    );
    assert!(types.diagnostics.is_empty(), "{:#?}", types.diagnostics);
    assert!(moves.diagnostics.is_empty(), "{:#?}", moves.diagnostics);

    let main = module
        .functions
        .values()
        .find(|function| function.name == "main")
        .expect("missing main");
    let drops = main
        .blocks
        .iter()
        .flat_map(|(_, block)| &block.insts)
        .filter(|inst| {
            matches!(
                &inst.kind,
                mir::instr::InstKind::Call(mir::value::FuncRef::Local(name), _)
                    if name == "drop__5:Guard"
            )
        })
        .count();
    assert_eq!(
        drops, 5,
        "drop glue must cover the discarded field and both source aggregates: {main:#?}"
    );
    let mut false_values = std::collections::HashSet::new();
    let moved_field_is_inactive = main.blocks.iter().any(|(_, block)| {
        block.insts.iter().enumerate().any(|(index, inst)| {
            let value = block.value_at(index);
            match inst.kind {
                mir::instr::InstKind::Const(mir::instr::ConstValue::Bool(false)) => {
                    false_values.insert(value);
                    false
                }
                mir::instr::InstKind::Store(value, _) => false_values.contains(&value),
                _ => false,
            }
        })
    });
    assert!(
        moved_field_is_inactive,
        "the moved field's source drop flag must be cleared: {main:#?}"
    );
}

#[test]
fn drop_locals_in_reverse_declaration_order() {
    let module = lower(
        r#"
        #[lang = "drop"]
        trait Drop {
            fun drop(&mut self);
        }

        struct Guard { id: i32 }

        impl Drop for Guard {
            fun drop(&mut self) {}
        }

        fun main() {
            let first = Guard { id: 1 };
            let second = Guard { id: 2 };
        }
        "#,
    );

    let main = module
        .functions
        .values()
        .find(|function| function.name == "main")
        .expect("missing main");
    let mut references = std::collections::HashMap::new();
    let mut drop_places = Vec::new();
    for (_, block) in main.blocks.iter() {
        for (index, inst) in block.insts.iter().enumerate() {
            let value = block.value_at(index);
            match &inst.kind {
                mir::instr::InstKind::UnOp(mir::instr::UnOp::MutRef, place) => {
                    references.insert(value, *place);
                }
                mir::instr::InstKind::Call(mir::value::FuncRef::Local(name), args)
                    if name.contains("drop") =>
                {
                    drop_places.push(references[&args[0]]);
                }
                _ => {}
            }
        }
    }

    assert_eq!(drop_places.len(), 2, "expected two Drop calls: {main:#?}");
    assert_ne!(drop_places[0], drop_places[1]);
    assert!(
        drop_places[0].0 > drop_places[1].0,
        "second local must drop before first: {drop_places:?}"
    );
}

#[test]
fn drop_glue_runs_user_drop_before_fields_in_declaration_order() {
    let module = lower(
        r#"
        #[lang = "drop"]
        trait Drop {
            fun drop(&mut self);
        }

        struct First {}
        struct Second {}
        struct Owner { first: First, second: Second }

        impl Drop for First { fun drop(&mut self) {} }
        impl Drop for Second { fun drop(&mut self) {} }
        impl Drop for Owner { fun drop(&mut self) {} }

        fun main() {
            let owner = Owner { first: First {}, second: Second {} };
        }
        "#,
    );

    let main = module
        .functions
        .values()
        .find(|function| function.name == "main")
        .expect("missing main");
    let calls = main
        .blocks
        .iter()
        .flat_map(|(_, block)| &block.insts)
        .filter_map(|inst| match &inst.kind {
            mir::instr::InstKind::Call(mir::value::FuncRef::Local(name), _)
                if name.contains("drop") =>
            {
                Some(name.as_str())
            }
            _ => None,
        })
        .collect::<Vec<_>>();

    assert_eq!(calls, ["drop__5:Owner", "drop__5:First", "drop__6:Second"]);
}

#[test]
fn aggregate_without_user_drop_still_drops_its_fields() {
    let module = lower(
        r#"
        #[lang = "drop"]
        trait Drop {
            fun drop(&mut self);
        }

        struct Guard {}
        struct Wrapper { guard: Guard }

        impl Drop for Guard { fun drop(&mut self) {} }

        fun main() {
            let wrapper = Wrapper { guard: Guard {} };
        }
        "#,
    );

    let main = module
        .functions
        .values()
        .find(|function| function.name == "main")
        .expect("missing main");
    assert!(
        main.blocks
            .iter()
            .any(|(_, block)| block.insts.iter().any(|inst| {
                matches!(
                    &inst.kind,
                    mir::instr::InstKind::Call(mir::value::FuncRef::Local(name), _)
                        if name == "drop__5:Guard"
                )
            })),
        "field drop glue is missing: {main:#?}"
    );
}

#[test]
fn partial_move_clears_only_the_moved_fields_drop_flag() {
    let module = lower(
        r#"
        #[lang = "drop"]
        trait Drop {
            fun drop(&mut self);
        }

        struct First {}
        struct Second {}
        struct Pair { first: First, second: Second }
        impl Drop for First { fun drop(&mut self) {} }
        impl Drop for Second { fun drop(&mut self) {} }

        fun consume(value: First) {}

        fun main() {
            let pair = Pair { first: First {}, second: Second {} };
            consume(pair.first);
        }
        "#,
    );

    let main = module
        .functions
        .values()
        .find(|function| function.name == "main")
        .expect("missing main");
    let bool_allocas = main
        .blocks
        .iter()
        .flat_map(|(_, block)| &block.insts)
        .filter(|inst| {
            matches!(
                inst.kind,
                mir::instr::InstKind::Alloca(mir::types::Type::Ptr(ref inner))
                    if **inner == mir::types::Type::Bool
            )
        })
        .count();
    let mut false_values = std::collections::HashSet::new();
    let mut false_stores = 0;
    for (_, block) in main.blocks.iter() {
        for (index, inst) in block.insts.iter().enumerate() {
            let value = block.value_at(index);
            match &inst.kind {
                mir::instr::InstKind::Const(mir::instr::ConstValue::Bool(false)) => {
                    false_values.insert(value);
                }
                mir::instr::InstKind::Store(value, _) if false_values.contains(value) => {
                    false_stores += 1;
                }
                _ => {}
            }
        }
    }

    assert_eq!(
        bool_allocas, 2,
        "each droppable field needs its own flag: {main:#?}"
    );
    assert_eq!(
        false_stores, 1,
        "only the moved field becomes inactive: {main:#?}"
    );
}

#[test]
fn dynamic_array_move_clears_only_the_selected_elements_drop_flag() {
    let module = lower(
        r#"
        #[lang = "drop"]
        trait Drop {
            fun drop(&mut self);
        }

        struct Guard {}
        impl Drop for Guard { fun drop(&mut self) {} }
        fun consume(value: Guard) {}

        fun main(index: usize) {
            let values = [Guard {}, Guard {}];
            consume(values[index]);
        }
        "#,
    );

    let main = module
        .functions
        .values()
        .find(|function| function.name == "main")
        .expect("missing main");
    assert!(
        main.blocks
            .iter()
            .any(|(_, block)| block.insts.iter().any(|inst| {
                matches!(
                    inst.kind,
                    mir::instr::InstKind::Const(mir::instr::ConstValue::Bool(false))
                )
            })),
        "dynamic element move must clear a runtime-selected flag: {main:#?}"
    );
}

#[test]
fn array_and_enum_drop_glue_cover_elements_and_active_variant() {
    let module = lower(
        r#"
        #[lang = "drop"]
        trait Drop {
            fun drop(&mut self);
        }

        struct First {}
        struct Second {}
        impl Drop for First { fun drop(&mut self) {} }
        impl Drop for Second { fun drop(&mut self) {} }

        enum Choice { First(First), Second(Second) }

        fun drop_array() {
            let values = [First {}, First {}];
        }

        fun drop_choice() {
            let choice = Choice::Second(Second {});
        }
        "#,
    );

    let calls = |function_name: &str| {
        let function = module
            .functions
            .values()
            .find(|function| function.name == function_name)
            .unwrap_or_else(|| panic!("missing {function_name}"));
        function
            .blocks
            .iter()
            .flat_map(|(_, block)| &block.insts)
            .filter(|inst| {
                matches!(
                    &inst.kind,
                    mir::instr::InstKind::Call(mir::value::FuncRef::Local(name), _)
                        if name.contains("drop")
                )
            })
            .count()
    };

    assert_eq!(calls("drop_array"), 2);
    assert_eq!(
        calls("drop_choice"),
        2,
        "both variant glue paths must be emitted"
    );
    let choice = module
        .functions
        .values()
        .find(|function| function.name == "drop_choice")
        .unwrap();
    assert!(
        choice.blocks.iter().any(|(_, block)| {
            matches!(block.terminator, mir::instr::Terminator::CondBranch(..))
        }),
        "enum drop glue must dispatch on the active variant: {choice:#?}"
    );
}

#[test]
fn return_drops_live_local_before_terminating() {
    let module = lower(
        r#"
        #[lang = "drop"]
        trait Drop {
            fun drop(&mut self);
        }

        struct Guard {}

        impl Drop for Guard {
            fun drop(&mut self) {}
        }

        fun answer() -> i32 {
            let guard = Guard {};
            return 42;
        }
        "#,
    );

    let answer = module
        .functions
        .values()
        .find(|function| function.name == "answer")
        .expect("missing answer");
    let has_drop = answer.blocks.iter().any(|(_, block)| {
        block.insts.iter().any(|inst| {
            matches!(
                &inst.kind,
                mir::instr::InstKind::Call(mir::value::FuncRef::Local(name), _)
                    if name.contains("drop")
            )
        })
    });

    assert!(has_drop, "return path must run Drop: {answer:#?}");
}

#[test]
fn moving_return_value_clears_its_drop_flag() {
    let module = lower(
        r#"
        #[lang = "drop"]
        trait Drop {
            fun drop(&mut self);
        }

        struct Guard {}

        impl Drop for Guard {
            fun drop(&mut self) {}
        }

        fun take() -> Guard {
            let guard = Guard {};
            return guard;
        }

        fun take_implicit() -> Guard {
            let guard = Guard {};
            guard
        }
        "#,
    );

    for name in ["take", "take_implicit"] {
        let function = module
            .functions
            .values()
            .find(|function| function.name == name)
            .unwrap_or_else(|| panic!("missing {name}"));
        let mut false_values = std::collections::HashSet::new();
        let mut cleared_flag = false;
        for (_, block) in function.blocks.iter() {
            for (index, inst) in block.insts.iter().enumerate() {
                let value = block.value_at(index);
                match &inst.kind {
                    mir::instr::InstKind::Const(mir::instr::ConstValue::Bool(false)) => {
                        false_values.insert(value);
                    }
                    mir::instr::InstKind::Store(value, _place) if false_values.contains(value) => {
                        cleared_flag = true;
                    }
                    _ => {}
                }
            }
        }

        assert!(
            cleared_flag,
            "moving a Drop local must clear its drop flag: {function:#?}"
        );
    }
}

#[test]
fn drop_parameters_in_declaration_order() {
    let module = lower(
        r#"
        #[lang = "drop"]
        trait Drop {
            fun drop(&mut self);
        }

        struct Guard {}

        impl Drop for Guard {
            fun drop(&mut self) {}
        }

        fun consume(first: Guard, second: Guard) {}
        "#,
    );

    let consume = module
        .functions
        .values()
        .find(|function| function.name == "consume")
        .expect("missing consume");
    let mut references = std::collections::HashMap::new();
    let mut drop_places = Vec::new();
    for (_, block) in consume.blocks.iter() {
        for (index, inst) in block.insts.iter().enumerate() {
            let value = block.value_at(index);
            match &inst.kind {
                mir::instr::InstKind::UnOp(mir::instr::UnOp::MutRef, place) => {
                    references.insert(value, *place);
                }
                mir::instr::InstKind::Call(mir::value::FuncRef::Local(name), args)
                    if name.contains("drop") =>
                {
                    drop_places.push(references[&args[0]]);
                }
                _ => {}
            }
        }
    }

    assert_eq!(
        drop_places.len(),
        2,
        "expected two parameter drops: {consume:#?}"
    );
    assert!(
        drop_places[0].0 < drop_places[1].0,
        "parameters must drop in declaration order: {drop_places:?}"
    );
}

#[test]
fn moving_parameter_clears_its_drop_flag() {
    let module = lower(
        r#"
        #[lang = "drop"]
        trait Drop {
            fun drop(&mut self);
        }

        struct Guard {}

        impl Drop for Guard {
            fun drop(&mut self) {}
        }

        fun take(guard: Guard) -> Guard {
            return guard;
        }
        "#,
    );

    let take = module
        .functions
        .values()
        .find(|function| function.name == "take")
        .expect("missing take");
    assert!(
        take.blocks.iter().any(|(_, block)| {
            let mut false_values = std::collections::HashSet::new();
            block.insts.iter().enumerate().any(|(index, inst)| {
                let value = block.value_at(index);
                match &inst.kind {
                    mir::instr::InstKind::Const(mir::instr::ConstValue::Bool(false)) => {
                        false_values.insert(value);
                        false
                    }
                    mir::instr::InstKind::Store(value, _) => false_values.contains(value),
                    _ => false,
                }
            })
        }),
        "moving a Drop parameter must clear its drop flag: {take:#?}"
    );
}

#[test]
fn assignment_drops_old_value_and_rearms_drop_flag() {
    let module = lower(
        r#"
        #[lang = "drop"]
        trait Drop {
            fun drop(&mut self);
        }

        struct Guard { id: i32 }
        impl Drop for Guard { fun drop(&mut self) {} }

        fun main() {
            let mut guard = Guard { id: 1 };
            guard = Guard { id: 2 };
        }
        "#,
    );

    let main = module
        .functions
        .values()
        .find(|function| function.name == "main")
        .expect("missing main");
    let drop_calls = main
        .blocks
        .iter()
        .flat_map(|(_, block)| &block.insts)
        .filter(|inst| {
            matches!(
                &inst.kind,
                mir::instr::InstKind::Call(mir::value::FuncRef::Local(name), _)
                    if name == "drop__5:Guard"
            )
        })
        .count();
    let true_values = main
        .blocks
        .iter()
        .flat_map(|(_, block)| &block.insts)
        .filter(|inst| {
            matches!(
                inst.kind,
                mir::instr::InstKind::Const(mir::instr::ConstValue::Bool(true))
            )
        })
        .count();

    assert_eq!(
        drop_calls, 2,
        "old and final values must both be dropped: {main:#?}"
    );
    assert!(
        true_values >= 2,
        "assignment must reactivate the drop flag: {main:#?}"
    );
}

#[test]
fn returned_closure_drops_value_captures_through_its_drop_function() {
    let module = lower(
        r#"
        #[lang = "drop"]
        trait Drop {
            fun drop(&mut self);
        }

        struct Guard {}
        impl Drop for Guard { fun drop(&mut self) {} }
        fun consume(value: Guard) {}

        fun make() -> impl FnOnce() -> () {
            let guard = Guard {};
            [ -> { consume(guard); }]
        }

        fun main() {
            let closure = make();
        }
        "#,
    );

    let closure_drop = module
        .functions
        .values()
        .find(|function| function.name.contains("lambda") && function.name.ends_with("_drop"))
        .expect("missing closure drop function");
    assert!(
        closure_drop
            .blocks
            .iter()
            .any(|(_, block)| block.insts.iter().any(|inst| {
                matches!(
                    &inst.kind,
                    mir::instr::InstKind::Call(mir::value::FuncRef::Local(name), _)
                        if name == "drop__5:Guard"
                )
            })),
        "closure drop function must drop its value captures: {closure_drop:#?}"
    );

    let main = module
        .functions
        .values()
        .find(|function| function.name == "main")
        .expect("missing main");
    assert!(
        main.blocks.iter().any(|(_, block)| block
            .insts
            .iter()
            .any(|inst| { matches!(inst.kind, mir::instr::InstKind::CallIndirect(_, _)) })),
        "closure owner must invoke its dynamic drop function: {main:#?}"
    );
}

#[test]
fn lambda_drops_its_by_value_parameters() {
    let module = lower(
        r#"
        #[lang = "drop"]
        trait Drop {
            fun drop(&mut self);
        }

        struct Guard {}
        impl Drop for Guard { fun drop(&mut self) {} }

        fun main() {
            let consume = [guard: Guard -> {}];
            consume(Guard {});
        }
        "#,
    );

    let lambda = module
        .functions
        .values()
        .find(|function| function.name.contains("lambda") && !function.name.ends_with("_drop"))
        .expect("missing lambda function");
    assert!(
        lambda
            .blocks
            .iter()
            .any(|(_, block)| block.insts.iter().any(|inst| {
                matches!(
                    &inst.kind,
                    mir::instr::InstKind::Call(mir::value::FuncRef::Local(name), _)
                        if name == "drop__5:Guard"
                )
            })),
        "lambda parameter must be dropped by the lambda body: {lambda:#?}"
    );
}

#[test]
fn monomorphized_generic_parameter_runs_drop() {
    let module = lower(
        r#"
        #[lang = "drop"]
        trait Drop {
            fun drop(&mut self);
        }

        struct Guard {}
        impl Drop for Guard { fun drop(&mut self) {} }

        fun drop_value<T>(value: T) {}

        fun main() {
            drop_value(Guard {});
        }
        "#,
    );

    let drop_value = module
        .functions
        .values()
        .find(|function| function.name.starts_with("drop_value__"))
        .expect("missing monomorphized drop_value");
    assert!(
        drop_value
            .blocks
            .iter()
            .any(|(_, block)| block.insts.iter().any(|inst| {
                matches!(
                    &inst.kind,
                    mir::instr::InstKind::Call(mir::value::FuncRef::Local(name), _)
                        if name == "drop__5:Guard"
                )
            })),
        "generic by-value parameter must be dropped after monomorphization: {drop_value:#?}"
    );
}

#[test]
fn monomorphized_generic_assignment_and_local_run_drop() {
    let module = lower(
        r#"
        #[lang = "drop"]
        trait Drop {
            fun drop(&mut self);
        }

        struct Guard {}
        impl Drop for Guard { fun drop(&mut self) {} }

        fun replace<T>(slot: &mut T, value: T, local: T) {
            *slot = value;
            let owned = local;
        }

        fun main() {
            let mut guard = Guard {};
            replace(&mut guard, Guard {}, Guard {});
        }
        "#,
    );

    let replace = module
        .functions
        .values()
        .find(|function| function.name.starts_with("replace__"))
        .expect("missing monomorphized replace");
    let drop_calls = replace
        .blocks
        .iter()
        .flat_map(|(_, block)| &block.insts)
        .filter(|inst| {
            matches!(
                &inst.kind,
                mir::instr::InstKind::Call(mir::value::FuncRef::Local(name), _)
                    if name == "drop__5:Guard"
            )
        })
        .count();

    assert!(
        drop_calls >= 2,
        "generic assignment and local must use the monomorphized type: {replace:#?}"
    );
}

#[test]
fn monomorphization_preserves_callers_drop_scope() {
    let module = lower(
        r#"
        #[lang = "drop"]
        trait Drop { fun drop(&mut self); }

        struct Guard {}
        impl Drop for Guard { fun drop(&mut self) {} }
        fun identity<T>(value: T) -> T { value }

        fun main() {
            let guard = Guard {};
            let value = identity(1);
        }
        "#,
    );

    let main = module
        .functions
        .values()
        .find(|function| function.name == "main")
        .expect("missing main");
    assert!(
        main.blocks
            .iter()
            .any(|(_, block)| block.insts.iter().any(|inst| {
                matches!(
                    &inst.kind,
                    mir::instr::InstKind::Call(mir::value::FuncRef::Local(name), _)
                        if name == "drop__5:Guard"
                )
            })),
        "generic lowering must restore the caller's drop scope: {main:#?}"
    );
}

#[test]
fn simple_function_no_params() {
    let module = lower(
        r"
        fun main() {
            let x = 42;
        }
        ",
    );
    assert_eq!(module.function_order.len(), 1);
    let func = &module.functions[module.function_order[0]];
    assert_eq!(func.name, "main");
    assert_eq!(func.params.len(), 0);
}

#[test]
fn function_with_params() {
    let module = lower(
        r"
        fun add(a: i32, b: i32) -> i32 {
            return a + b;
        }
        ",
    );
    let func = &module.functions[module.function_order[0]];
    assert_eq!(func.name, "add");
    assert_eq!(func.params.len(), 2);
    assert_eq!(func.params[0].name, "a");
    assert_eq!(func.params[1].name, "b");
}

#[test]
fn integer_literal() {
    let module = lower(
        r"
        fun main() {
            let x = 42;
        }
        ",
    );
    let func = &module.functions[module.function_order[0]];
    // 入口块应当包含 const 指令
    let entry = &func.blocks[func.entry];
    assert!(
        !entry.insts.is_empty(),
        "entry block should have instructions"
    );
}

#[test]
fn string_literal() {
    let module = lower(
        r#"
        fun main() {
            let x = "hello";
        }
        "#,
    );
    let func = &module.functions[module.function_order[0]];
    let string = func.blocks[func.entry]
        .insts
        .iter()
        .find(|inst| {
            matches!(
                inst.kind,
                mir::instr::InstKind::Const(mir::instr::ConstValue::String(_))
            )
        })
        .expect("missing string constant");
    assert_eq!(
        string.ty,
        mir::types::Type::Ref(Box::new(mir::types::Type::Str), false)
    );
}

#[test]
fn array_repeat_lowers_to_array_value() {
    let module = lower(
        r"
        fun main() {
            let xs: [i32; 4] = [5; 4];
        }
        ",
    );
    let func = &module.functions[module.function_order[0]];
    let entry = &func.blocks[func.entry];
    let repeated = entry.insts.iter().find_map(|inst| match &inst.kind {
        mir::instr::InstKind::ArrayValue(values) => Some(values),
        _ => None,
    });

    assert!(
        matches!(repeated, Some(values) if values.len() == 4 && values.iter().all(|v| *v == values[0])),
        "expected repeated ArrayValue, got {:?}",
        entry.insts.iter().map(|i| &i.kind).collect::<Vec<_>>()
    );
}

#[test]
fn tuple_expression_lowers_to_tuple_value() {
    let module = lower(
        r"
        fun pair() -> (i32, i32) {
            (2, 3)
        }
        ",
    );
    let pair = module
        .function_order
        .iter()
        .map(|fid| &module.functions[*fid])
        .find(|function| function.name == "pair")
        .unwrap();

    assert!(pair.blocks.iter().any(|(_, block)| block.insts.iter().any(
        |instruction| matches!(instruction.kind, mir::instr::InstKind::TupleValue(ref values) if values.len() == 2)
    )));
}

#[test]
fn tuple_equality_lowers_to_element_comparisons() {
    let module = lower(
        r#"
        #[lang = "partial_eq"]
        trait PartialEq<Rhs = Self> {
            fun eq(&self, other: &Rhs) -> bool;
        }

        fun main() -> bool {
            let left: (i32, i32) = (1, 2);
            let right: (i32, i32) = (1, 2);
            left == right
        }
        "#,
    );
    let main = module
        .function_order
        .iter()
        .map(|id| &module.functions[*id])
        .find(|function| function.name == "main")
        .unwrap();
    let comparisons = main
        .blocks
        .iter()
        .flat_map(|(_, block)| &block.insts)
        .filter(|inst| matches!(inst.kind, mir::instr::InstKind::Cmp(..)))
        .count();

    assert!(
        comparisons >= 2,
        "tuple equality should compare its elements: {main:#?}"
    );
}

#[test]
fn tuple_ordering_lowers_lexicographically() {
    let module = lower(
        r#"
        #[lang = "partial_ord"]
        trait PartialOrd<Rhs = Self> {
            fun lt(&self, other: &Rhs) -> bool;
        }

        fun main() -> bool {
            let left: (i32, i32) = (1, 2);
            let right: (i32, i32) = (1, 3);
            left < right
        }
        "#,
    );
    let main = module
        .function_order
        .iter()
        .map(|id| &module.functions[*id])
        .find(|function| function.name == "main")
        .unwrap();
    let comparisons = main
        .blocks
        .iter()
        .flat_map(|(_, block)| &block.insts)
        .filter(|inst| matches!(inst.kind, mir::instr::InstKind::Cmp(..)))
        .count();
    let branches = main
        .blocks
        .iter()
        .filter(|(_, block)| matches!(block.terminator, mir::instr::Terminator::CondBranch(..)))
        .count();

    assert!(
        comparisons >= 4,
        "tuple ordering should compare equality and order: {main:#?}"
    );
    assert!(
        branches >= 2,
        "tuple ordering should branch on equal prefixes: {main:#?}"
    );
}

#[test]
fn array_for_loop_lowers_to_indexed_loop() {
    let module = lower(
        r"
        fun main() {
            let mut sum = 0;
            let values = [1, 2, 3];
            for item in values {
                sum += item;
            }
        }
        ",
    );
    let func = &module.functions[module.function_order[0]];
    let has_loop_branch = func
        .blocks
        .iter()
        .any(|(_, block)| matches!(block.terminator, mir::instr::Terminator::CondBranch(..)));
    let has_index_ptr = func.blocks.iter().any(|(_, block)| {
        block
            .insts
            .iter()
            .any(|inst| matches!(inst.kind, mir::instr::InstKind::IndexPtr(..)))
    });

    assert!(has_loop_branch, "{func:#?}");
    assert!(has_index_ptr, "{func:#?}");
}

#[test]
fn generic_for_loop_lowers_to_iterator_calls() {
    let module = lower(
        r"
        enum Option<T> {
            Some(T),
            None,
        }

        trait Iterator {
            type Item;
            fun next(&mut self) -> Option<Self::Item>;
        }

        trait IntoIterator {
            type Item;
            type IntoIter;
            fun into_iter(self) -> Self::IntoIter;
        }

        struct Counter {
            current: i32,
        }

        impl Iterator for Counter {
            type Item = i32;

            fun next(&mut self) -> Option<Self::Item> {
                if self.current < 3 {
                    let value = self.current;
                    self.current += 1;
                    Option::Some(value)
                } else {
                    Option::None
                }
            }
        }

        impl IntoIterator for Counter {
            type Item = i32;
            type IntoIter = Counter;

            fun into_iter(self) -> Self::IntoIter {
                self
            }
        }

        fun main() {
            let counter = Counter { current: 0 };
            for item in counter {
                let next = item + 1;
            }
        }
        ",
    );
    let func = module
        .function_order
        .iter()
        .map(|fid| &module.functions[*fid])
        .find(|func| func.name == "main")
        .unwrap();

    let calls = func
        .blocks
        .iter()
        .flat_map(|(_, block)| block.insts.iter())
        .filter_map(|inst| match &inst.kind {
            mir::instr::InstKind::Call(mir::FuncRef::Local(name), _) => Some(name.as_str()),
            _ => None,
        })
        .collect::<Vec<_>>();
    assert!(
        calls.iter().any(|name| name.starts_with("into_iter")),
        "{func:#?}"
    );
    assert!(
        calls.iter().any(|name| name.starts_with("next")),
        "{func:#?}"
    );
    assert!(
        func.blocks
            .iter()
            .any(|(_, block)| matches!(block.terminator, mir::instr::Terminator::CondBranch(..)))
    );
}

#[test]
fn generic_function_for_loop_uses_concrete_iterator_impl_and_enum_layout() {
    let module = lower(
        r"
        enum Option<T> {
            Spare(bool),
            Some(T),
            None,
        }

        trait Iterator {
            type Item;
            fun next(&mut self) -> Option<Self::Item>;
        }

        trait IntoIterator {
            type Item;
            type IntoIter;
            fun into_iter(self) -> Self::IntoIter;
        }

        struct Counter<T> { current: T }

        impl<T> Iterator for Counter<T> {
            type Item = T;
            fun next(&mut self) -> Option<Self::Item> { Option::None }
        }

        impl<T> IntoIterator for Counter<T> {
            type Item = T;
            type IntoIter = Counter<T>;
            fun into_iter(self) -> Self::IntoIter { self }
        }

        fun consume<T: IntoIterator<Item = i32, IntoIter = Counter<i32>>>(values: T) {
            for value in values {
                let next = value + 1;
            }
        }

        fun main() {
            consume(Counter { current: 0 });
        }
        ",
    );
    let func = module
        .function_order
        .iter()
        .map(|fid| &module.functions[*fid])
        .find(|func| func.name == "consume__11:Counter_i32")
        .unwrap();
    let insts = func
        .blocks
        .iter()
        .flat_map(|(_, block)| block.insts.iter())
        .collect::<Vec<_>>();

    assert!(insts.iter().any(|inst| {
        matches!(&inst.kind, mir::instr::InstKind::Call(mir::FuncRef::Local(name), _) if name.starts_with("into_iter"))
    }));
    assert!(insts.iter().any(|inst| {
        matches!(&inst.kind, mir::instr::InstKind::Call(mir::FuncRef::Local(name), _) if name.starts_with("next"))
    }));
    assert!(
        insts
            .iter()
            .any(|inst| { matches!(inst.kind, mir::instr::InstKind::ExtractValue(_, 2)) })
    );
}

#[test]
fn if_expression_creates_blocks() {
    let module = lower(
        r"
        fun choose(flag: bool) -> i32 {
            if flag {
                return 1;
            }
            return 0;
        }
        ",
    );
    let func = &module.functions[module.function_order[0]];
    // if 应当产生多个基本块
    let block_count = func.blocks.iter().count();
    assert!(
        block_count >= 2,
        "expected at least 2 blocks for if, got {block_count}"
    );
}

#[test]
fn nested_if_phi_uses_actual_predecessor_blocks() {
    let module = lower(
        r"
        enum Choice { First, Second, Third }

        fun choose(first: bool, second: bool) -> Choice {
            if first {
                Choice::First
            } else {
                if second { Choice::Second } else { Choice::Third }
            }
        }
        ",
    );
    let func = &module.functions[module.function_order[0]];

    for (block_id, block) in func.blocks.iter() {
        for inst in &block.insts {
            let mir::instr::InstKind::Phi(inputs) = &inst.kind else {
                continue;
            };
            for (_, predecessor) in inputs {
                let has_edge = match func.blocks[*predecessor].terminator {
                    mir::instr::Terminator::Branch(target) => target == block_id,
                    mir::instr::Terminator::CondBranch(_, then_block, else_block) => {
                        then_block == block_id || else_block == block_id
                    }
                    _ => false,
                };
                assert!(
                    has_edge,
                    "phi in {block_id:?} names non-predecessor {predecessor:?}: {func:#?}"
                );
            }
        }
    }
}

#[test]
fn while_loop_creates_blocks() {
    let module = lower(
        r"
        fun loop_test() {
            let x = true;
            while x {
                let x = false;
            }
        }
        ",
    );
    let func = &module.functions[module.function_order[0]];
    // while 应当产生至少 3 个块 (cond, body, exit)
    let block_count = func.blocks.iter().count();
    assert!(
        block_count >= 3,
        "expected at least 3 blocks for while, got {block_count}"
    );
}

#[test]
fn break_and_continue_lower_to_loop_targets_and_skip_dead_code() {
    let module = lower(
        r"
        fun dead() {}

        fun main() {
            let mut i = 0;
            while i < 5 {
                i += 1;
                if i == 2 {
                    continue;
                    dead();
                }
                break;
                dead();
            }

            for item in [1, 2, 3] {
                if item == 1 {
                    continue;
                }
                break;
            }
        }
        ",
    );
    let func = module
        .function_order
        .iter()
        .map(|fid| &module.functions[*fid])
        .find(|func| func.name == "main")
        .unwrap();
    let block_id = |label: &str| {
        func.blocks
            .iter()
            .find_map(|(id, block)| (block.label.as_deref() == Some(label)).then_some(id))
            .unwrap()
    };
    let branch_count = |target| {
        func.blocks
            .iter()
            .filter(|(_, block)| {
                matches!(block.terminator, mir::instr::Terminator::Branch(id) if id == target)
            })
            .count()
    };

    assert!(branch_count(block_id("while_cond")) >= 2, "{func:#?}");
    assert!(branch_count(block_id("while_exit")) >= 1, "{func:#?}");
    assert!(branch_count(block_id("for_array_step")) >= 1, "{func:#?}");
    assert!(branch_count(block_id("for_array_exit")) >= 1, "{func:#?}");
    assert!(!func.blocks.iter().any(|(_, block)| {
        block.insts.iter().any(|inst| {
            matches!(&inst.kind, mir::instr::InstKind::Call(mir::FuncRef::Local(name), _) if name == "dead")
        })
    }));
}

#[test]
fn break_and_continue_drop_locals_before_branching() {
    let module = lower(
        r#"
        #[lang = "drop"]
        trait Drop {
            fun drop(&mut self);
        }

        struct Guard {}

        impl Drop for Guard {
            fun drop(&mut self) {}
        }

        fun on_break() {
            while true {
                let guard = Guard {};
                break;
            }
        }

        fun on_continue() {
            while true {
                let guard = Guard {};
                continue;
            }
        }
        "#,
    );

    for name in ["on_break", "on_continue"] {
        let function = module
            .functions
            .values()
            .find(|function| function.name == name)
            .unwrap_or_else(|| panic!("missing {name}"));
        let has_drop = function.blocks.iter().any(|(_, block)| {
            block.insts.iter().any(|inst| {
                matches!(
                    &inst.kind,
                    mir::instr::InstKind::Call(mir::value::FuncRef::Local(name), _)
                        if name.contains("drop")
                )
            })
        });
        assert!(
            has_drop,
            "{name} must drop locals before jumping: {function:#?}"
        );
    }
}

#[test]
fn arithmetic_operations() {
    let module = lower(
        r"
        fun compute(a: i32, b: i32) -> i32 {
            let c = a + b;
            let d = c * 2;
            let e = d - 1;
            return e / 3;
        }
        ",
    );
    let func = &module.functions[module.function_order[0]];
    let entry = &func.blocks[func.entry];
    // 应当有多条指令
    assert!(entry.insts.len() >= 4, "expected at least 4 instructions");
}

#[test]
fn i32_add_lowers_to_builtin_binop() {
    let module = lower(
        r"
        fun main() {
            let a: i32 = 1;
            let b: i32 = 2;
            let sum = a + b;
        }
        ",
    );
    let func = module
        .function_order
        .iter()
        .map(|fid| &module.functions[*fid])
        .find(|func| func.name == "main")
        .unwrap();
    let entry = &func.blocks[func.entry];

    assert!(entry.insts.iter().any(|i| matches!(
        i.kind,
        mir::instr::InstKind::BinOp(mir::instr::BinOp::Add, _, _)
    )));
}

#[test]
fn primitive_lang_operator_methods_lower_without_wrapper_functions() {
    let module = lower(
        r#"
        #[lang = "add"]
        trait Add<Rhs = Self> {
            type Output;
            fun add(self, rhs: Rhs) -> Self::Output;
        }

        #[lang = "neg"]
        trait Neg {
            type Output;
            fun neg(self) -> Self::Output;
        }

        #[lang = "add_assign"]
        trait AddAssign<Rhs = Self> {
            fun add_assign(&mut self, rhs: Rhs);
        }

        impl Add for i32 {
            type Output = i32;
            fun add(self, rhs: Self) -> Self::Output { self + rhs }
        }

        impl Neg for i32 {
            type Output = i32;
            fun neg(self) -> Self::Output { -self }
        }

        impl AddAssign for i32 {
            fun add_assign(&mut self, rhs: Self) { *self += rhs; }
        }

        fun main() -> i32 {
            let mut value: i32 = 1;
            let sum = value.add(2);
            let negated = sum.neg();
            value.add_assign(3);
            value + negated
        }
        "#,
    );

    let wrappers = module
        .function_order
        .iter()
        .map(|fid| module.functions[*fid].name.as_str())
        .filter(|name| matches!(*name, "add__i32" | "neg__i32" | "add_assign__i32"))
        .collect::<Vec<_>>();
    assert!(wrappers.is_empty(), "unexpected wrappers: {wrappers:?}");
    let main = module
        .function_order
        .iter()
        .map(|fid| &module.functions[*fid])
        .find(|function| function.name == "main")
        .unwrap();
    let instructions = main
        .blocks
        .iter()
        .flat_map(|(_, block)| block.insts.iter())
        .collect::<Vec<_>>();

    assert!(instructions.iter().any(|instruction| matches!(
        instruction.kind,
        mir::instr::InstKind::BinOp(mir::instr::BinOp::Add, _, _)
    )));
    assert!(instructions.iter().any(|instruction| matches!(
        instruction.kind,
        mir::instr::InstKind::UnOp(mir::instr::UnOp::Neg, _)
    )));
    assert!(!instructions.iter().any(|instruction| matches!(
        &instruction.kind,
        mir::instr::InstKind::Call(mir::FuncRef::Local(name), _)
            if name == "add__i32" || name == "neg__i32" || name == "add_assign__i32"
    )));
    assert_eq!(
        instructions
            .iter()
            .filter(|instruction| matches!(instruction.kind, mir::instr::InstKind::Store(_, _)))
            .count(),
        2,
        "initialization and add_assign must both write to value"
    );
    assert!(instructions.windows(4).any(|window| {
        matches!(
            window[0].kind,
            mir::instr::InstKind::Const(mir::instr::ConstValue::Int(3, _))
        ) && matches!(window[1].kind, mir::instr::InstKind::Load(_))
            && matches!(
                window[2].kind,
                mir::instr::InstKind::BinOp(mir::instr::BinOp::Add, _, _)
            )
            && matches!(window[3].kind, mir::instr::InstKind::Store(_, _))
    }));
}

#[test]
fn primitive_operator_trait_without_lang_marker_keeps_ordinary_method() {
    let module = lower(
        r"
        trait Add {
            type Output;
            fun add(self, rhs: Self) -> Self::Output;
        }

        impl Add for i32 {
            type Output = i32;
            fun add(self, rhs: Self) -> Self::Output { self + rhs }
        }

        fun main() -> i32 {
            let value: i32 = 1;
            value.add(2)
        }
        ",
    );

    assert!(
        module
            .function_order
            .iter()
            .any(|fid| module.functions[*fid].name == "add__i32")
    );
    let main = module
        .function_order
        .iter()
        .map(|fid| &module.functions[*fid])
        .find(|function| function.name == "main")
        .unwrap();
    assert!(
        main.blocks
            .iter()
            .any(|(_, block)| block.insts.iter().any(|instruction| matches!(
                &instruction.kind,
                mir::instr::InstKind::Call(mir::FuncRef::Local(name), _)
                    if name == "add__i32"
            )))
    );
}

#[test]
fn malformed_lang_operator_signature_is_rejected() {
    // A `#[lang = "add"]` trait whose method has the wrong arity (missing rhs)
    // and no `Output` associated type must be rejected at registration time with
    // E0053, not silently accepted and degraded to an ordinary method.
    let (_, type_result, _, _) = compile(
        r#"
        #[lang = "add"]
        trait Add {
            fun add(self) -> i32;
        }

        impl Add for i32 {
            fun add(self) -> i32 { self }
        }

        fun main() -> i32 {
            let value: i32 = 1;
            value.add()
        }
        "#,
    );

    assert!(
        type_result
            .diagnostics
            .iter()
            .any(|d| d.code == "E0053" && d.message.contains("invalid trait signature")),
        "expected E0053 for malformed lang trait signature, got: {:?}",
        type_result.diagnostics,
    );
}

#[test]
fn generic_lang_operator_method_uses_builtin_after_monomorphization() {
    let module = lower(
        r#"
        #[lang = "add"]
        trait Add {
            type Output;
            fun add(self, rhs: Self) -> Self::Output;
        }

        impl Add for i32 {
            type Output = i32;
            fun add(self, rhs: Self) -> Self::Output { self + rhs }
        }

        fun sum<T: Add<Output = T>>(left: T, right: T) -> T {
            left.add(right)
        }

        fun main() -> i32 {
            sum(1, 2)
        }
        "#,
    );

    assert!(
        module
            .function_order
            .iter()
            .all(|fid| module.functions[*fid].name != "add__i32")
    );
    let sum = module
        .function_order
        .iter()
        .map(|fid| &module.functions[*fid])
        .find(|function| function.name == "sum__i32")
        .unwrap();
    assert!(
        sum.blocks
            .iter()
            .any(|(_, block)| block.insts.iter().any(|instruction| matches!(
                instruction.kind,
                mir::instr::InstKind::BinOp(mir::instr::BinOp::Add, _, _)
            )))
    );
    assert!(!sum.blocks.iter().any(|(_, block)| {
        block
            .insts
            .iter()
            .any(|instruction| matches!(instruction.kind, mir::instr::InstKind::Call(_, _)))
    }));
}

#[test]
fn generic_bound_operator_dispatches_after_monomorphization() {
    let (_, type_result, _, module) = compile(
        r#"
        #[lang = "add"]
        trait Add {
            type Output;
            fun add(self, rhs: Self) -> Self::Output;
        }

        struct Number { value: i32 }

        impl Add for Number {
            type Output = Number;
            fun add(self, rhs: Self) -> Self::Output {
                Number { value: self.value + rhs.value }
            }
        }

        fun sum<T: Add<Output = T>>(left: T, right: T) -> T {
            left + right
        }

        fun main() -> i32 {
            sum(Number { value: 1 }, Number { value: 2 }).value
        }
        "#,
    );

    assert!(
        type_result.diagnostics.is_empty(),
        "type errors: {:?}",
        type_result.diagnostics
    );
    let sum = module
        .function_order
        .iter()
        .map(|fid| &module.functions[*fid])
        .find(|function| function.name == "sum__6:Number")
        .unwrap();
    assert!(sum.blocks.iter().any(|(_, block)| {
        block.insts.iter().any(|instruction| matches!(
            &instruction.kind,
            mir::instr::InstKind::Call(mir::FuncRef::Local(name), _) if name.starts_with("add__6:Number")
        ))
    }), "{sum:#?}");
    assert!(
        !sum.blocks.iter().any(|(_, block)| {
            block.insts.iter().any(|instruction| {
                matches!(
                    instruction.kind,
                    mir::instr::InstKind::BinOp(mir::instr::BinOp::Add, _, _)
                )
            })
        }),
        "{sum:#?}"
    );
}

#[test]
fn heterogeneous_generic_operator_selects_rhs_impl() {
    let (_, type_result, _, module) = compile(
        r#"
        #[lang = "add"]
        trait Add<Rhs = Self> {
            type Output;
            fun add(self, rhs: Rhs) -> Self::Output;
        }

        struct Number { value: i32 }
        struct Delta { value: i32 }

        impl Add for Number {
            type Output = Number;
            fun add(self, rhs: Self) -> Self::Output {
                Number { value: self.value + rhs.value }
            }
        }

        impl Add<Delta> for Number {
            type Output = i32;
            fun add(self, rhs: Delta) -> Self::Output {
                self.value + rhs.value
            }
        }

        fun sum<L, R>(left: L, right: R) -> i32
        where L: Add<R, Output = i32> {
            left + right
        }

        fun main() -> i32 {
            sum(Number { value: 1 }, Delta { value: 2 })
        }
        "#,
    );

    assert!(
        type_result.diagnostics.is_empty(),
        "type errors: {:?}",
        type_result.diagnostics
    );
    let sum = module
        .function_order
        .iter()
        .map(|fid| &module.functions[*fid])
        .find(|function| function.name == "sum__6:Number_5:Delta")
        .unwrap();
    assert!(
        sum.blocks.iter().any(|(_, block)| {
            block.insts.iter().any(|instruction| {
                matches!(
                    &instruction.kind,
                    mir::instr::InstKind::Call(mir::FuncRef::Local(name), _)
                        if name == "add__6:Number_5:Delta"
                )
            })
        }),
        "{sum:#?}"
    );
}

#[test]
fn heterogeneous_operator_selects_rhs_impl_for_primitive_lhs() {
    let (_, type_result, _, module) = compile(
        r#"
        #[lang = "add"]
        trait Add<Rhs = Self> {
            type Output;
            fun add(self, rhs: Rhs) -> Self::Output;
        }

        struct Delta { value: i32 }

        impl Add<Delta> for i32 {
            type Output = i32;

            fun add(self, rhs: Delta) -> Self::Output {
                self + rhs.value
            }
        }

        fun main() -> i32 {
            1 + Delta { value: 2 }
        }
        "#,
    );

    assert!(
        type_result.diagnostics.is_empty(),
        "type errors: {:?}",
        type_result.diagnostics
    );
    let main = module
        .function_order
        .iter()
        .map(|fid| &module.functions[*fid])
        .find(|function| function.name == "main")
        .unwrap();
    assert!(
        main.blocks.iter().any(|(_, block)| {
            block.insts.iter().any(|instruction| {
                matches!(
                    &instruction.kind,
                    mir::instr::InstKind::Call(mir::FuncRef::Local(name), _)
                        if name == "add__i32_5:Delta"
                )
            })
        }),
        "{main:#?}"
    );
}

#[test]
fn heterogeneous_comparison_selects_rhs_impl_for_primitive_lhs() {
    let (_, type_result, _, module) = compile(
        r#"
        #[lang = "partial_eq"]
        trait PartialEq<Rhs = Self> {
            fun eq(&self, other: &Rhs) -> bool;
            fun ne(&self, other: &Rhs) -> bool { !self.eq(other) }
        }

        struct Delta { value: i32 }

        impl PartialEq<Delta> for i32 {
            fun eq(&self, other: &Delta) -> bool {
                *self == other.value
            }
        }

        fun main() -> bool {
            1 == Delta { value: 1 }
        }
        "#,
    );

    assert!(
        type_result.diagnostics.is_empty(),
        "type errors: {:?}",
        type_result.diagnostics
    );
    let main = module
        .function_order
        .iter()
        .map(|fid| &module.functions[*fid])
        .find(|function| function.name == "main")
        .unwrap();
    assert!(
        main.blocks.iter().any(|(_, block)| {
            block.insts.iter().any(|instruction| {
                matches!(
                    &instruction.kind,
                    mir::instr::InstKind::Call(mir::FuncRef::Local(name), _)
                        if name == "eq__i32_5:Delta"
                )
            })
        }),
        "{main:#?}"
    );
}

#[test]
fn heterogeneous_compound_assignment_selects_rhs_impl_for_primitive_lhs() {
    let (_, type_result, _, module) = compile(
        r#"
        #[lang = "add_assign"]
        trait AddAssign<Rhs = Self> {
            fun add_assign(&mut self, rhs: Rhs);
        }

        struct Delta { value: i32 }

        impl AddAssign<Delta> for i32 {
            fun add_assign(&mut self, rhs: Delta) {
                *self += rhs.value;
            }
        }

        fun main() -> i32 {
            let mut value = 1;
            value += Delta { value: 2 };
            value
        }
        "#,
    );

    assert!(
        type_result.diagnostics.is_empty(),
        "type errors: {:?}",
        type_result.diagnostics
    );
    let main = module
        .function_order
        .iter()
        .map(|fid| &module.functions[*fid])
        .find(|function| function.name == "main")
        .unwrap();
    assert!(
        main.blocks.iter().any(|(_, block)| {
            block.insts.iter().any(|instruction| {
                matches!(
                    &instruction.kind,
                    mir::instr::InstKind::Call(mir::FuncRef::Local(name), _)
                        if name == "add_assign__i32_5:Delta"
                )
            })
        }),
        "{main:#?}"
    );
}

#[test]
fn overloaded_add_lowers_to_method_call() {
    let module = lower(
        r#"
        #[lang = "add"]
        trait Add {
            type Output;
            fun add(self, rhs: Self) -> Self::Output;
        }

        impl Add for i32 {
            type Output = i32;
            fun add(self, rhs: Self) -> Self::Output {
                self + rhs
            }
        }

        struct Box<T> {
            value: T,
        }

        impl<T: Add<Output = T>> Add for Box<T> {
            type Output = T;

            fun add(self, rhs: Self) -> Self::Output {
                self.value + rhs.value
            }
        }

        fun main() {
            let a: Box<i32> = Box { value: 1 };
            let b: Box<i32> = Box { value: 2 };
            let sum = a + b;
        }
        "#,
    );
    let func = module
        .function_order
        .iter()
        .map(|fid| &module.functions[*fid])
        .find(|func| func.name == "main")
        .unwrap();
    let entry = &func.blocks[func.entry];

    assert!(entry.insts.iter().any(|i| matches!(
        &i.kind,
        mir::instr::InstKind::Call(mir::FuncRef::Local(name), _) if name.starts_with("add")
    )));
    assert!(
        !entry.insts.iter().any(|i| matches!(
            i.kind,
            mir::instr::InstKind::BinOp(mir::instr::BinOp::Add, _, _)
        )),
        "overloaded add should call Add::add, got {:?}",
        entry.insts.iter().map(|i| &i.kind).collect::<Vec<_>>()
    );
}

#[test]
fn overloaded_binary_unary_and_assign_lower_to_method_calls() {
    let module = lower(
        r#"
        #[lang = "sub"]
        trait Sub { type Output; fun sub(self, rhs: Self) -> Self::Output; }
        #[lang = "neg"]
        trait Neg { type Output; fun neg(self) -> Self::Output; }
        #[lang = "add_assign"]
        trait AddAssign { fun add_assign(&mut self, rhs: Self); }

        struct Number { value: i32 }

        impl Sub for Number {
            type Output = Number;
            fun sub(self, rhs: Self) -> Self::Output {
                Number { value: self.value - rhs.value }
            }
        }
        impl Neg for Number {
            type Output = Number;
            fun neg(self) -> Self::Output {
                Number { value: -self.value }
            }
        }
        impl AddAssign for Number {
            fun add_assign(&mut self, rhs: Self) {
                self.value += rhs.value;
            }
        }

        fun main() {
            let left = Number { value: 7 };
            let right = Number { value: 2 };
            let difference = left - right;
            let negated = -difference;
            let mut total = Number { value: 10 };
            total += negated;
        }
        "#,
    );
    let main = module
        .function_order
        .iter()
        .map(|fid| &module.functions[*fid])
        .find(|function| function.name == "main")
        .unwrap();
    let calls = main
        .blocks
        .iter()
        .flat_map(|(_, block)| &block.insts)
        .filter(|instruction| matches!(instruction.kind, mir::instr::InstKind::Call(_, _)))
        .count();

    assert_eq!(calls, 3);
}

#[test]
fn enum_variant_constructor_lowers_to_discriminant() {
    let module = lower(
        r"
        enum Option<T> {
            Some(T),
            None,
        }

        fun make() -> Option<i32> {
            Option::Some(1)
        }
        ",
    );
    let func = module
        .function_order
        .iter()
        .map(|fid| &module.functions[*fid])
        .find(|func| func.name == "make")
        .unwrap();
    let entry = &func.blocks[func.entry];

    assert!(!entry.insts.iter().any(|i| matches!(
        &i.kind,
        mir::instr::InstKind::Call(mir::FuncRef::Local(name), _) if name == "Option::Some"
    )));
}

#[test]
fn nested_enum_pattern_checks_tag_before_reading_payload() {
    let module = lower(
        r"
        enum TokenTree {
            Punct(i32),
            Ident,
        }

        enum MaybeToken {
            Some(TokenTree),
            None,
        }

        fun is_punct(value: MaybeToken) -> bool {
            match value {
                MaybeToken::Some(TokenTree::Punct(_)) => true,
                _ => false,
            }
        }
        ",
    );
    let function = module
        .functions
        .values()
        .find(|function| function.name == "is_punct")
        .expect("missing is_punct");

    assert!(
        !function.blocks[function.entry]
            .insts
            .iter()
            .any(|inst| matches!(inst.kind, mir::instr::InstKind::ExtractValue(_, 1))),
        "enum payload was read before its tag matched: {function:#?}"
    );
    assert!(
        function.blocks.iter().any(|(_, block)| {
            block.label.as_deref() == Some("match_pattern_payload")
                && block
                    .insts
                    .iter()
                    .any(|inst| matches!(inst.kind, mir::instr::InstKind::ExtractValue(_, 1)))
        }),
        "missing guarded enum payload read: {function:#?}"
    );
}

#[test]
fn compound_assignment_lowers_to_load_binop_store() {
    let module = lower(
        r"
        fun main() {
            let mut n: i32 = 1;
            n += 2;
        }
        ",
    );
    let func = &module.functions[module.function_order[0]];
    let entry = &func.blocks[func.entry];

    assert!(
        entry
            .insts
            .iter()
            .any(|i| matches!(i.kind, mir::instr::InstKind::Load(_)))
    );
    assert!(entry.insts.iter().any(|i| matches!(
        i.kind,
        mir::instr::InstKind::BinOp(mir::instr::BinOp::Add, _, _)
    )));
    assert!(
        entry
            .insts
            .iter()
            .any(|i| matches!(i.kind, mir::instr::InstKind::Store(_, _)))
    );
}

#[test]
fn assignment_evaluates_rhs_before_lhs_place() {
    let module = lower(
        r"
        fun lhs_index() -> i32 { 0 }
        fun rhs_value() -> i32 { 1 }

        fun main() {
            let mut values = [0];
            values[lhs_index()] = rhs_value();
        }
        ",
    );
    let main = module
        .function_order
        .iter()
        .map(|fid| &module.functions[*fid])
        .find(|function| function.name == "main")
        .unwrap();
    let calls = main
        .blocks
        .iter()
        .flat_map(|(_, block)| &block.insts)
        .filter_map(|instruction| match &instruction.kind {
            mir::instr::InstKind::Call(mir::FuncRef::Local(name), _) => Some(name.as_str()),
            _ => None,
        })
        .collect::<Vec<_>>();

    assert_eq!(calls, ["rhs_value", "lhs_index"]);
}

#[test]
fn primitive_compound_assignment_evaluates_rhs_before_lhs_place() {
    let module = lower(
        r"
        fun lhs_index() -> i32 { 0 }
        fun rhs_value() -> i32 { 1 }

        fun main() {
            let mut values = [0];
            values[lhs_index()] += rhs_value();
        }
        ",
    );
    let main = module
        .function_order
        .iter()
        .map(|fid| &module.functions[*fid])
        .find(|function| function.name == "main")
        .unwrap();
    let calls = main
        .blocks
        .iter()
        .flat_map(|(_, block)| &block.insts)
        .filter_map(|instruction| match &instruction.kind {
            mir::instr::InstKind::Call(mir::FuncRef::Local(name), _) => Some(name.as_str()),
            _ => None,
        })
        .collect::<Vec<_>>();

    assert_eq!(calls, ["rhs_value", "lhs_index"]);
}

#[test]
fn overloaded_compound_assignment_evaluates_lhs_before_rhs() {
    let module = lower(
        r#"
        #[lang = "add_assign"]
        trait AddAssign { fun add_assign(&mut self, rhs: Self); }

        struct Number { value: i32 }

        impl AddAssign for Number {
            fun add_assign(&mut self, rhs: Self) {
                self.value += rhs.value;
            }
        }

        fun lhs_index() -> i32 { 0 }
        fun rhs_value() -> Number { Number { value: 1 } }

        fun main() {
            let mut values = [Number { value: 0 }];
            values[lhs_index()] += rhs_value();
        }
        "#,
    );
    let main = module
        .function_order
        .iter()
        .map(|fid| &module.functions[*fid])
        .find(|function| function.name == "main")
        .unwrap();
    let calls = main
        .blocks
        .iter()
        .flat_map(|(_, block)| &block.insts)
        .filter_map(|instruction| match &instruction.kind {
            mir::instr::InstKind::Call(mir::FuncRef::Local(name), _) => Some(name.as_str()),
            _ => None,
        })
        .collect::<Vec<_>>();

    assert!(
        calls[0] == "lhs_index" && calls[1] == "rhs_value",
        "{calls:?}"
    );
}

#[test]
fn struct_literal() {
    let module = lower(
        r"
        struct Point { x: i32, y: i32 }

        fun make() -> Point {
            let p = Point { x: 1, y: 2 };
            return p;
        }
        ",
    );
    let func = &module.functions[module.function_order[0]];
    assert!(!func.blocks[func.entry].insts.is_empty());
}

#[test]
fn field_access() {
    let module = lower(
        r"
        struct Point { x: i32, y: i32 }

        fun get_x(p: &Point) -> i32 {
            return p.x;
        }
        ",
    );
    let func = &module.functions[module.function_order[0]];
    assert!(!func.blocks[func.entry].insts.is_empty());
}

#[test]
fn function_call() {
    let module = lower(
        r"
        fun square(n: i32) -> i32 {
            return n * n;
        }

        fun main() -> i32 {
            return square(5);
        }
        ",
    );
    let func = &module.functions[module.function_order[1]];
    assert!(!func.blocks[func.entry].insts.is_empty());
}

#[test]
fn multiple_functions() {
    let module = lower(
        r"
        fun a() {}
        fun b() {}
        fun c() {}
        ",
    );
    assert_eq!(module.function_order.len(), 3);
}

#[test]
fn empty_function() {
    let module = lower(
        r"
        fun nothing() {}
        ",
    );
    let func = &module.functions[module.function_order[0]];
    let entry = &func.blocks[func.entry];
    // 空函数应当至少有一个 return 终止指令
    assert!(
        matches!(entry.terminator, mir::instr::Terminator::Return(_)),
        "empty function should end with return"
    );
}

#[test]
fn comparison_operators() {
    let module = lower(
        r"
        fun cmp(a: i32, b: i32) -> bool {
            return a < b;
        }
        ",
    );
    let func = &module.functions[module.function_order[0]];
    assert!(!func.blocks[func.entry].insts.is_empty());
}

#[test]
fn bool_literal() {
    let module = lower(
        r"
        fun truth() -> bool {
            return true;
        }
        ",
    );
    let func = &module.functions[module.function_order[0]];
    assert!(!func.blocks[func.entry].insts.is_empty());
}

#[test]
fn escape_analysis_affects_allocation() {
    let module = lower(
        r"
        struct Data { value: i32 }

        fun keep() {
            let local = Data { value: 1 };
            // local 不逃逸，应当栈分配
        }

        fun escape() -> &Data {
            let local = Data { value: 1 };
            return &local;
            // local 逃逸，应当堆分配
        }
        ",
    );
    assert_eq!(module.function_order.len(), 2);
}

#[test]
fn param_used_in_return() {
    let module = lower(
        r"
        fun identity(n: i32) -> i32 {
            return n;
        }
        ",
    );
    let func = &module.functions[module.function_order[0]];
    let entry = &func.blocks[func.entry];
    // Should contain "return" with the param value, not unit
    assert!(
        matches!(entry.terminator, mir::instr::Terminator::Return(Some(_))),
        "param should be used in return, got {:?}",
        entry.terminator
    );
}

#[test]
fn param_used_in_expression() {
    let module = lower(
        r"
        fun double(n: i32) -> i32 {
            let d = n + n;
            return d;
        }
        ",
    );
    let func = &module.functions[module.function_order[0]];
    let entry = &func.blocks[func.entry];
    assert!(entry.insts.len() >= 2, "expected param load + add + return");
    assert!(
        matches!(entry.terminator, mir::instr::Terminator::Return(Some(_))),
        "should return a value, got {:?}",
        entry.terminator
    );
}

#[test]
fn local_var_used_as_init() {
    let module = lower(
        r"
        fun f() -> i32 {
            let x = 42;
            let y = x;
            return y;
        }
        ",
    );
    let func = &module.functions[module.function_order[0]];
    let entry = &func.blocks[func.entry];
    assert!(
        matches!(entry.terminator, mir::instr::Terminator::Return(Some(_))),
        "local var chain should resolve, got {:?}",
        entry.terminator
    );
}

#[test]
fn delayed_let_allocates_storage_and_assignment_stores_value() {
    let module = lower(
        r"
        fun f() -> i32 {
            let x: i32;
            x = 7;
            return x;
        }
        ",
    );
    let func = &module.functions[module.function_order[0]];
    assert_eq!(func.name, "f");
    assert!(
        func.blocks.iter().any(|(_, block)| block
            .insts
            .iter()
            .any(|inst| matches!(inst.kind, mir::instr::InstKind::Alloca(_)))),
        "delayed bindings need storage"
    );
    assert!(
        func.blocks.iter().any(|(_, block)| block
            .insts
            .iter()
            .any(|inst| matches!(inst.kind, mir::instr::InstKind::Store(_, _)))),
        "the first assignment must initialize the storage"
    );
}

#[test]
fn two_params_both_used() {
    let module = lower(
        r"
        fun add(a: i32, b: i32) -> i32 {
            return a + b;
        }
        ",
    );
    let func = &module.functions[module.function_order[0]];
    assert_eq!(func.params.len(), 2);
    let entry = &func.blocks[func.entry];
    assert!(
        entry
            .insts
            .iter()
            .any(|i| matches!(i.kind, mir::instr::InstKind::BinOp(..))),
        "expected a BinOp instruction for a + b, got {:?}",
        entry.insts.iter().map(|i| &i.kind).collect::<Vec<_>>()
    );
}

#[test]
fn escaping_local_produces_heap_alloc_instruction() {
    let module = lower(
        r"
        struct Data { value: i32 }

        fun escape() -> &Data {
            let local = Data { value: 1 };
            return &local;
        }
        ",
    );
    let func = &module.functions[module.function_order[0]];
    let entry = &func.blocks[func.entry];
    let has_heap_alloc = entry
        .insts
        .iter()
        .any(|i| matches!(i.kind, mir::instr::InstKind::HeapAlloc(_)));
    assert!(
        has_heap_alloc,
        "escaping local should produce HeapAlloc, got: {:?}",
        entry.insts.iter().map(|i| &i.kind).collect::<Vec<_>>()
    );
}

#[test]
fn non_escaping_reference_temporary_uses_stack_storage() {
    let module = lower(
        r"
        fun read() -> i32 {
            let value = 1;
            **&&value
        }
        ",
    );
    let func = &module.functions[module.function_order[0]];
    assert!(
        func.blocks
            .values()
            .flat_map(|block| &block.insts)
            .any(|inst| matches!(inst.kind, mir::instr::InstKind::Alloca(_))),
        "a non-escaping reference temporary needs stack storage: {func:#?}"
    );
    assert!(
        !func
            .blocks
            .values()
            .flat_map(|block| &block.insts)
            .any(|inst| matches!(inst.kind, mir::instr::InstKind::HeapAlloc(_))),
        "a local-only reference temporary must not escape: {func:#?}"
    );
}

#[test]
fn reference_to_unit_enum_variant_materializes_storage() {
    let module = lower(
        r"
        enum State { Ready }

        fun inspect(value: &State) {}

        fun main() {
            inspect(&State::Ready);
        }
        ",
    );
    let main = module
        .function_order
        .iter()
        .map(|id| &module.functions[*id])
        .find(|function| function.name == "main")
        .unwrap();
    assert!(
        main.blocks
            .values()
            .flat_map(|block| &block.insts)
            .any(|inst| matches!(inst.kind, mir::instr::InstKind::Alloca(_))),
        "a referenced unit enum temporary needs storage: {main:#?}"
    );
}

#[test]
fn colliding_trait_method_symbols_are_disambiguated() {
    let module = lower(
        r"
        trait First { fun fmt(&self) -> i32; }
        trait Second { fun fmt(&self) -> i32; }

        impl First for i32 { fun fmt(&self) -> i32 { 1 } }
        impl Second for i32 { fun fmt(&self) -> i32 { 2 } }

        fun first<T: First>(value: &T) -> i32 { value.fmt() }
        fun second<T: Second>(value: &T) -> i32 { value.fmt() }

        fun main() -> i32 {
            let value = 0;
            first(&value) + second(&value)
        }
        ",
    );
    let names = module
        .function_order
        .iter()
        .map(|id| module.functions[*id].name.as_str())
        .filter(|name| name.starts_with("fmt__"))
        .collect::<Vec<_>>();
    assert_eq!(names.len(), 2, "{names:?}");
    assert_ne!(names[0], names[1]);
}

#[test]
fn escaping_destructured_binding_promotes_only_the_escaping_field() {
    let module = lower(
        r"
        struct Data { value: i32 }

        fun escape() -> &Data {
            let (first, second) = (Data { value: 1 }, Data { value: 2 });
            return &first;
        }
        ",
    );
    let func = &module.functions[module.function_order[0]];
    let entry = &func.blocks[func.entry];
    let heap_allocs = entry
        .insts
        .iter()
        .filter(|i| matches!(i.kind, mir::instr::InstKind::HeapAlloc(_)))
        .count();
    assert!(
        heap_allocs == 1,
        "a reference to one destructured binding should promote only its field, got: {:?}",
        entry.insts
    );
}

#[test]
fn tuple_destructuring_only_promotes_the_escaping_reference_source() {
    let module = lower(
        r"
        fun escape_second() -> &mut i32 {
            let mut first = 1;
            let mut second = 2;
            let (first_ref, second_ref) = (&mut first, &mut second);
            return second_ref;
        }
        ",
    );
    let func = &module.functions[module.function_order[0]];
    let heap_allocs = func
        .blocks
        .values()
        .flat_map(|block| &block.insts)
        .filter(|inst| matches!(inst.kind, mir::instr::InstKind::HeapAlloc(_)))
        .count();
    assert_eq!(heap_allocs, 1, "only `second` should escape: {func:#?}");
}

#[test]
fn returned_tuple_destructuring_only_promotes_the_escaping_reference_source() {
    let module = lower(
        r"
        fun pair(first: &mut i32, second: &mut i32) -> (&mut i32, &mut i32) {
            (first, second)
        }

        fun escape_second() -> &mut i32 {
            let mut first = 1;
            let mut second = 2;
            let (first_ref, second_ref) = pair(&mut first, &mut second);
            return second_ref;
        }
        ",
    );
    let func = module
        .function_order
        .iter()
        .map(|id| &module.functions[*id])
        .find(|function| function.name == "escape_second")
        .unwrap();
    let heap_allocs = func
        .blocks
        .values()
        .flat_map(|block| &block.insts)
        .filter(|inst| matches!(inst.kind, mir::instr::InstKind::HeapAlloc(_)))
        .count();
    assert_eq!(heap_allocs, 1, "only `second` should escape: {func:#?}");
}

#[test]
fn non_escaping_destructured_bindings_stay_on_the_stack() {
    let module = lower(
        r"
        struct Data { value: i32 }

        fun keep() -> i32 {
            let (first, second) = (Data { value: 1 }, Data { value: 2 });
            first.value + second.value
        }
        ",
    );
    let func = &module.functions[module.function_order[0]];
    let entry = &func.blocks[func.entry];
    let has_heap_alloc = entry
        .insts
        .iter()
        .any(|i| matches!(i.kind, mir::instr::InstKind::HeapAlloc(_)));
    assert!(
        !has_heap_alloc,
        "destructuring alone must not force a heap allocation"
    );
}

#[test]
fn non_escaping_local_no_heap_alloc() {
    let module = lower(
        r"
        struct Data { value: i32 }

        fun keep() {
            let local = Data { value: 1 };
            // local doesn't escape — must be stack allocated (no HeapAlloc)
        }
        ",
    );
    let func = &module.functions[module.function_order[0]];
    let entry = &func.blocks[func.entry];
    let has_heap_alloc = entry
        .insts
        .iter()
        .any(|i| matches!(i.kind, mir::instr::InstKind::HeapAlloc(_)));
    assert!(
        !has_heap_alloc,
        "non-escaping local should NOT produce HeapAlloc"
    );
}

#[test]
fn returned_reference_consumed_by_caller_stays_on_stack() {
    let module = lower(
        r"
        struct Data { value: i32 }

        fun forward(value: &Data) -> &Data { value }

        fun read_forwarded() -> i32 {
            let local = Data { value: 42 };
            (*forward(&local)).value
        }
        ",
    );
    let function = module
        .function_order
        .iter()
        .map(|id| &module.functions[*id])
        .find(|function| function.name == "read_forwarded")
        .unwrap();

    assert!(
        !function.blocks.iter().any(|(_, block)| {
            block
                .insts
                .iter()
                .any(|inst| matches!(inst.kind, mir::instr::InstKind::HeapAlloc(_)))
        }),
        "a returned reference consumed inside the caller must not promote its source"
    );
}

#[test]
fn lambda_return_does_not_leak_into_outer_function_summary() {
    let module = lower(
        r"
        struct Data { value: i32 }

        fun read_in_lambda(value: &Data) -> i32 {
            let read = [ -> { return value; }];
            (*read()).value
        }

        fun caller() -> i32 {
            let local = Data { value: 42 };
            read_in_lambda(&local)
        }
        ",
    );

    for name in ["read_in_lambda", "caller"] {
        let function = module
            .function_order
            .iter()
            .map(|id| &module.functions[*id])
            .find(|function| function.name == name)
            .unwrap();
        assert!(
            !function.blocks.iter().any(|(_, block)| {
                block
                    .insts
                    .iter()
                    .any(|inst| matches!(inst.kind, mir::instr::InstKind::HeapAlloc(_)))
            }),
            "{name} should not inherit a nested lambda's return summary"
        );
    }
}

#[test]
fn reference_returned_through_local_lambda_still_escapes() {
    let module = lower(
        r"
        struct Data { value: i32 }

        fun relay(value: &Data) -> &Data {
            let return_value = [ -> value];
            return_value()
        }

        fun caller() -> &Data {
            let local = Data { value: 42 };
            relay(&local)
        }
        ",
    );
    let function = module
        .function_order
        .iter()
        .map(|id| &module.functions[*id])
        .find(|function| function.name == "caller")
        .unwrap();

    assert!(
        function.blocks.iter().any(|(_, block)| {
            block
                .insts
                .iter()
                .any(|inst| matches!(inst.kind, mir::instr::InstKind::HeapAlloc(_)))
        }),
        "a reference returned through a local lambda must keep its source alive"
    );
}

const REFERENCE_PROMOTION_SOURCE: &str = r#"
        struct Data { value: i32 }
        struct Receiver { value: i32 }
        struct Other { value: i64 }
        struct Slot { value: &Data }
        struct Holder { value: &Data }

        fun direct_mut() -> &mut Data {
            let mut local = Data { value: 1 };
            &mut local
        }

        fun field_ref() -> &i32 {
            let local = Data { value: 1 };
            &local.value
        }

        fun param_field_ref(value: &Data) -> &i32 { &value.value }

        fun alias_field_ref() -> &i32 {
            let local = Data { value: 1 };
            let alias = &local;
            &alias.value
        }

        fun mutable_alias_field_ref() -> &i32 {
            let local = Data { value: 1 };
            let mut alias = &local;
            &alias.value
        }

        fun index_ref() -> &Data {
            let items = [Data { value: 1 }, Data { value: 2 }];
            &items[0]
        }

        fun mutable_alias_index_ref() -> &Data {
            let items = [Data { value: 1 }, Data { value: 2 }];
            let mut alias = &items;
            &(*alias)[0]
        }

        fun mutable_alias_deref_ref() -> &Data {
            let local = Data { value: 1 };
            let mut alias = &local;
            &*alias
        }

        fun block_ref() -> &Data {
            let local = Data { value: 1 };
            { &local }
        }

        fun by_value_param_ref(value: Data) -> &Data { &value }

        fun identity<T>(value: T) -> T { value }

        fun generic_before_param_ref(value: Data) -> &Data {
            let ignored = identity(1);
            &value
        }

        fun branch_refs(flag: bool) -> &Data {
            let left = Data { value: 1 };
            let right = Data { value: 2 };
            if flag { &left } else { &right }
        }

        struct Pair {
            left: &Data,
            right: &Data,
        }

        fun aggregate_refs() -> Pair {
            let left = Data { value: 1 };
            let right = Data { value: 2 };
            Pair { left: &left, right: &right }
        }

        fun reassigned_ref(flag: bool) -> &Data {
            let left = Data { value: 1 };
            let right = Data { value: 2 };
            let mut reference = &left;
            if flag { reference = &right; }
            reference
        }

        impl Data {
            fun value_ref(&self) -> &i32 { &self.value }
        }

        impl Receiver {
            fun choose(receiver: &Receiver, other: &Other) -> &Other { other }
        }

        fun method_ref() -> &i32 {
            let local = Data { value: 1 };
            local.value_ref()
        }

        fun method_alias_ref() -> &i32 {
            let local = Data { value: 1 };
            let alias = &local;
            alias.value_ref()
        }

        fun method_arg_ref() -> &Other {
            let receiver = Receiver { value: 1 };
            let other = Other { value: 2 };
            receiver.choose(&other)
        }

        fun loop_backedge(flag: bool) -> &Data {
            let first = Data { value: 1 };
            let later = Data { value: 2 };
            let mut current = &first;
            let mut escaped = current;
            while flag {
                escaped = current;
                current = &later;
            }
            escaped
        }

        fun indirect_store(slot: &mut Slot) {
            let local = Data { value: 1 };
            slot.value = &local;
        }

        fun deref_store(slot: &mut &Data) {
            let local = Data { value: 1 };
            *slot = &local;
        }

        fun consume_holder(holder: Holder) -> i32 { holder.value.value }
        fun read_holder(holder: &Holder) -> i32 { holder.value.value }

        fun value_capture() -> impl Fn() -> i32 {
            let local = Data { value: 1 };
            let holder = Holder { value: &local };
            [ -> read_holder(&holder)]
        }

        fun lambda_param_ref() -> impl Fn(Data) -> &Data {
            [value: Data -> &value]
        }

        fun read(value: &Data) -> i32 { value.value }

        fun nonescaping_call() -> i32 {
            let local = Data { value: 1 };
            read(&local)
        }

        unsafe extern "C" {
            fun store(value: &Data);
        }

        fun escaping_call() {
            let local = Data { value: 1 };
            unsafe { store({ &local }); }
        }

        fun loop_call_sink(flag: bool) {
            let first = Data { value: 1 };
            let later = Data { value: 2 };
            let mut reference = &first;
            while flag {
                unsafe { store(reference); }
                reference = &later;
            }
        }
        "#;

#[test]
fn all_reference_forms_promote_their_source_local() {
    let (_, type_result, _, module) = compile(REFERENCE_PROMOTION_SOURCE);
    assert!(
        type_result.diagnostics.is_empty(),
        "type errors: {:?}",
        type_result.diagnostics
    );
    assert_reference_allocation_counts(&module);
    assert_reference_places_are_heap_allocated(&module);
    assert_reference_closure_allocations(&module);
}

fn assert_reference_allocation_counts(module: &mir::Module) {
    for (name, expected_allocations) in [
        ("direct_mut", 1),
        ("field_ref", 1),
        ("param_field_ref", 0),
        ("alias_field_ref", 1),
        ("mutable_alias_field_ref", 1),
        ("index_ref", 1),
        ("mutable_alias_index_ref", 1),
        ("mutable_alias_deref_ref", 1),
        ("block_ref", 1),
        ("by_value_param_ref", 1),
        ("generic_before_param_ref", 1),
        ("branch_refs", 2),
        ("aggregate_refs", 2),
        ("reassigned_ref", 2),
        ("method_ref", 1),
        ("loop_backedge", 2),
        ("indirect_store", 1),
        ("deref_store", 1),
        ("nonescaping_call", 0),
        ("escaping_call", 1),
        ("loop_call_sink", 2),
        ("method_alias_ref", 1),
    ] {
        let function = reference_function(module, name);
        let allocations = function
            .blocks
            .iter()
            .flat_map(|(_, block)| &block.insts)
            .filter(|inst| matches!(inst.kind, mir::instr::InstKind::HeapAlloc(_)))
            .count();
        assert_eq!(
            allocations, expected_allocations,
            "{name} should heap-allocate every escaping local"
        );
    }

    let method = reference_function(module, "method_arg_ref");
    let allocated_structs: Vec<_> = method
        .blocks
        .iter()
        .flat_map(|(_, block)| &block.insts)
        .filter_map(|inst| match &inst.kind {
            mir::instr::InstKind::HeapAlloc(mir::types::Type::Ptr(ty)) => match ty.as_ref() {
                mir::types::Type::Struct(ty) => Some(ty.name()),
                _ => None,
            },
            _ => None,
        })
        .collect();
    assert_eq!(allocated_structs, ["Other"]);
}

fn assert_reference_places_are_heap_allocated(module: &mir::Module) {
    let method_ref = reference_function(module, "method_ref");
    let find_inst = |value: mir::Value| {
        method_ref.blocks.iter().find_map(|(_, block)| {
            let index = value.0.checked_sub(block.start_value)? as usize;
            block.insts.get(index)
        })
    };
    let receiver = method_ref
        .blocks
        .iter()
        .flat_map(|(_, block)| &block.insts)
        .find_map(|inst| match &inst.kind {
            mir::instr::InstKind::Call(_, args) => args.first().copied(),
            _ => None,
        })
        .unwrap();
    let receiver_place = match &find_inst(receiver).unwrap().kind {
        mir::instr::InstKind::UnOp(mir::instr::UnOp::Ref, place) => *place,
        inst => panic!("method receiver should be borrowed from a place, got {inst:?}"),
    };
    assert!(matches!(
        find_inst(receiver_place).unwrap().kind,
        mir::instr::InstKind::HeapAlloc(_)
    ));

    let index_ref = reference_function(module, "index_ref");
    let index_base = index_ref
        .blocks
        .iter()
        .flat_map(|(_, block)| &block.insts)
        .find_map(|inst| match inst.kind {
            mir::instr::InstKind::IndexPtr(base, _)
            | mir::instr::InstKind::CheckedIndexPtr(base, _, _) => Some(base),
            _ => None,
        })
        .unwrap();
    let index_base_inst = index_ref
        .blocks
        .iter()
        .find_map(|(_, block)| {
            let index = index_base.0.checked_sub(block.start_value)? as usize;
            block.insts.get(index)
        })
        .unwrap();
    assert!(matches!(
        index_base_inst.kind,
        mir::instr::InstKind::HeapAlloc(_)
    ));
}

fn assert_reference_closure_allocations(module: &mir::Module) {
    let closure = reference_function(module, "value_capture");
    assert!(
        closure
            .blocks
            .iter()
            .flat_map(|(_, block)| &block.insts)
            .any(|inst| matches!(
                &inst.kind,
                mir::instr::InstKind::HeapAlloc(mir::types::Type::Ptr(ty))
                    if matches!(ty.as_ref(), mir::types::Type::Struct(ty) if ty.name() == "Data")
            )),
        "a value-captured aggregate must keep its referenced local alive"
    );

    let lambda = module
        .function_order
        .iter()
        .map(|id| &module.functions[*id])
        .find(|function| {
            function.name.starts_with("__riddle_lambda_")
                && matches!(
                    &function.ret_type,
                    mir::types::Type::Ref(ty, _)
                        if matches!(ty.as_ref(), mir::types::Type::Struct(ty) if ty.name() == "Data")
                )
        })
        .unwrap();
    assert!(
        lambda
            .blocks
            .iter()
            .flat_map(|(_, block)| &block.insts)
            .any(|inst| matches!(inst.kind, mir::instr::InstKind::HeapAlloc(_)))
    );

    let generic_caller = reference_function(module, "generic_before_param_ref");
    let borrowed = generic_caller
        .blocks
        .iter()
        .flat_map(|(_, block)| &block.insts)
        .find_map(|inst| match inst.kind {
            mir::instr::InstKind::UnOp(mir::instr::UnOp::Ref, place) => Some(place),
            _ => None,
        })
        .unwrap();
    assert!(generic_caller.blocks.iter().any(|(_, block)| {
        let index = borrowed
            .0
            .checked_sub(block.start_value)
            .map(|index| index as usize);
        index
            .and_then(|index| block.insts.get(index))
            .is_some_and(|inst| matches!(inst.kind, mir::instr::InstKind::HeapAlloc(_)))
    }));
}

fn reference_function<'a>(module: &'a mir::Module, name: &str) -> &'a mir::Function {
    module
        .function_order
        .iter()
        .map(|id| &module.functions[*id])
        .find(|function| function.name == name)
        .unwrap()
}

#[test]
fn overloaded_operator_uses_parameter_escape_summary() {
    let (_, type_result, _, module) = compile(
        r#"
        struct LeftData { value: i32 }
        struct RightData { value: i32 }
        struct Left { value: &LeftData }
        struct Right { value: &RightData }

        #[lang = "add"]
        trait Add {
            type Output;
            fun add(self, rhs: Right) -> Self::Output;
        }

        impl Add for Left {
            type Output = &RightData;

            fun add(self, rhs: Right) -> Self::Output {
                rhs.value
            }
        }

        fun overloaded_add_ref() -> &RightData {
            let left_data = LeftData { value: 1 };
            let right_data = RightData { value: 2 };
            let left = Left { value: &left_data };
            let right = Right { value: &right_data };
            left + right
        }
        "#,
    );
    assert!(
        type_result.diagnostics.is_empty(),
        "type errors: {:?}",
        type_result.diagnostics
    );
    assert_eq!(type_result.operator_calls.len(), 1);

    let function = module
        .function_order
        .iter()
        .map(|id| &module.functions[*id])
        .find(|function| function.name == "overloaded_add_ref")
        .unwrap();
    let allocated_structs = function
        .blocks
        .iter()
        .flat_map(|(_, block)| &block.insts)
        .filter_map(|inst| match &inst.kind {
            mir::instr::InstKind::HeapAlloc(mir::types::Type::Ptr(ty)) => match ty.as_ref() {
                mir::types::Type::Struct(ty) => Some(ty.name()),
                _ => None,
            },
            _ => None,
        })
        .collect::<Vec<_>>();
    assert_eq!(allocated_structs, ["RightData"]);
}

#[test]
fn generic_trait_operator_result_preserves_reference_escape() {
    let (_, type_result, analysis, module) = compile(
        r#"
        #[lang = "add"]
        trait Add {
            type Output;
            fun add(self, rhs: Self) -> Self::Output;
        }

        struct S { r: &i32 }

        impl Add for S {
            type Output = S;
            fun add(self, rhs: Self) -> Self::Output { self }
        }

        fun f<T: Add<Output = T>>(value: T) -> T {
            value + value
        }

        fun make() -> S {
            let x = 573;
            let s = S { r: &x };
            f(s)
        }
        "#,
    );
    assert!(
        type_result.diagnostics.is_empty(),
        "type errors: {:?}",
        type_result.diagnostics
    );
    assert!(
        analysis.diagnostics.is_empty(),
        "analysis errors: {:?}",
        analysis.diagnostics
    );

    let make = module
        .functions
        .values()
        .find(|function| function.name == "make")
        .expect("missing make");
    assert!(
        make.blocks.iter().any(|(_, block)| block
            .insts
            .iter()
            .any(|inst| matches!(inst.kind, mir::instr::InstKind::HeapAlloc(_)))),
        "the referenced local must be promoted: {make:#?}"
    );
}

#[test]
fn reference_to_raw_pointer_cast_preserves_address() {
    let (_, type_result, analysis, module) = compile(
        r"
        fun read_address() -> i32 {
            let x = 42;
            let pointer = &x as *const i32;
            unsafe { *pointer }
        }
        ",
    );
    assert!(
        type_result.diagnostics.is_empty(),
        "type errors: {:?}",
        type_result.diagnostics
    );
    assert!(
        analysis.diagnostics.is_empty(),
        "analysis errors: {:?}",
        analysis.diagnostics
    );

    let function = module
        .functions
        .values()
        .find(|function| function.name == "read_address")
        .expect("missing read_address");
    assert!(
        function
            .blocks
            .iter()
            .any(|(_, block)| block.insts.iter().any(|inst| matches!(
                inst.kind,
                mir::instr::InstKind::Cast(mir::instr::CastOp::PtrToPtr, _, _)
            ))),
        "the cast must preserve the reference address: {function:#?}"
    );
}

#[test]
fn pattern_bindings_preserve_reference_sources() {
    let (_, type_result, _, module) = compile(
        r"
        struct Data { value: i32 }
        struct Holder { value: &Data }

        fun match_pattern_ref() -> &Data {
            let local = Data { value: 1 };
            match &local {
                value => value
            }
        }

        fun for_pattern_ref(fallback: &Data) -> &Data {
            let local = Data { value: 2 };
            for item in [&local] {
                return item;
            }
            fallback
        }

        fun shorthand_pattern_ref() -> &Data {
            let local = Data { value: 3 };
            let holder = Holder { value: &local };
            match holder {
                Holder { value } => value
            }
        }
        ",
    );
    assert!(
        type_result.diagnostics.is_empty(),
        "type errors: {:?}",
        type_result.diagnostics
    );

    for name in [
        "match_pattern_ref",
        "for_pattern_ref",
        "shorthand_pattern_ref",
    ] {
        let function = module
            .function_order
            .iter()
            .map(|id| &module.functions[*id])
            .find(|function| function.name == name)
            .unwrap();
        assert_eq!(
            function
                .blocks
                .iter()
                .flat_map(|(_, block)| &block.insts)
                .filter(|inst| matches!(inst.kind, mir::instr::InstKind::HeapAlloc(_)))
                .count(),
            1,
            "{name} should heap-allocate the referenced local"
        );
    }
}

#[test]
fn ergonomic_pattern_bindings_lower_as_references() {
    let (_, type_result, analysis, module) = compile(
        r"
        fun shared(value: &(i32, i32)) -> i32 {
            let (left, right) = value;
            *left + *right
        }

        fun mutable(value: &mut (i32, i32)) -> i32 {
            let (left, right) = value;
            *left = 10;
            *right = 20;
            *left + *right
        }

        fun explicit(value: &mut i32) -> i32 {
            let &mut copy = value;
            copy
        }
        ",
    );
    assert_eq!(type_result.diagnostics, vec![]);
    assert_eq!(analysis.diagnostics, vec![]);

    let has_unop = |name: &str, expected| {
        module
            .function_order
            .iter()
            .map(|id| &module.functions[*id])
            .find(|function| function.name == name)
            .unwrap()
            .blocks
            .values()
            .flat_map(|block| &block.insts)
            .any(|inst| matches!(inst.kind, mir::instr::InstKind::UnOp(op, _) if op == expected))
    };
    assert!(has_unop("shared", mir::instr::UnOp::Ref));
    assert!(has_unop("mutable", mir::instr::UnOp::MutRef));
    assert!(!has_unop("explicit", mir::instr::UnOp::MutRef));
}

#[test]
fn returned_ergonomic_pattern_reference_promotes_its_source() {
    let module = lower(
        r"
        fun first() -> &i32 {
            let pair = (10, 20);
            let (first, second) = &pair;
            first
        }
        ",
    );
    let function = module
        .function_order
        .iter()
        .map(|id| &module.functions[*id])
        .find(|function| function.name == "first")
        .unwrap();
    assert_eq!(
        function
            .blocks
            .values()
            .flat_map(|block| &block.insts)
            .filter(|inst| matches!(inst.kind, mir::instr::InstKind::HeapAlloc(_)))
            .count(),
        1,
        "the referenced tuple must outlive the function: {function:#?}"
    );
}

#[test]
fn pos_unary_is_noop() {
    let module = lower(
        r"
        fun f(x: i32) -> i32 {
            let y = +x;
            return y;
        }
        ",
    );
    let func = &module.functions[module.function_order[0]];
    let entry = &func.blocks[func.entry];
    // +x should not produce a Neg instruction
    let has_neg = entry
        .insts
        .iter()
        .any(|i| matches!(i.kind, mir::instr::InstKind::UnOp(mir::instr::UnOp::Neg, _)));
    assert!(!has_neg, "+x should not produce Neg instruction");
    assert!(
        matches!(entry.terminator, mir::instr::Terminator::Return(Some(_))),
        "should return a value, got {:?}",
        entry.terminator
    );
}

#[test]
fn anonymous_function_lowers_to_function_pointer_call() {
    let module = lower(
        r"
        fun apply(f: impl Fn(i32) -> i32, value: i32) -> i32 {
            f(value)
        }

        fun main() -> i32 {
            let inc = [x -> x + 1];
            apply(inc, 41)
        }
        ",
    );

    assert!(
        module
            .function_order
            .iter()
            .any(|id| module.functions[*id].name.starts_with("__riddle_lambda_"))
    );
    let apply = module
        .function_order
        .iter()
        .map(|id| &module.functions[*id])
        .find(|function| function.name.starts_with("apply__"))
        .unwrap();
    assert!(apply.blocks.iter().any(|(_, block)| {
        block
            .insts
            .iter()
            .any(|inst| matches!(inst.kind, mir::instr::InstKind::CallIndirect(..)))
    }));
}

#[test]
fn non_escaping_closure_keeps_environment_and_capture_on_stack() {
    let module = lower(
        r"
        fun main() -> i32 {
            let base = 40;
            let add = [value: i32 -> base + value];
            add(2)
        }
        ",
    );

    let lambda = module
        .function_order
        .iter()
        .map(|id| &module.functions[*id])
        .find(|function| function.name.starts_with("__riddle_lambda_"))
        .unwrap();
    assert!(matches!(lambda.params[0].ty, mir::types::Type::Ptr(_)));
    let main = module
        .function_order
        .iter()
        .map(|id| &module.functions[*id])
        .find(|function| function.name == "main")
        .unwrap();
    assert!(
        !main.blocks.iter().any(|(_, block)| {
            block
                .insts
                .iter()
                .any(|inst| matches!(inst.kind, mir::instr::InstKind::HeapAlloc(_)))
        }),
        "a closure called within its defining frame should not allocate on the heap"
    );
    assert!(
        main.blocks.iter().any(|(_, block)| {
            block
                .insts
                .iter()
                .filter(|inst| matches!(inst.kind, mir::instr::InstKind::Alloca(_)))
                .count()
                >= 2
        }),
        "the captured local and closure environment need stack storage"
    );
}

#[test]
fn lambda_returned_from_lambda_uses_heap_environment() {
    let module = lower(
        r"
        fun nested(base: i32) -> impl Fn(i32) -> impl Fn(i32) -> i32 {
            [first: i32 -> [second: i32 -> base + first + second]]
        }
        ",
    );

    assert!(
        module
            .function_order
            .iter()
            .map(|id| &module.functions[*id])
            .filter(|function| function.name.starts_with("__riddle_lambda_"))
            .any(|function| function.blocks.iter().any(|(_, block)| {
                block.insts.iter().any(|inst| matches!(
                    &inst.kind,
                    mir::instr::InstKind::HeapAlloc(mir::types::Type::Ptr(ty))
                        if matches!(ty.as_ref(), mir::types::Type::Struct(ty) if ty.name().ends_with("_env"))
                ))
            })),
        "an inner lambda returned across the outer lambda frame must escape"
    );

    let drop = module
        .functions
        .values()
        .find(|function| {
            function.name.starts_with("__riddle_lambda_") && function.name.ends_with("_drop")
        })
        .expect("missing escaping closure drop function");
    assert!(
        drop.blocks.iter().any(|(_, block)| block
            .insts
            .iter()
            .any(|inst| matches!(inst.kind, mir::instr::InstKind::HeapFree(_)))),
        "escaping closure drop adapter must release its environment: {drop:#?}"
    );
}

#[test]
fn closure_field_capture_clears_only_that_fields_drop_slot() {
    let module = lower(
        r#"
        #[lang = "drop"]
        trait Drop { fun drop(&mut self); }
        struct First {}
        struct Second {}
        struct Pair { first: First, second: Second }
        impl Drop for First { fun drop(&mut self) {} }
        impl Drop for Second { fun drop(&mut self) {} }
        fun consume(value: First) {}
        fun main() {
            let pair = Pair { first: First {}, second: Second {} };
            let take_first = [ -> { consume(pair.first); }];
        }
        "#,
    );

    let lambda_drop = module
        .functions
        .values()
        .find(|function| {
            function.name.starts_with("__riddle_lambda_") && function.name.ends_with("_drop")
        })
        .expect("missing closure drop function");
    assert!(
        lambda_drop
            .blocks
            .iter()
            .any(|(_, block)| block.insts.iter().any(|inst| {
                matches!(
                    &inst.kind,
                    mir::instr::InstKind::Call(mir::value::FuncRef::Local(name), _)
                        if name == "drop__5:First"
                )
            }))
    );

    let main = module
        .functions
        .values()
        .find(|function| function.name == "main")
        .expect("missing main");
    assert!(
        main.blocks
            .iter()
            .any(|(_, block)| block.insts.iter().any(|inst| {
                matches!(
                    &inst.kind,
                    mir::instr::InstKind::Call(mir::value::FuncRef::Local(name), _)
                        if name == "drop__6:Second"
                )
            }))
    );
}

#[test]
fn loop_with_break_value_uses_slot_and_branches_to_exit() {
    let module = lower(
        r"
        fun f() -> i32 {
            let mut n = 0;
            let r = loop {
                n += 1;
                if n > 3 {
                    break n;
                }
            };
            r
        }
        ",
    );
    let func = module
        .functions
        .values()
        .find(|function| function.name == "f")
        .expect("missing f");
    let block_id = |label: &str| {
        func.blocks
            .iter()
            .find_map(|(id, block)| (block.label.as_deref() == Some(label)).then_some(id))
            .unwrap_or_else(|| panic!("missing block {label}: {func:#?}"))
    };
    let body = block_id("loop_body");
    let exit = block_id("loop_exit");

    let breaking = func
        .blocks
        .iter()
        .map(|(_, block)| block)
        .find(|block| {
            matches!(block.terminator, mir::instr::Terminator::Branch(target) if target == exit)
        })
        .expect("missing break block");
    let slot = breaking
        .insts
        .iter()
        .find_map(|inst| match inst.kind {
            mir::instr::InstKind::Store(_, ptr) => Some(ptr),
            _ => None,
        })
        .expect("break must store its value before branching to loop_exit");
    assert!(
        func.blocks[exit]
            .insts
            .iter()
            .any(|inst| matches!(inst.kind, mir::instr::InstKind::Load(ptr) if ptr == slot)),
        "loop_exit must load the value staged by break: {func:#?}"
    );
    // 循环体尾部必须跳回 loop_body，否则循环会意外落空
    let falls_back = func.blocks.iter().any(|(_, block)| {
        matches!(block.terminator, mir::instr::Terminator::Branch(target) if target == body)
    });
    assert!(
        falls_back,
        "loop body must branch back to itself: {func:#?}"
    );
}

#[test]
fn nested_loop_break_only_exits_inner_loop() {
    let module = lower(
        r"
        fun f() -> i32 {
            let mut n = 0;
            let r = loop {
                n += 1;
                let inner = loop {
                    break n > 3;
                };
                if inner {
                    break n;
                }
            };
            r
        }
        ",
    );
    let func = module
        .functions
        .values()
        .find(|function| function.name == "f")
        .expect("missing f");

    let exits: Vec<_> = func
        .blocks
        .iter()
        .filter(|(_, block)| block.label.as_deref() == Some("loop_exit"))
        .map(|(id, _)| id)
        .collect();
    assert_eq!(exits.len(), 2, "expected two loop exits: {func:#?}");

    let reaches = |start, want: &dyn Fn(&mir::block::Block) -> bool| {
        let mut seen = std::collections::HashSet::new();
        let mut stack = vec![start];
        while let Some(id) = stack.pop() {
            if !seen.insert(id) {
                continue;
            }
            let block = &func.blocks[id];
            if want(block) {
                return true;
            }
            match block.terminator {
                mir::instr::Terminator::Branch(target) => stack.push(target),
                mir::instr::Terminator::CondBranch(_, then, otherwise) => {
                    stack.push(then);
                    stack.push(otherwise);
                }
                _ => {}
            }
        }
        false
    };
    let is_loop_body = |block: &mir::block::Block| block.label.as_deref() == Some("loop_body");

    // 内层 break 出口加载 bool；外层出口加载 i32。
    let (inner_exit, outer_exit) = exits.iter().partition::<Vec<_>, _>(|id| {
        func.blocks[**id].insts.iter().any(|inst| {
            matches!(inst.kind, mir::instr::InstKind::Load(_)) && inst.ty == mir::types::Type::Bool
        })
    });
    let (inner_exit, outer_exit) = (
        *inner_exit.first().expect("missing inner loop exit"),
        *outer_exit.first().expect("missing outer loop exit"),
    );

    assert!(
        reaches(inner_exit, &is_loop_body),
        "inner break must fall back into the outer loop body: {func:#?}"
    );
    assert!(
        !reaches(outer_exit, &is_loop_body),
        "outer break must leave both loops: {func:#?}"
    );
}

#[test]
fn loop_break_value_stores_before_dropping_locals() {
    let module = lower(
        r#"
        #[lang = "drop"]
        trait Drop {
            fun drop(&mut self);
        }

        struct Guard {}

        impl Drop for Guard {
            fun drop(&mut self) {}
        }

        fun f() -> i32 {
            let r = loop {
                let guard = Guard {};
                break 42;
            };
            r
        }
        "#,
    );
    let func = module
        .functions
        .values()
        .find(|function| function.name == "f")
        .expect("missing f");
    let exit = func
        .blocks
        .iter()
        .find_map(|(id, block)| (block.label.as_deref() == Some("loop_exit")).then_some(id))
        .expect("missing loop_exit");
    let slot = func.blocks[exit]
        .insts
        .iter()
        .find_map(|inst| match inst.kind {
            mir::instr::InstKind::Load(ptr) => Some(ptr),
            _ => None,
        })
        .expect("loop_exit must load the break value");

    let reaches = |start, want: &dyn Fn(&mir::block::Block) -> bool| {
        let mut seen = std::collections::HashSet::new();
        let mut stack = vec![start];
        while let Some(id) = stack.pop() {
            if !seen.insert(id) {
                continue;
            }
            let block = &func.blocks[id];
            if want(block) {
                return true;
            }
            match block.terminator {
                mir::instr::Terminator::Branch(target) => stack.push(target),
                mir::instr::Terminator::CondBranch(_, then, otherwise) => {
                    stack.push(then);
                    stack.push(otherwise);
                }
                _ => {}
            }
        }
        false
    };
    let is_drop_block = |block: &mir::block::Block| {
        block.insts.iter().any(|inst| {
            matches!(
                &inst.kind,
                mir::instr::InstKind::Call(mir::value::FuncRef::Local(name), _)
                    if name.contains("drop")
            )
        })
    };

    // drop 发射会拆分块，因此沿控制流断言顺序：写入 break 槽的块必须先于 drop 块执行
    let store_block =
        func.blocks
            .iter()
            .map(|(id, _)| id)
            .find(|id| {
                func.blocks[*id].insts.iter().any(
                    |inst| matches!(inst.kind, mir::instr::InstKind::Store(_, ptr) if ptr == slot),
                )
            })
            .expect("break must store its value");
    assert!(
        reaches(store_block, &is_drop_block),
        "break value must be stored before locals are dropped: {func:#?}"
    );
    let drop_block = func
        .blocks
        .iter()
        .map(|(id, _)| id)
        .find(|id| is_drop_block(&func.blocks[*id]))
        .expect("break must drop loop locals");
    assert!(
        reaches(drop_block, &|block| block.label.as_deref()
            == Some("loop_exit")),
        "break path must reach loop_exit after dropping locals: {func:#?}"
    );
}

#[test]
fn if_let_desugars_to_a_match_with_a_wildcard_else_arm() {
    let (hir, types, _, _) = compile(
        r"
        enum Option { Some(i32), None }

        fun main() -> i32 {
            let opt = Option::Some(1);
            if let Option::Some(x) = opt { x } else { 0 }
        }
        ",
    );
    assert!(types.diagnostics.is_empty(), "{:#?}", types.diagnostics);

    let desugared = hir.bodies.iter().any(|(_, body)| {
        body.exprs.iter().any(|(_, expr)| {
            matches!(expr, hir::body::Expr::Match { arms, .. }
                if arms.len() == 2
                    && arms.iter().all(|arm| arm.guard.is_none())
                    && matches!(body.pats[arms[1].pat], hir::body::Pattern::Wildcard))
        })
    });
    assert!(desugared, "if-let should desugar to a two-arm match");
}

#[test]
fn while_let_desugars_to_a_loop_whose_body_matches_the_scrutinee() {
    let (hir, types, _, _) = compile(
        r"
        enum Option { Some(i32), None }

        fun next(state: i32) -> Option {
            if state > 0 { Option::Some(state - 1) } else { Option::None }
        }

        fun main() -> i32 {
            let mut state = 3;
            let mut total = 0;
            while let Option::Some(v) = next(state) {
                total += v;
                state = v;
            }
            total
        }
        ",
    );
    assert!(types.diagnostics.is_empty(), "{:#?}", types.diagnostics);

    let desugared = hir.bodies.iter().any(|(_, body)| {
        body.exprs.iter().any(|(_, expr)| {
            let hir::body::Expr::Loop { body: loop_body } = expr else {
                return false;
            };
            let hir::body::Expr::Block {
                tail: Some(tail), ..
            } = &body.exprs[*loop_body]
            else {
                return false;
            };
            let hir::body::Expr::Match { arms, .. } = &body.exprs[*tail] else {
                return false;
            };
            let [matched, missed] = arms.as_slice() else {
                return false;
            };
            !matches!(body.pats[matched.pat], hir::body::Pattern::Wildcard)
                && matches!(body.pats[missed.pat], hir::body::Pattern::Wildcard)
                && matches!(
                    &body.exprs[missed.body],
                    hir::body::Expr::Block { stmts, tail: None }
                        if stmts.iter().any(|stmt| matches!(
                            body.stmts[*stmt],
                            hir::body::Stmt::Break { value: None }
                        ))
                )
        })
    });
    assert!(
        desugared,
        "while-let should desugar to a loop whose body matches, breaking on the wildcard arm"
    );
}

#[test]
fn while_let_lowers_to_a_back_edge_and_a_pattern_branch() {
    let module = lower(
        r"
        enum Option { Some(i32), None }

        fun next(state: i32) -> Option {
            if state > 0 { Option::Some(state - 1) } else { Option::None }
        }

        fun main() -> i32 {
            let mut state = 3;
            let mut total = 0;
            while let Option::Some(v) = next(state) {
                total += v;
                state = v;
            }
            total
        }
        ",
    );

    let main = module
        .functions
        .values()
        .find(|function| function.name == "main")
        .expect("missing main");
    let has_back_edge = main.blocks.iter().any(|(id, block)| {
        matches!(block.terminator, mir::instr::Terminator::Branch(target) if target <= id)
    });
    assert!(has_back_edge, "while-let should loop: {main:#?}");
    assert!(
        main.blocks
            .iter()
            .any(|(_, block)| matches!(block.terminator, mir::instr::Terminator::CondBranch(..))),
        "while-let should branch on the pattern test: {main:#?}"
    );
}

#[test]
fn for_loop_tuple_pattern_binds_elements_over_arrays() {
    let module = lower(
        r"
        fun main() {
            let mut sum = 0;
            let pairs = [(1, 2), (3, 4)];
            for (a, b) in pairs {
                sum += a + b;
            }
        }
        ",
    );
    let main = module
        .functions
        .values()
        .find(|function| function.name == "main")
        .expect("missing main");
    let extracts = main
        .blocks
        .iter()
        .flat_map(|(_, block)| block.insts.iter())
        .filter(|inst| matches!(inst.kind, mir::instr::InstKind::ExtractValue(..)))
        .count();
    assert!(
        extracts >= 2,
        "both tuple elements should be extracted into bindings: {main:#?}"
    );
    assert!(
        main.blocks
            .iter()
            .any(|(_, block)| matches!(block.terminator, mir::instr::Terminator::CondBranch(..))),
        "{main:#?}"
    );
}

#[test]
fn for_loop_tuple_pattern_binds_elements_over_iterators() {
    let module = lower(
        r"
        enum Option<T> {
            Some(T),
            None,
        }

        trait Iterator {
            type Item;
            fun next(&mut self) -> Option<Self::Item>;
        }

        trait IntoIterator {
            type Item;
            type IntoIter;
            fun into_iter(self) -> Self::IntoIter;
        }

        struct Counter {
            current: i32,
        }

        impl Iterator for Counter {
            type Item = (i32, i32);

            fun next(&mut self) -> Option<Self::Item> {
                if self.current < 3 {
                    let value = self.current;
                    self.current += 1;
                    Option::Some((value, value))
                } else {
                    Option::None
                }
            }
        }

        impl IntoIterator for Counter {
            type Item = (i32, i32);
            type IntoIter = Counter;

            fun into_iter(self) -> Self::IntoIter {
                self
            }
        }

        fun main() {
            let mut sum = 0;
            let counter = Counter { current: 0 };
            for (a, b) in counter {
                sum += a + b;
            }
        }
        ",
    );
    let main = module
        .functions
        .values()
        .find(|function| function.name == "main")
        .expect("missing main");
    let extracts = main
        .blocks
        .iter()
        .flat_map(|(_, block)| block.insts.iter())
        .filter(|inst| matches!(inst.kind, mir::instr::InstKind::ExtractValue(..)))
        .count();
    assert!(
        extracts >= 2,
        "both tuple elements should be extracted into bindings: {main:#?}"
    );
}

#[test]
fn for_loop_over_range_shaped_struct_still_binds_the_pattern() {
    // Without an `IntoIterator` trait in scope the range fast path lowers the
    // loop directly from the struct; the header binding must survive.
    let module = lower(
        r"
        struct Range { start: i32, end: i32 }

        fun main() {
            let mut sum = 0;
            let range = Range { start: 0, end: 3 };
            for i in range {
                sum += i;
            }
        }
        ",
    );
    let main = module
        .functions
        .values()
        .find(|function| function.name == "main")
        .expect("missing main");
    let has_back_edge = main.blocks.iter().any(|(id, block)| {
        matches!(block.terminator, mir::instr::Terminator::Branch(target) if target <= id)
    });
    assert!(has_back_edge, "range loop should loop: {main:#?}");
    assert!(
        main.blocks
            .iter()
            .any(|(_, block)| matches!(block.terminator, mir::instr::Terminator::CondBranch(..))),
        "range loop should branch on the cursor: {main:#?}"
    );
}

#[test]
fn let_else_lowers_to_a_conditional_binding() {
    let module = lower(
        r"
        enum Option { Some(i32), None }

        fun unwrap_or(opt: Option, fallback: i32) -> i32 {
            let Option::Some(x) = opt else {
                return fallback;
            };
            x
        }
        ",
    );
    let func = module
        .functions
        .values()
        .find(|function| function.name == "unwrap_or")
        .expect("missing unwrap_or");
    let block_id = |label: &str| {
        func.blocks
            .iter()
            .find_map(|(id, block)| (block.label.as_deref() == Some(label)).then_some(id))
            .unwrap_or_else(|| panic!("missing block {label}: {func:#?}"))
    };
    let matched = block_id("let_else_match");
    let diverge = block_id("let_else_diverge");
    let merge = block_id("let_else_merge");

    assert!(
        func.blocks.iter().any(|(_, block)| matches!(
            block.terminator,
            mir::instr::Terminator::CondBranch(_, then, otherwise)
                if then == matched && otherwise == diverge
        )),
        "let-else must branch on the pattern test: {func:#?}"
    );
    assert!(
        matches!(
            func.blocks[matched].terminator,
            mir::instr::Terminator::Branch(target) if target == merge
        ),
        "the matched block must bind and fall through to the merge block: {func:#?}"
    );

    let reaches = |start, want: &dyn Fn(&mir::block::Block) -> bool| {
        let mut seen = std::collections::HashSet::new();
        let mut stack = vec![start];
        while let Some(id) = stack.pop() {
            if !seen.insert(id) {
                continue;
            }
            let block = &func.blocks[id];
            if want(block) {
                return true;
            }
            match block.terminator {
                mir::instr::Terminator::Branch(target) => stack.push(target),
                mir::instr::Terminator::CondBranch(_, then, otherwise) => {
                    stack.push(then);
                    stack.push(otherwise);
                }
                _ => {}
            }
        }
        false
    };
    assert!(
        reaches(diverge, &|block| {
            matches!(block.terminator, mir::instr::Terminator::Return(_))
        }),
        "the diverging else block must end in a return: {func:#?}"
    );
    assert!(
        !reaches(diverge, &|block| block.label.as_deref()
            == Some("let_else_merge")),
        "the else path never falls through past the statement: {func:#?}"
    );
}

#[test]
fn let_else_activates_drop_flags_only_on_the_matched_path() {
    let module = lower(
        r#"
        #[lang = "drop"]
        trait Drop {
            fun drop(&mut self);
        }

        struct Guard { id: i32 }

        impl Drop for Guard {
            fun drop(&mut self) {}
        }

        enum Maybe { Some(Guard), None }

        fun unwrap_or(maybe: Maybe, fallback: i32) -> i32 {
            let Maybe::Some(g) = maybe else {
                return fallback;
            };
            g.id
        }
        "#,
    );
    let func = module
        .functions
        .values()
        .find(|function| function.name == "unwrap_or")
        .expect("missing unwrap_or");
    let block_id = |label: &str| {
        func.blocks
            .iter()
            .find_map(|(id, block)| (block.label.as_deref() == Some(label)).then_some(id))
            .unwrap_or_else(|| panic!("missing block {label}: {func:#?}"))
    };
    let matched = block_id("let_else_match");
    let diverge = block_id("let_else_diverge");
    let defines = |value: mir::value::Value| {
        func.blocks
            .iter()
            .flat_map(|(_, block)| {
                block
                    .insts
                    .iter()
                    .enumerate()
                    .map(move |(index, inst)| (block.value_at(index), inst))
            })
            .find(|(candidate, _)| *candidate == value)
            .map(|(_, inst)| inst)
    };
    let stores_bool = |block: &mir::block::Block, expect: bool| {
        block
            .insts
            .iter()
            .filter_map(move |inst| match inst.kind {
                mir::instr::InstKind::Store(value, ptr) => match defines(value) {
                    Some(defining)
                        if matches!(
                            defining.kind,
                            mir::instr::InstKind::Const(mir::instr::ConstValue::Bool(b)) if b == expect
                        ) =>
                    {
                        Some(ptr)
                    }
                    _ => None,
                },
                _ => None,
            })
            .collect::<Vec<_>>()
    };

    let flag = stores_bool(&func.blocks[matched], true)
        .into_iter()
        .next()
        .expect("the matched block must activate the drop flag");
    let test_block = func
        .blocks
        .iter()
        .map(|(id, _)| id)
        .find(|id| {
            matches!(
                func.blocks[*id].terminator,
                mir::instr::Terminator::CondBranch(_, then, otherwise)
                    if then == matched && otherwise == diverge
            )
        })
        .expect("missing the pattern test block");
    assert!(
        stores_bool(&func.blocks[test_block], false)
            .into_iter()
            .any(|ptr| ptr == flag),
        "the drop flag must start inactive before the branch: {func:#?}"
    );

    let mut seen = std::collections::HashSet::new();
    let mut stack = vec![diverge];
    while let Some(id) = stack.pop() {
        if !seen.insert(id) {
            continue;
        }
        let block = &func.blocks[id];
        assert!(
            !stores_bool(block, true).into_iter().any(|ptr| ptr == flag),
            "the else path must not activate the drop flag: {func:#?}"
        );
        match block.terminator {
            mir::instr::Terminator::Branch(target) => stack.push(target),
            mir::instr::Terminator::CondBranch(_, then, otherwise) => {
                stack.push(then);
                stack.push(otherwise);
            }
            _ => {}
        }
    }
}

#[test]
fn let_else_with_an_irrefutable_pattern_binds_unconditionally() {
    let module = lower(
        r"
        fun main() -> i32 {
            let (a, b) = (1, 2) else {
                return 0;
            };
            a + b
        }
        ",
    );
    let main = module
        .functions
        .values()
        .find(|function| function.name == "main")
        .expect("missing main");
    assert!(
        !main
            .blocks
            .iter()
            .any(|(_, block)| block.label.as_deref() == Some("let_else_match")),
        "an irrefutable pattern needs no conditional binding: {main:#?}"
    );
    assert!(
        main.blocks
            .iter()
            .flat_map(|(_, block)| block.insts.iter())
            .filter(|inst| matches!(inst.kind, mir::instr::InstKind::ExtractValue(..)))
            .count()
            >= 2,
        "both tuple elements should be extracted into bindings: {main:#?}"
    );
}

#[test]
fn known_lambda_callback_does_not_retain_unreferenced_arguments() {
    // Calling a lambda literal through a binding must not blanket-promote
    // every reference argument: the lambda `[p -> &p.value]` references only
    // its parameter, so the unrelated `other` local stays on the stack.
    let module = lower(
        r"
        struct Data { value: i32 }

        fun pick(data: &Data) -> &i32 {
            let selector = [p -> &p.value];
            selector(data)
        }

        fun unrelated_heap_use_is_not_forced() -> i32 {
            let data = Data { value: 1 };
            let other = Data { value: 2 };
            let selector = [p -> p.value];
            let picked = selector(&data);
            picked + other.value
        }
        ",
    );
    for function in module.functions.values() {
        if function.name.contains("unrelated") {
            assert!(
                !function
                    .blocks
                    .values()
                    .flat_map(|block| &block.insts)
                    .any(|inst| matches!(inst.kind, mir::instr::InstKind::HeapAlloc(_))),
                "a callback that never references its argument must not force heap promotion: {function:#?}"
            );
        }
    }
}

#[test]
fn lambda_returning_argument_reference_promotes_it() {
    // `[p -> &p.value]` does reference its parameter, so the argument's
    // storage must still be heap-promoted.
    let module = lower(
        r"
        struct Data { value: i32 }

        fun pick() -> &i32 {
            let local = Data { value: 1 };
            let selector = [p -> &p.value];
            selector(&local)
        }
        ",
    );
    let func = module
        .functions
        .values()
        .find(|function| function.name == "pick")
        .expect("pick function should exist");
    assert!(
        func.blocks
            .values()
            .flat_map(|block| &block.insts)
            .any(|inst| matches!(inst.kind, mir::instr::InstKind::HeapAlloc(_))),
        "a callback that returns a reference to its argument must promote it: {func:#?}"
    );
}

#[test]
fn recursive_struct_keeps_a_finite_type_graph() {
    // `next: *const Node` reaches the struct that is being defined. Lowering has
    // to share one definition and refer back to it, because expanding the field
    // types eagerly would never end.
    let module = lower(
        r"
        struct Node {
            data: i32,
            next: *const Node,
        }

        fun touch(node: &Node) -> i32 {
            node.data
        }
        ",
    );

    let node = module
        .struct_defs
        .iter()
        .find(|def| def.name == "Node")
        .expect("the recursive definition is registered on the module");
    assert_eq!(node.fields.len(), 2);
    let mir::types::Type::Ptr(inner) = &node.fields[1].1 else {
        panic!(
            "`next` should lower to a pointer, got {:?}",
            node.fields[1].1
        );
    };
    let mir::types::Type::Struct(handle) = inner.as_ref() else {
        panic!("`next` should point at the struct itself");
    };
    assert!(
        handle.is_recursive_occurrence(),
        "the field reaching the struct again must be the shared back edge"
    );
    assert_eq!(
        handle.def().symbol,
        node.symbol,
        "resolving the back edge must land on the same definition, not a copy"
    );
}
