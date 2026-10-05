use mir::builder::Builder;
use mir::func::Function;
use mir::instr::*;
use mir::types::*;
use mir::value::{FuncRef, Value};

#[test]
fn build_const_int() {
    let mut func = Function::new("test".into(), Type::Int(IntTy::I32));
    let mut builder = Builder::new(&mut func);
    let v = builder.iconst(42, IntTy::I32);
    assert_eq!(v.0, 0); // 第一个值的编号
    let entry = &func.blocks[func.entry];
    assert_eq!(entry.insts.len(), 1);
    assert!(matches!(
        entry.insts[0].kind,
        InstKind::Const(ConstValue::Int(42, IntWidth::I32))
    ));
}

#[test]
fn build_add() {
    let mut func = Function::new("test".into(), Type::Int(IntTy::I32));
    let mut builder = Builder::new(&mut func);
    let a = builder.iconst(10, IntTy::I32);
    let b = builder.iconst(20, IntTy::I32);
    let c = builder.binop(BinOp::Add, a, b, Type::Int(IntTy::I32));
    assert_eq!(c.0, 2);
    assert_eq!(func.blocks[func.entry].insts.len(), 3);
}

#[test]
fn build_cmp() {
    let mut func = Function::new("test".into(), Type::Bool);
    let mut builder = Builder::new(&mut func);
    let a = builder.iconst(10, IntTy::I32);
    let b = builder.iconst(20, IntTy::I32);
    let c = builder.cmp(CmpOp::Lt, a, b);
    assert_eq!(c.0, 2);
    assert_eq!(func.blocks[func.entry].insts[2].ty, Type::Bool);
}

#[test]
fn build_alloca_and_heap_alloc() {
    let mut func = Function::new("test".into(), Type::Unit);
    let mut builder = Builder::new(&mut func);

    let stack = builder.alloca(Type::Int(IntTy::I32));
    let heap = builder.heap_alloc(Type::Int(IntTy::I32));

    let entry = &func.blocks[func.entry];
    assert!(matches!(entry.insts[0].kind, InstKind::Alloca(_)));
    assert!(matches!(entry.insts[1].kind, InstKind::HeapAlloc(_)));
    assert_ne!(stack.0, heap.0);
}

#[test]
fn build_multiple_blocks() {
    let mut func = Function::new("test".into(), Type::Unit);

    // 先创建块，再创建 builder
    let then_block = func.new_block_labeled("then");
    let else_block = func.new_block_labeled("else");
    let merge_block = func.new_block_labeled("merge");

    let mut builder = Builder::new(&mut func);
    builder.set_cond_branch(Value(0), then_block, else_block);

    builder.switch_to_block(then_block);
    builder.set_branch(merge_block);

    builder.switch_to_block(else_block);
    builder.set_branch(merge_block);

    builder.switch_to_block(merge_block);
    builder.set_return(None);

    assert_eq!(func.blocks.iter().count(), 4);
}

#[test]
fn string_const_is_shared_str_ref() {
    let mut func = Function::new("test".into(), Type::Unit);
    let mut builder = Builder::new(&mut func);
    builder.sconst("\"hello\"".into());

    assert_eq!(
        func.blocks[func.entry].insts[0].ty,
        Type::Ref(Box::new(Type::Str), false)
    );
}

#[test]
fn values_are_unique_across_precreated_blocks() {
    let mut func = Function::new("test".into(), Type::Unit);
    let left = func.new_block_labeled("left");
    let right = func.new_block_labeled("right");

    let mut builder = Builder::new(&mut func);
    builder.switch_to_block(left);
    let a = builder.bconst(true);
    builder.switch_to_block(right);
    let b = builder.bconst(false);

    assert_ne!(a, b);
}

#[test]
fn build_call() {
    let mut func = Function::new("test".into(), Type::Int(IntTy::I32));
    let mut builder = Builder::new(&mut func);
    let a = builder.iconst(5, IntTy::I32);
    let result = builder.call(
        FuncRef::Local("square".into()),
        vec![a],
        Type::Int(IntTy::I32),
    );
    assert_eq!(result.0, 1);
    assert!(matches!(
        &func.blocks[func.entry].insts[1].kind,
        InstKind::Call(FuncRef::Local(name), _) if name == "square"
    ));
}

#[test]
fn build_return_value() {
    let mut func = Function::new("test".into(), Type::Int(IntTy::I32));
    let mut builder = Builder::new(&mut func);
    let v = builder.iconst(42, IntTy::I32);
    builder.set_return(Some(v));
    assert!(matches!(
        func.blocks[func.entry].terminator,
        Terminator::Return(Some(val)) if val.0 == v.0
    ));
}

#[test]
fn build_return_void() {
    let mut func = Function::new("test".into(), Type::Unit);
    let mut builder = Builder::new(&mut func);
    builder.set_return(None);
    assert!(matches!(
        func.blocks[func.entry].terminator,
        Terminator::Return(None)
    ));
}

#[test]
fn value_numbering_consecutive() {
    let mut func = Function::new("test".into(), Type::Int(IntTy::I32));
    let mut builder = Builder::new(&mut func);
    let a = builder.iconst(1, IntTy::I32);
    let b = builder.iconst(2, IntTy::I32);
    let c = builder.iconst(3, IntTy::I32);
    assert_eq!(a.0, 0);
    assert_eq!(b.0, 1);
    assert_eq!(c.0, 2);
}

#[test]
fn function_params_have_values() {
    let mut func = Function::new("test".into(), Type::Unit);
    let a = func.add_param("x".into(), Type::Int(IntTy::I32));
    let b = func.add_param("y".into(), Type::Bool);
    assert_eq!(a.0, 0);
    assert_eq!(b.0, 1);
    assert_eq!(func.params.len(), 2);
}
