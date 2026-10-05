use std::{
    collections::{HashMap, HashSet},
    mem::{Discriminant, discriminant},
};

use hir::{
    body::{LiteralPattern, MatchArm, PatId, Pattern},
    item_tree::{EnumId, HirPath, HirVariantKind, StructId},
};

use crate::{
    checker::TypeChecker,
    context::BodyCtx,
    types::{ConstArg, FloatTy, IntTy, Type},
};

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
enum Constructor {
    Bool(bool),
    EnumVariant(EnumId, usize),
    Tuple,
    Array,
    Struct(StructId),
    Unit,
    Int(u64),
    Float(u64),
    String(String),
    Char(String),
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
enum MatrixPat {
    Wildcard,
    Constructor(Constructor, Vec<Self>),
    IntegerRanges {
        ty: IntTy,
        ranges: Vec<IntegerRange>,
    },
    Invalid,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
struct IntegerRange {
    start: u128,
    end: u128,
}

const MAX_DISPLAYED_INTEGER_RANGES: usize = 8;

#[derive(Clone)]
struct ConstructorInfo {
    constructor: Constructor,
    fields: Vec<Type>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
enum TypeHead {
    Tuple(usize),
    Struct(StructId),
    Enum(EnumId),
    Other(Discriminant<Type>),
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
struct UsefulnessState {
    matrix: Vec<Vec<MatrixPat>>,
    vector: Vec<MatrixPat>,
    type_heads: Vec<TypeHead>,
    type_size: usize,
}

impl TypeChecker<'_> {
    /// Whether a single pattern fails to cover `scrutinee_ty`, i.e. whether it
    /// is refutable. Irrefutability is exactly one-arm exhaustiveness.
    pub(crate) fn missing_let_pattern(
        &mut self,
        ctx: &BodyCtx<'_>,
        pat: PatId,
        scrutinee_ty: &Type,
    ) -> Option<(String, Vec<String>)> {
        if scrutinee_ty.is_unknown_like() {
            return None;
        }

        let coverage_ty = reference_pattern_coverage_type(scrutinee_ty);
        let matrix = vec![vec![self.lower_matrix_pattern(ctx, pat, &coverage_ty)]];
        let witness = self.useful(
            &matrix,
            &[MatrixPat::Wildcard],
            std::slice::from_ref(&coverage_ty),
        )?;
        let witness = witness.first()?;
        let pattern = self.display_matrix_pattern(witness);
        let notes = integer_range_notes(witness, &pattern, self.hir.pointer_width_bits);
        Some((pattern, notes))
    }

    pub(crate) fn missing_match_pattern(
        &mut self,
        ctx: &BodyCtx<'_>,
        arms: &[MatchArm],
        scrutinee_ty: &Type,
    ) -> Option<(String, Vec<String>)> {
        if scrutinee_ty.is_unknown_like() {
            return None;
        }

        let coverage_ty = reference_pattern_coverage_type(scrutinee_ty);
        let matrix = arms
            .iter()
            .filter(|arm| arm.guard.is_none())
            .flat_map(|arm| self.lower_matrix_rows(ctx, arm.pat, &coverage_ty))
            .map(|row| vec![row])
            .collect::<Vec<_>>();
        let witness = self.useful(
            &matrix,
            &[MatrixPat::Wildcard],
            std::slice::from_ref(&coverage_ty),
        )?;
        let witness = witness.first()?;
        let pattern = self.display_matrix_pattern(witness);
        let notes = integer_range_notes(witness, &pattern, self.hir.pointer_width_bits);
        Some((pattern, notes))
    }

    /// Maranget's U(P, q): return a value matched by q and by no row in P.
    fn useful(
        &mut self,
        matrix: &[Vec<MatrixPat>],
        vector: &[MatrixPat],
        types: &[Type],
    ) -> Option<Vec<MatrixPat>> {
        self.useful_inner(matrix, vector, types, &mut HashSet::new())
    }

    fn useful_inner(
        &mut self,
        matrix: &[Vec<MatrixPat>],
        vector: &[MatrixPat],
        types: &[Type],
        active: &mut HashSet<UsefulnessState>,
    ) -> Option<Vec<MatrixPat>> {
        let state = UsefulnessState {
            matrix: matrix.to_vec(),
            vector: vector.to_vec(),
            type_heads: types.iter().map(type_head).collect(),
            type_size: types.iter().map(type_size).sum(),
        };
        // Finite nesting shrinks; recursive generic expansion stays flat or grows.
        if active.iter().any(|previous| {
            previous.matrix == state.matrix
                && previous.vector == state.vector
                && previous.type_heads == state.type_heads
                && previous.type_size <= state.type_size
        }) {
            return None;
        }
        active.insert(state.clone());
        let result = self.useful_step(matrix, vector, types, active);
        active.remove(&state);
        result
    }

    fn useful_step(
        &mut self,
        matrix: &[Vec<MatrixPat>],
        vector: &[MatrixPat],
        types: &[Type],
        active: &mut HashSet<UsefulnessState>,
    ) -> Option<Vec<MatrixPat>> {
        if vector.is_empty() {
            return matrix.is_empty().then(Vec::new);
        }

        match &vector[0] {
            MatrixPat::Invalid | MatrixPat::IntegerRanges { .. } => None,
            MatrixPat::Constructor(constructor, fields) => {
                let field_types = self.constructor_fields(&types[0], constructor)?;
                if field_types.len() != fields.len() {
                    return None;
                }
                let specialized = specialize(matrix, constructor, fields.len());
                let mut specialized_vector = fields.clone();
                specialized_vector.extend_from_slice(&vector[1..]);
                let mut specialized_types = field_types;
                specialized_types.extend_from_slice(&types[1..]);
                let witness = self.useful_inner(
                    &specialized,
                    &specialized_vector,
                    &specialized_types,
                    active,
                )?;
                Some(rebuild_witness(constructor, fields.len(), witness))
            }
            MatrixPat::Wildcard if integer_type(&types[0]).is_some() => {
                let ty = integer_type(&types[0]).unwrap();
                let ranges = uncovered_integer_ranges(matrix, ty, self.hir.pointer_width_bits);
                if !ranges.is_empty() {
                    let witness = self.useful_inner(
                        &default_matrix(matrix),
                        &vector[1..],
                        &types[1..],
                        active,
                    )?;
                    let mut result = vec![MatrixPat::IntegerRanges { ty, ranges }];
                    result.extend(witness);
                    return Some(result);
                }

                if vector.len() == 1 {
                    return None;
                }

                // A finite integer domain may be fully listed with singleton patterns.
                // ponytail: multi-column full domains scan per literal; group rows if this
                // becomes measurable for exhaustive integer product matches.
                for (_, value) in integer_literals(matrix, ty, self.hir.pointer_width_bits) {
                    let constructor = Constructor::Int(value);
                    let specialized = specialize(matrix, &constructor, 0);
                    if let Some(witness) =
                        self.useful_inner(&specialized, &vector[1..], &types[1..], active)
                    {
                        return Some(rebuild_witness(&constructor, 0, witness));
                    }
                }
                None
            }
            MatrixPat::Wildcard => {
                if let Some(constructors) = self.constructors(&types[0]) {
                    for info in constructors {
                        let arity = info.fields.len();
                        let specialized = specialize(matrix, &info.constructor, arity);
                        let mut specialized_vector = vec![MatrixPat::Wildcard; arity];
                        specialized_vector.extend_from_slice(&vector[1..]);
                        let mut specialized_types = info.fields;
                        specialized_types.extend_from_slice(&types[1..]);
                        if let Some(witness) = self.useful_inner(
                            &specialized,
                            &specialized_vector,
                            &specialized_types,
                            active,
                        ) {
                            return Some(rebuild_witness(&info.constructor, arity, witness));
                        }
                    }
                    None
                } else {
                    let witness = self.useful_inner(
                        &default_matrix(matrix),
                        &vector[1..],
                        &types[1..],
                        active,
                    )?;
                    let mut result = vec![MatrixPat::Wildcard];
                    result.extend(witness);
                    Some(result)
                }
            }
        }
    }

    /// Expands an arm pattern into the coverage matrix rows it contributes.
    /// A top-level or-pattern `A | B` is a disjunction, so it contributes one
    /// row per alternative; every other shape is a single row.
    fn lower_matrix_rows(
        &mut self,
        ctx: &BodyCtx<'_>,
        pat: PatId,
        expected: &Type,
    ) -> Vec<MatrixPat> {
        if let Pattern::Or { alternatives } = ctx.body.pats[pat].clone() {
            alternatives
                .into_iter()
                .map(|alternative| self.lower_matrix_pattern(ctx, alternative, expected))
                .collect()
        } else {
            vec![self.lower_matrix_pattern(ctx, pat, expected)]
        }
    }

    fn lower_matrix_pattern(
        &mut self,
        ctx: &BodyCtx<'_>,
        pat: PatId,
        expected: &Type,
    ) -> MatrixPat {
        let effective = self
            .result
            .pattern_types
            .get(&(ctx.body_id, pat))
            .cloned()
            .unwrap_or_else(|| expected.clone());
        let expected = &effective;
        match ctx.body.pats[pat].clone() {
            Pattern::Wildcard => MatrixPat::Wildcard,
            Pattern::Reference { pattern, .. } => self.lower_matrix_pattern(ctx, pattern, expected),
            Pattern::Literal(literal) => {
                literal_constructor(literal, expected, self.hir.pointer_width_bits)
                    .map_or(MatrixPat::Invalid, |constructor| {
                        MatrixPat::Constructor(constructor, Vec::new())
                    })
            }
            Pattern::Binding { name, .. } => {
                let Type::Enum(enum_id, _) = expected else {
                    return MatrixPat::Wildcard;
                };
                let enum_data = &self.hir.item_tree.enums[*enum_id];
                match enum_data
                    .variants
                    .iter()
                    .enumerate()
                    .find(|(_, variant)| variant.name.0 == name.0)
                {
                    Some((index, variant)) if matches!(variant.kind, HirVariantKind::Unit) => {
                        MatrixPat::Constructor(
                            Constructor::EnumVariant(*enum_id, index),
                            Vec::new(),
                        )
                    }
                    Some(_) => MatrixPat::Invalid,
                    None => MatrixPat::Wildcard,
                }
            }
            Pattern::Path { path } => self.lower_unit_variant_pattern(expected, &path),
            Pattern::Tuple { elements } => {
                if elements.is_empty() && expected == &Type::Unit {
                    return MatrixPat::Constructor(Constructor::Unit, Vec::new());
                }
                let Type::Tuple(expected_elements) = expected else {
                    return MatrixPat::Invalid;
                };
                if elements.len() != expected_elements.len() {
                    return MatrixPat::Invalid;
                }
                MatrixPat::Constructor(
                    Constructor::Tuple,
                    elements
                        .into_iter()
                        .zip(expected_elements)
                        .map(|(element, ty)| self.lower_matrix_pattern(ctx, element, ty))
                        .collect(),
                )
            }
            Pattern::TupleStruct { path, elements } => {
                self.lower_tuple_variant_pattern(ctx, expected, &path, &elements)
            }
            Pattern::Struct { path, fields } => {
                self.lower_struct_pattern(ctx, expected, &path, &fields)
            }
            // The parser only accepts `A | B` at the top of an arm, and
            // `lower_matrix_rows` expands those into one row per alternative
            // before this runs. A nested or-pattern reaching here would be a
            // bug, so refuse to reason about it rather than under-approximate
            // coverage with just the first alternative.
            Pattern::Or { .. } => MatrixPat::Invalid,
        }
    }

    fn lower_unit_variant_pattern(&self, expected: &Type, path: &HirPath) -> MatrixPat {
        let Type::Enum(enum_id, _) = expected else {
            return MatrixPat::Invalid;
        };
        let Some(index) = self.enum_variant_index(*enum_id, path) else {
            return MatrixPat::Invalid;
        };
        matches!(
            self.hir.item_tree.enums[*enum_id].variants[index].kind,
            HirVariantKind::Unit
        )
        .then(|| MatrixPat::Constructor(Constructor::EnumVariant(*enum_id, index), Vec::new()))
        .unwrap_or(MatrixPat::Invalid)
    }

    fn lower_tuple_variant_pattern(
        &mut self,
        ctx: &BodyCtx<'_>,
        expected: &Type,
        path: &HirPath,
        elements: &[PatId],
    ) -> MatrixPat {
        let Type::Enum(enum_id, _) = expected else {
            return MatrixPat::Invalid;
        };
        let Some(index) = self.enum_variant_index(*enum_id, path) else {
            return MatrixPat::Invalid;
        };
        let constructor = Constructor::EnumVariant(*enum_id, index);
        let Some(field_types) = self.constructor_fields(expected, &constructor) else {
            return MatrixPat::Invalid;
        };
        if !matches!(
            self.hir.item_tree.enums[*enum_id].variants[index].kind,
            HirVariantKind::Tuple(_)
        ) || elements.len() != field_types.len()
        {
            return MatrixPat::Invalid;
        }
        MatrixPat::Constructor(
            constructor,
            elements
                .iter()
                .zip(&field_types)
                .map(|(element, ty)| self.lower_matrix_pattern(ctx, *element, ty))
                .collect(),
        )
    }

    fn lower_struct_pattern(
        &mut self,
        ctx: &BodyCtx<'_>,
        expected: &Type,
        path: &HirPath,
        fields: &[hir::body::FieldPat],
    ) -> MatrixPat {
        match expected {
            Type::Struct(struct_id, _) => {
                let strukt = self.hir.item_tree.structs[*struct_id].clone();
                if path
                    .segments
                    .last()
                    .is_none_or(|name| name.0 != strukt.name.0)
                {
                    return MatrixPat::Invalid;
                }
                let constructor = Constructor::Struct(*struct_id);
                let Some(field_types) = self.constructor_fields(expected, &constructor) else {
                    return MatrixPat::Invalid;
                };
                let declared = strukt
                    .fields
                    .iter()
                    .map(|field| field.name.0.as_str())
                    .collect::<HashSet<_>>();
                let mut seen = HashSet::new();
                if fields.iter().any(|field| {
                    !declared.contains(field.name.0.as_str()) || !seen.insert(field.name.0.as_str())
                }) {
                    return MatrixPat::Invalid;
                }
                let patterns = strukt
                    .fields
                    .iter()
                    .zip(field_types)
                    .map(|(field, ty)| {
                        fields
                            .iter()
                            .find(|pattern| pattern.name.0 == field.name.0)
                            .and_then(|pattern| pattern.pat)
                            .map_or(MatrixPat::Wildcard, |pat| {
                                self.lower_matrix_pattern(ctx, pat, &ty)
                            })
                    })
                    .collect();
                MatrixPat::Constructor(constructor, patterns)
            }
            Type::Enum(enum_id, _) => {
                let Some(index) = self.enum_variant_index(*enum_id, path) else {
                    return MatrixPat::Invalid;
                };
                let enum_data = self.hir.item_tree.enums[*enum_id].clone();
                let HirVariantKind::Struct(items) = &enum_data.variants[index].kind else {
                    return MatrixPat::Invalid;
                };
                let constructor = Constructor::EnumVariant(*enum_id, index);
                let Some(field_types) = self.constructor_fields(expected, &constructor) else {
                    return MatrixPat::Invalid;
                };
                let declared = items
                    .iter()
                    .map(|field| field.name.0.as_str())
                    .collect::<HashSet<_>>();
                let mut seen = HashSet::new();
                if fields.iter().any(|field| {
                    !declared.contains(field.name.0.as_str()) || !seen.insert(field.name.0.as_str())
                }) {
                    return MatrixPat::Invalid;
                }
                let patterns = items
                    .iter()
                    .zip(field_types)
                    .map(|(field, ty)| {
                        fields
                            .iter()
                            .find(|pattern| pattern.name.0 == field.name.0)
                            .and_then(|pattern| pattern.pat)
                            .map_or(MatrixPat::Wildcard, |pat| {
                                self.lower_matrix_pattern(ctx, pat, &ty)
                            })
                    })
                    .collect();
                MatrixPat::Constructor(constructor, patterns)
            }
            _ => MatrixPat::Invalid,
        }
    }

    pub(crate) fn enum_variant_index(&self, enum_id: EnumId, path: &HirPath) -> Option<usize> {
        let name = path.segments.last()?;
        if let Some(owner) = path.segments.iter().rev().nth(1)
            && self.find_enum_by_name(&owner.0) != Some(enum_id)
        {
            return None;
        }
        self.hir.item_tree.enums[enum_id]
            .variants
            .iter()
            .position(|variant| variant.name.0 == name.0)
    }

    fn constructors(&mut self, ty: &Type) -> Option<Vec<ConstructorInfo>> {
        if self.type_has_infinite_layout(ty) {
            return Some(Vec::new());
        }
        match ty {
            Type::Bool => Some(vec![
                ConstructorInfo {
                    constructor: Constructor::Bool(false),
                    fields: Vec::new(),
                },
                ConstructorInfo {
                    constructor: Constructor::Bool(true),
                    fields: Vec::new(),
                },
            ]),
            Type::Enum(enum_id, args) => Some(self.enum_constructors(*enum_id, args)),
            Type::Tuple(fields) => Some(vec![ConstructorInfo {
                constructor: Constructor::Tuple,
                fields: fields.clone(),
            }]),
            Type::Array(element, ConstArg::Value(len)) => Some(vec![ConstructorInfo {
                constructor: Constructor::Array,
                fields: (*len != 0).then(|| *element.clone()).into_iter().collect(),
            }]),
            Type::Struct(struct_id, args) => {
                let strukt = self.hir.item_tree.structs[*struct_id].clone();
                let params = strukt
                    .generics
                    .iter()
                    .chain(strukt.const_generics.iter())
                    .zip(args)
                    .map(|(name, ty)| (name.0.clone(), ty.clone()))
                    .collect::<HashMap<_, _>>();
                Some(vec![ConstructorInfo {
                    constructor: Constructor::Struct(*struct_id),
                    fields: strukt
                        .fields
                        .iter()
                        .map(|field| {
                            self.lower_type_ref_with_params_at(
                                &field.ty,
                                &params,
                                Some(field.ty_range),
                            )
                        })
                        .collect(),
                }])
            }
            Type::Unit => Some(vec![ConstructorInfo {
                constructor: Constructor::Unit,
                fields: Vec::new(),
            }]),
            Type::Never => Some(Vec::new()),
            _ => None,
        }
    }

    fn enum_constructors(&mut self, enum_id: EnumId, args: &[Type]) -> Vec<ConstructorInfo> {
        let enum_data = self.hir.item_tree.enums[enum_id].clone();
        let params = enum_data
            .generics
            .iter()
            .chain(enum_data.const_generics.iter())
            .zip(args)
            .map(|(name, ty)| (name.0.clone(), ty.clone()))
            .collect::<HashMap<_, _>>();
        enum_data
            .variants
            .iter()
            .enumerate()
            .map(|(index, variant)| ConstructorInfo {
                constructor: Constructor::EnumVariant(enum_id, index),
                fields: match &variant.kind {
                    HirVariantKind::Unit => Vec::new(),
                    HirVariantKind::Tuple(fields) => fields
                        .iter()
                        .enumerate()
                        .map(|(field_index, field)| {
                            self.lower_type_ref_with_params_at(
                                field,
                                &params,
                                variant
                                    .field_ranges
                                    .get(field_index)
                                    .copied()
                                    .or(Some(variant.name_range)),
                            )
                        })
                        .collect(),
                    HirVariantKind::Struct(fields) => fields
                        .iter()
                        .map(|field| {
                            self.lower_type_ref_with_params_at(
                                &field.ty,
                                &params,
                                Some(field.ty_range),
                            )
                        })
                        .collect(),
                },
            })
            .collect()
    }

    fn constructor_fields(&mut self, ty: &Type, constructor: &Constructor) -> Option<Vec<Type>> {
        match constructor {
            Constructor::Int(_)
            | Constructor::Float(_)
            | Constructor::String(_)
            | Constructor::Char(_) => Some(Vec::new()),
            _ => self
                .constructors(ty)?
                .into_iter()
                .find(|info| &info.constructor == constructor)
                .map(|info| info.fields),
        }
    }

    fn display_matrix_pattern(&self, pattern: &MatrixPat) -> String {
        match pattern {
            MatrixPat::Wildcard | MatrixPat::IntegerRanges { .. } | MatrixPat::Invalid => {
                "_".to_string()
            }
            MatrixPat::Constructor(constructor, fields) => match constructor {
                Constructor::Bool(value) => value.to_string(),
                Constructor::EnumVariant(enum_id, index) => {
                    let enum_data = &self.hir.item_tree.enums[*enum_id];
                    let variant = &enum_data.variants[*index];
                    let path = format!("{}::{}", enum_data.name.0, variant.name.0);
                    match &variant.kind {
                        HirVariantKind::Unit => path,
                        HirVariantKind::Tuple(_) => format!(
                            "{path}({})",
                            fields
                                .iter()
                                .map(|field| self.display_matrix_pattern(field))
                                .collect::<Vec<_>>()
                                .join(", ")
                        ),
                        HirVariantKind::Struct(items) => format!(
                            "{path} {{ {} }}",
                            items
                                .iter()
                                .zip(fields)
                                .map(|(item, field)| format!(
                                    "{}: {}",
                                    item.name.0,
                                    self.display_matrix_pattern(field)
                                ))
                                .collect::<Vec<_>>()
                                .join(", ")
                        ),
                    }
                }
                Constructor::Tuple => {
                    let single = fields.len() == 1;
                    let fields = fields
                        .iter()
                        .map(|field| self.display_matrix_pattern(field))
                        .collect::<Vec<_>>()
                        .join(", ");
                    if single {
                        format!("({fields},)")
                    } else {
                        format!("({fields})")
                    }
                }
                Constructor::Array => "_".to_string(),
                Constructor::Struct(struct_id) => {
                    let strukt = &self.hir.item_tree.structs[*struct_id];
                    format!(
                        "{} {{ {} }}",
                        strukt.name.0,
                        strukt
                            .fields
                            .iter()
                            .zip(fields)
                            .map(|(item, field)| format!(
                                "{}: {}",
                                item.name.0,
                                self.display_matrix_pattern(field)
                            ))
                            .collect::<Vec<_>>()
                            .join(", ")
                    )
                }
                Constructor::Unit => "()".to_string(),
                Constructor::Int(value) => value.to_string(),
                Constructor::Float(bits) => f64::from_bits(*bits).to_string(),
                Constructor::String(value) => format!("{value:?}"),
                Constructor::Char(value) => format!("'{value}'"),
            },
        }
    }
}

fn reference_pattern_coverage_type(ty: &Type) -> Type {
    let mut ty = ty.clone();
    loop {
        let Type::Ref(inner, mutable) = &ty else {
            return ty;
        };
        if !mutable && inner.as_ref() == &Type::Str {
            return ty;
        }
        ty = inner.as_ref().clone();
    }
}

fn literal_constructor(
    literal: LiteralPattern,
    expected: &Type,
    pointer_bits: u32,
) -> Option<Constructor> {
    match literal {
        LiteralPattern::Int {
            value,
            suffix,
            valid,
        } => {
            let ty = integer_type(expected)?;
            (valid
                && ty.contains_u64(value, pointer_bits)
                && suffix
                    .as_deref()
                    .is_none_or(|suffix| IntTy::parse(suffix) == Some(ty)))
            .then_some(Constructor::Int(value))
        }
        LiteralPattern::Float {
            value,
            suffix,
            valid,
        } => {
            let ty = match expected {
                Type::Float(ty) => *ty,
                Type::InferFloat => FloatTy::F64,
                _ => return None,
            };
            (valid
                && suffix
                    .as_deref()
                    .is_none_or(|suffix| FloatTy::parse(suffix) == Some(ty)))
            .then_some(Constructor::Float(value.to_bits()))
        }
        LiteralPattern::String(value) if matches!(expected, Type::Ref(inner, false) if **inner == Type::Str) => {
            Some(Constructor::String(value))
        }
        LiteralPattern::Char(value) if expected == &Type::Char => Some(Constructor::Char(value)),
        LiteralPattern::Bool(value) if expected == &Type::Bool => Some(Constructor::Bool(value)),
        LiteralPattern::String(_) | LiteralPattern::Char(_) | LiteralPattern::Bool(_) => None,
    }
}

const fn integer_type(ty: &Type) -> Option<IntTy> {
    match ty {
        Type::Int(ty) => Some(*ty),
        Type::InferInt => Some(IntTy::I32),
        _ => None,
    }
}

fn integer_literals(matrix: &[Vec<MatrixPat>], ty: IntTy, pointer_bits: u32) -> Vec<(u128, u64)> {
    let mut literals = matrix
        .iter()
        .filter_map(|row| match row.first()? {
            MatrixPat::Constructor(Constructor::Int(value), fields) if fields.is_empty() => {
                integer_ordinal(ty, *value, pointer_bits).map(|ordinal| (ordinal, *value))
            }
            _ => None,
        })
        .collect::<Vec<_>>();
    literals.sort_unstable_by_key(|(ordinal, _)| *ordinal);
    literals.dedup_by_key(|(ordinal, _)| *ordinal);
    literals
}

fn uncovered_integer_ranges(
    matrix: &[Vec<MatrixPat>],
    ty: IntTy,
    pointer_bits: u32,
) -> Vec<IntegerRange> {
    let max = integer_max_ordinal(ty, pointer_bits);
    let mut next = Some(0u128);
    let mut ranges = Vec::new();

    for (ordinal, _) in integer_literals(matrix, ty, pointer_bits) {
        let Some(start) = next else {
            break;
        };
        if ordinal < start {
            continue;
        }
        if start < ordinal {
            ranges.push(IntegerRange {
                start,
                end: ordinal - 1,
            });
        }
        next = ordinal.checked_add(1);
    }

    if let Some(start) = next
        && start <= max
    {
        ranges.push(IntegerRange { start, end: max });
    }
    ranges
}

fn integer_range_notes(
    pattern: &MatrixPat,
    displayed_pattern: &str,
    pointer_bits: u32,
) -> Vec<String> {
    let mut uncovered = Vec::new();
    collect_integer_ranges(pattern, &mut uncovered);
    let positions = uncovered.len();
    uncovered
        .into_iter()
        .enumerate()
        .map(|(index, (ty, ranges))| {
            let omitted = ranges.len().saturating_sub(MAX_DISPLAYED_INTEGER_RANGES);
            let mut ranges = ranges
                .iter()
                .take(MAX_DISPLAYED_INTEGER_RANGES)
                .map(|range| format!("`{}`", display_integer_range(ty, *range, pointer_bits)))
                .collect::<Vec<_>>();
            if omitted != 0 {
                ranges.push(format!("and {omitted} more"));
            }
            let location = if positions == 1 {
                format!("for `{displayed_pattern}`")
            } else {
                format!(
                    "for integer position {} in `{displayed_pattern}`",
                    index + 1
                )
            };
            format!(
                "uncovered {} ranges {location}: {}",
                ty.as_str(),
                ranges.join(", ")
            )
        })
        .collect()
}

fn collect_integer_ranges<'a>(
    pattern: &'a MatrixPat,
    uncovered: &mut Vec<(IntTy, &'a [IntegerRange])>,
) {
    match pattern {
        MatrixPat::IntegerRanges { ty, ranges } => uncovered.push((*ty, ranges)),
        MatrixPat::Constructor(_, fields) => {
            for field in fields {
                collect_integer_ranges(field, uncovered);
            }
        }
        MatrixPat::Wildcard | MatrixPat::Invalid => {}
    }
}

fn display_integer_range(ty: IntTy, range: IntegerRange, pointer_bits: u32) -> String {
    let start = integer_value(ty, range.start, pointer_bits);
    if range.start == range.end {
        start
    } else {
        format!("{start}..={}", integer_value(ty, range.end, pointer_bits))
    }
}

fn integer_ordinal(ty: IntTy, value: u64, pointer_bits: u32) -> Option<u128> {
    let bits = ty.width_bits(pointer_bits);
    if !ty.is_signed() {
        let value = u128::from(value);
        return (value <= integer_max_ordinal(ty, pointer_bits)).then_some(value);
    }

    let half = 1u128 << (bits - 1);
    let value = u128::from(value);
    (value < half).then_some(half + value)
}

fn integer_value(ty: IntTy, ordinal: u128, pointer_bits: u32) -> String {
    let bits = ty.width_bits(pointer_bits);
    if !ty.is_signed() {
        return ordinal.to_string();
    }
    let min = -(1i128 << (bits - 1));
    let ordinal = i128::try_from(ordinal).expect("integer ordinal should fit in i128");
    (min + ordinal).to_string()
}

const fn integer_max_ordinal(ty: IntTy, pointer_bits: u32) -> u128 {
    (1u128 << ty.width_bits(pointer_bits)) - 1
}

const fn type_head(ty: &Type) -> TypeHead {
    match ty {
        Type::Tuple(fields) => TypeHead::Tuple(fields.len()),
        Type::Struct(id, _) => TypeHead::Struct(*id),
        Type::Enum(id, _) => TypeHead::Enum(*id),
        other => TypeHead::Other(discriminant(other)),
    }
}

fn type_size(ty: &Type) -> usize {
    match ty {
        Type::Ref(inner, _)
        | Type::Ptr { inner, .. }
        | Type::Slice(inner)
        | Type::Array(inner, _) => 1usize.saturating_add(type_size(inner)),
        Type::Tuple(fields) | Type::Struct(_, fields) | Type::Enum(_, fields) => fields
            .iter()
            .fold(1usize, |size, field| size.saturating_add(type_size(field))),
        _ => 1,
    }
}

fn specialize(
    matrix: &[Vec<MatrixPat>],
    constructor: &Constructor,
    arity: usize,
) -> Vec<Vec<MatrixPat>> {
    matrix
        .iter()
        .filter_map(|row| {
            let (head, tail) = row.split_first()?;
            let mut specialized = match head {
                MatrixPat::Wildcard => vec![MatrixPat::Wildcard; arity],
                MatrixPat::Constructor(found, fields)
                    if found == constructor && fields.len() == arity =>
                {
                    fields.clone()
                }
                MatrixPat::Constructor(_, _)
                | MatrixPat::IntegerRanges { .. }
                | MatrixPat::Invalid => return None,
            };
            specialized.extend_from_slice(tail);
            Some(specialized)
        })
        .collect()
}

fn default_matrix(matrix: &[Vec<MatrixPat>]) -> Vec<Vec<MatrixPat>> {
    matrix
        .iter()
        .filter_map(|row| {
            let (head, tail) = row.split_first()?;
            matches!(head, MatrixPat::Wildcard).then(|| tail.to_vec())
        })
        .collect()
}

fn rebuild_witness(
    constructor: &Constructor,
    arity: usize,
    mut witness: Vec<MatrixPat>,
) -> Vec<MatrixPat> {
    let tail = witness.split_off(arity);
    let mut result = vec![MatrixPat::Constructor(constructor.clone(), witness)];
    result.extend(tail);
    result
}
