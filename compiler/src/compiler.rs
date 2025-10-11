// Copyright (C) 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

//! The compiler class takes a [`ParseTree`] as input, turns it into IR, runs
//! the optimizer, and generates machine code.

use std::rc::Rc;

use babbelaar::*;
use log::{debug, error};

use crate::{ir::FunctionArgument, optimize_program, ArgumentList, FunctionAttribute, FunctionBuilder, Immediate, Label, MathOperation, Operand, PrimitiveType, Program, ProgramBuilder, Register, TypeId, TypeInfo};

#[derive(Debug)]
pub struct Compiler {
    program_builder: ProgramBuilder,
}

impl Compiler {
    pub fn new() -> Self {
        Self {
            program_builder: ProgramBuilder::new(),
        }
    }

    pub fn compile_trees(&mut self, trees: &[ParseTree]) {
        self.layout_structures(trees);
        self.add_function_declarations(trees);
        self.compile_methods(trees);
        self.compile_functions(trees);
    }

    fn layout_structures(&mut self, trees: &[ParseTree]) {
        for statement in trees.iter().flat_map(|t| t.structures()) {
            let StatementKind::Structure(structure) = &statement.kind else {
                debug_assert!(false, "Verwachtte alleen `structuren`");
                continue;
            };

            self.program_builder.add_structure(structure);
        }
    }

    fn add_function_declarations(&mut self, trees: &[ParseTree]) {
        for func in Builtin::FUNCTIONS {
            let type_id = self.program_builder.type_id_for_builtin(func.return_type);
            self.program_builder.add_function_return_type(BabString::new_static(func.name), type_id);
        }

        for ty in Builtin::TYPES {
            for method in ty.methods() {
                let type_id = self.program_builder.type_id_for_builtin(method.return_type);
                let name = create_mangled_method_name(&ty.name(), &BabString::new_static(method.name));
                self.program_builder.add_function_return_type(name, type_id);
            }
        }

        for statement in trees.iter().flat_map(|x| x.all()) {
            match &statement.kind {
                StatementKind::Function(func) => {
                    self.analyze_function_attributes(func, func.name.value().clone(), &statement.attributes);

                    if let Some(ret) = &func.return_type {
                        let type_id = self.program_builder.type_id_for_structure(&ret.specifier.unqualified_name());
                        self.program_builder.add_function_return_type(func.name.value().clone(), type_id);
                    }
                }

                StatementKind::Structure(structure) => {
                    for method in &structure.methods {
                        if let Some(ret) = &method.function.return_type {
                            let type_id = self.program_builder.type_id_for_structure(&ret.specifier.unqualified_name());
                            let name = create_mangled_method_name(&structure.name, &method.function.name);
                            self.program_builder.add_function_return_type(name, type_id);
                        }
                    }
                }

                _ => ()
            }
        }
    }

    fn compile_functions(&mut self, trees: &[ParseTree]) {
        for statement in trees.iter().flat_map(|t| t.functions()) {
            let StatementKind::Function(func) = &statement.kind else {
                debug_assert!(false, "Verwachtte alleen `werkwijzen`");
                continue;
            };

            self.compile_function(func, func.name.value().clone(), CallingConvention::Regular);
        }
    }

    fn compile_methods(&mut self, trees: &[ParseTree]) {
        for statement in trees.iter().flat_map(|t| t.structures()) {
            let StatementKind::Structure(structure) = &statement.kind else {
                debug_assert!(false, "Verwachtte alleen `structuren`");
                continue;
            };

            let type_id = self.program_builder.type_id_for_structure(structure.name.value());

            for method in &structure.methods {
                let name = create_mangled_method_name(structure.name.value(), method.function.name.value());
                self.compile_function(&method.function, name, CallingConvention::Method {
                    this: type_id,
                });
            }
        }
    }

    #[must_use]
    pub fn finish(self) -> Program {
        let mut program = self.program_builder.build();
        optimize_program(&mut program);
        program
    }

    fn analyze_function_attributes(&mut self, func: &FunctionStatement, name: BabString, attributes: &[Ranged<Attribute>]) {
        for attr in attributes {
            if *attr.name == AttributeName::VarArgs {
                self.program_builder.add_function_attribute(name.clone(), FunctionAttribute::VarArgs {
                    after_n_normal_params: func.parameters.len(),
                });
            }

            if *attr.name == AttributeName::Extern {
                for arg in attr.arguments.value() {
                    if arg.name.value() == "naam" {
                        if let PrimaryExpression::StringLiteral(actual_name) = arg.value.value() {
                            self.program_builder.add_function_alias(&name, actual_name);
                            break;
                        }
                    }
                }
            }
        }
    }

    fn compile_function(&mut self, func: &FunctionStatement, name: BabString, call_convention: CallingConvention) {
        let Some(body) = &func.body else {
            return;
        };

        debug!("IR-code aan het genereren voor werkwijze `{name}`");

        let mut arguments = ArgumentList::new();

        match call_convention {
            CallingConvention::Regular => (),
            CallingConvention::Method { this } => {
                arguments.add_this(this);
            }
        }

        for parameter in &func.parameters {
            let type_id = self.program_builder.type_id_for_structure(&parameter.ty.specifier.unqualified_name());
            let mut type_info = TypeInfo::Plain(type_id);

            for qualifier in &parameter.ty.qualifiers {
                match qualifier.value() {
                    TypeQualifier::Array | TypeQualifier::Pointer => {
                        type_info = TypeInfo::Array(Box::new(type_info));
                    }
                }
            }

            arguments.add(parameter.name.value(), type_info);
            log::trace!("Argument {parameter:?}");
        }

        self.program_builder.build_function(name, arguments, |builder| {
            let mut ctx = FunctionContext::default();
            for statement in body {
                statement.compile(builder, &mut ctx);
            }

            // Make sure the main function always returns a known value. If there was a return statement,
            // this will be removed by DCE.
            if func.name.value().as_str() == Constants::MAIN_FUNCTION {
                let typ = builder.return_type();
                let fallback_value_zero = builder.load_immediate(Immediate::Integer8(0), typ);
                builder.ret_with(fallback_value_zero);
            } else {
                builder.ret();
            }
        });
    }
}

#[must_use]
fn create_mangled_method_name(structure: &BabString, method: &BabString) -> BabString {
    BabString::new(
        format!("{structure}__{method}")
    )
}

trait CompileStatement {
    fn compile(&self, builder: &mut FunctionBuilder, ctx: &mut FunctionContext);
}

impl CompileStatement for Statement {
    fn compile(&self, builder: &mut FunctionBuilder, ctx: &mut FunctionContext) {
        match &self.kind {
            StatementKind::Assignment(statement) => {
                statement.compile(builder, ctx);
            }

            StatementKind::Break => {
                let after = ctx.loops.last().expect("kan een `kap` hebben zonder lus!").after.clone();
                builder.jump(after);
            }

            StatementKind::Continue => {
                let body_begin = ctx.loops.last().expect("kan een `vervolg` hebben zonder lus!").next_start.clone();
                builder.jump(body_begin);
            }

            StatementKind::Expression(expression) => {
                _ = expression.compile(builder, ctx);
            }

            StatementKind::Extension(statement) => {
                statement.compile(builder, ctx);
            }

            StatementKind::Function(statement) => {
                statement.compile(builder, ctx);
            }

            StatementKind::For(statement) => {
                statement.compile(builder, ctx);
            }

            StatementKind::If(statement) => {
                statement.compile(builder, ctx);
            }

            StatementKind::Interface(statement) => {
                statement.compile(builder, ctx);
            }

            StatementKind::Return(statement) => {
                statement.compile(builder, ctx);
            }

            StatementKind::Structure(statement) => {
                _ = statement;
            }

            StatementKind::Variable(statement) => {
                statement.compile(builder, ctx);
            }
        }
    }
}

impl CompileStatement for AssignStatement {
    fn compile(&self, builder: &mut FunctionBuilder, ctx: &mut FunctionContext) {
        let (rhs, rhs_ty) = self.source.compile(builder, ctx).to_readable_and_type(builder);
        let rhs_primitive_ty = builder.primitive_type_of(&rhs_ty);

        let source = match self.kind.value() {
            AssignKind::Regular => rhs,
            AssignKind::Math(op) => {
                let lhs = self.destination.compile(builder, ctx).to_readable_and_type(builder);
                compile_math_op(builder, *op, lhs, (rhs, rhs_ty)).to_readable(builder)
            }
        };

        if self.destination.value().as_identifier() == Some(&Constants::DISCARDING_IDENT) {
            return;
        }

        let dst = self.destination.compile(builder, ctx);
        debug!("Dst is: {dst:#?}");

        match dst.kind {
            ExpressionResultKind::Register(destination) => {
                builder.move_register(destination, source, rhs_primitive_ty);
            }

            ExpressionResultKind::Comparison(..) => todo!(),

            ExpressionResultKind::PointerRegister { base_ptr, offset, typ } => {
                builder.store_ptr(base_ptr, offset, source, typ);
            }

            ExpressionResultKind::PointerRegisterExplicit { .. } => panic!("Cannot assign to explicit pointer reference"),
        }
    }
}

impl CompileStatement for ExtensionStatement {
    fn compile(&self, builder: &mut FunctionBuilder, ctx: &mut FunctionContext) {
        _ = builder;
        _ = ctx;
        todo!();
    }
}

impl CompileStatement for FunctionStatement {
    fn compile(&self, builder: &mut FunctionBuilder, ctx: &mut FunctionContext) {
        _ = builder;
        _ = ctx;
        todo!();
    }
}

impl CompileStatement for ForStatement {
    fn compile(&self, builder: &mut FunctionBuilder, ctx: &mut FunctionContext) {
        let after = builder.create_label("na-volg");
        let body = builder.create_label("volg-lichaam");
        let next_start = builder.create_label("volg-check");

        ctx.loops.push(LoopContext {
            body,
            after,
            next_start,
        });

        match self.iterable.value() {
            ForIterableKind::Expression(expression) => {
                let (reg, ty) = expression.compile(builder, ctx).to_readable_and_type(builder);

                if ty.type_id() == TypeId::SLINGER {
                    let str_ptr_reg = reg;
                    let indexer_typ = builder.pointer_primitive_type();
                    let current_value = builder.load_immediate(Immediate::Integer64(0), indexer_typ);

                    let end = builder.call(
                        create_mangled_method_name(&BuiltinType::Slinger.name(),
                        &BabString::new_static("lengte")),
                        [
                            FunctionArgument::new(
                                str_ptr_reg,
                                TypeInfo::Plain(TypeId::SLINGER),
                                builder.layout_of(TypeId::SLINGER).primitive_type()
                            )
                        ],
                    );

                    builder.compare(current_value, end, indexer_typ);
                    builder.jump_if_greater_or_equal(after);

                    builder.link_label_here(body);

                    let char_reg = builder.load_ptr(str_ptr_reg, Operand::Register(current_value), PrimitiveType::new(4, false));

                    if *self.iterator_name != Constants::DISCARDING_IDENT {
                        builder.associate_register_to_local(char_reg, self.iterator_name.value(), ty);
                    }

                    for statement in &self.body {
                        statement.compile(builder, ctx);
                    }

                    builder.link_label_here(next_start);
                    builder.increment(current_value, indexer_typ);

                    builder.compare(current_value, end, indexer_typ);
                    builder.jump_if_less(body);
                } else {
                    println!("{builder:#?}");
                    todo!("ondersteun Doorloper-gebaseerde expressies met `volg` en type {ty:?}");
                }
            }

            ForIterableKind::Range(range) => {
                let (current_value, ty) = range.start.compile(builder, ctx).to_readable_and_type(builder);
                let indexer_typ = builder.pointer_primitive_type();

                if *self.iterator_name != Constants::DISCARDING_IDENT {
                    builder.associate_register_to_local(current_value, self.iterator_name.value(), ty);
                }

                let end = range.end.compile(builder, ctx).to_readable(builder);

                builder.compare(current_value, end, indexer_typ);
                builder.jump_if_greater_or_equal(after);

                builder.link_label_here(body);

                for statement in &self.body {
                    statement.compile(builder, ctx);
                }

                builder.link_label_here(next_start);
                builder.increment(current_value, builder.pointer_primitive_type());

                builder.compare(current_value, end, indexer_typ);
                builder.jump_if_less(body);
                // otherwise, fall through to the after the for-statement
            }
        }

        let loop_ctx = ctx.loops.pop();
        debug_assert!(loop_ctx.is_some());
        debug_assert_eq!(loop_ctx.as_ref().unwrap().after, after);
        debug_assert_eq!(loop_ctx.as_ref().unwrap().body, body);

        builder.link_label_here(after);
    }
}

impl CompileStatement for IfStatement {
    fn compile(&self, builder: &mut FunctionBuilder, ctx: &mut FunctionContext) {
        let after_block = builder.create_label(format!("na-als"));
        let then_block = builder.create_label(format!("als {}", self.condition.value()));

        let comparison = self.condition.compile(builder, ctx).to_comparison(builder);

        builder.jump_if(then_block, comparison.into());
        builder.jump(after_block);

        builder.link_label_here(then_block);
        for statement in &self.body {
            statement.compile(builder, ctx);
        }

        builder.link_label_here(after_block);
    }
}

impl CompileStatement for InterfaceStatement {
    fn compile(&self, builder: &mut FunctionBuilder, ctx: &mut FunctionContext) {
        _ = builder;
        _ = ctx;
        todo!();
    }
}

impl CompileStatement for ReturnStatement {
    fn compile(&self, builder: &mut FunctionBuilder, ctx: &mut FunctionContext) {
        match &self.expression {
            Some(expression) => {
                let register = expression.compile(builder, ctx).to_readable(builder);
                builder.ret_with(register);
            }

            None => {
                builder.ret();
            }
        }
    }
}

impl CompileStatement for VariableStatement {
    fn compile(&self, builder: &mut FunctionBuilder, ctx: &mut FunctionContext) {
        let (register, ty) = self.expression.compile(builder, ctx).to_readable_and_type(builder);
        builder.associate_register_to_local(register, self.name.value(), ty);
    }
}

trait CompileExpression {
    fn compile(&self, builder: &mut FunctionBuilder, ctx: &mut FunctionContext) -> ExpressionResult;
}

impl CompileExpression for Expression {
    fn compile(&self, builder: &mut FunctionBuilder, ctx: &mut FunctionContext) -> ExpressionResult {
        match self {
            Expression::BiExpression(expression) => {
                expression.compile(builder, ctx)
            }

            Expression::Postfix(expression) => {
                expression.compile(builder, ctx)
            }

            Expression::Primary(expression) => {
                expression.compile(builder, ctx)
            }

            Expression::Unary(expression) => {
                expression.compile(builder, ctx)
            }
        }
    }
}

impl CompileExpression for BiExpression {
    fn compile(&self, builder: &mut FunctionBuilder, ctx: &mut FunctionContext) -> ExpressionResult {
        let (lhs, lhs_ty) = self.lhs.compile(builder, ctx).to_readable_and_type(builder);
        let (rhs, rhs_ty) = self.rhs.compile(builder, ctx).to_readable_and_type(builder);
        let ty = builder.primitive_type_of(&lhs_ty);

        match self.operator.value() {
            BiOperator::Math(math) => {
                compile_math_op(builder, *math, (lhs, lhs_ty), (rhs, rhs_ty))
            }

            BiOperator::Comparison(comparison) => {
                assert!(lhs_ty.type_id().is_integer(), "Ongeldige type aan de linkerhand: {lhs_ty:?}");
                assert!(rhs_ty.type_id().is_integer(), "Ongeldige type aan de rechterhand: {rhs_ty:?}");
                builder.compare(lhs, rhs, ty);
                comparison.into()
            }
        }
    }
}

#[must_use]
fn compile_math_op(builder: &mut FunctionBuilder, op: MathOperator, lhs: (Register, TypeInfo), rhs: (Register, TypeInfo)) -> ExpressionResult {
    let (lhs, lhs_ty) = lhs;
    let (rhs, rhs_ty) = rhs;

    if op == MathOperator::Add && lhs_ty.type_id() == TypeId::SLINGER {
        assert_eq!(rhs_ty.type_id(), TypeId::SLINGER, "Kan alleen een slinger en een slinger samenvoegen!");

        let func = create_mangled_method_name(&BabString::new_static("Slinger"), &BabString::new_static("voegSamen"));
        let new_str = builder.call(func, [
            FunctionArgument::new(lhs, lhs_ty, PrimitiveType::new(builder.pointer_size(), false)),
            FunctionArgument::new(rhs, rhs_ty, PrimitiveType::new(builder.pointer_size(), false)),
        ].to_vec());

        return ExpressionResult::typed(new_str, TypeId::SLINGER);
    }

    assert!(lhs_ty.type_id().is_integer(), "Ongeldige type aan de linkerhand: {lhs_ty:?}");
                assert!(rhs_ty.type_id().is_integer(), "Ongeldige type aan de rechterhand: {rhs_ty:?}");

    let math_operation = MathOperation::from(op);
    let typ = builder.primitive_type_of(&lhs_ty);
    let reg = builder.math(math_operation, typ, lhs, rhs);
    ExpressionResult::typed(reg, lhs_ty)
}

impl CompileExpression for PostfixExpression {
    fn compile(&self, builder: &mut FunctionBuilder, ctx: &mut FunctionContext) -> ExpressionResult {
        match self.kind.value() {
            PostfixExpressionKind::Call(call) => {
                let arguments = call.arguments.compile(builder, ctx);

                match self.lhs.value() {
                    Expression::Primary(PrimaryExpression::Reference(reference)) => {
                        let reg = builder.call(reference.value().clone(), arguments);
                        let ty = builder.return_type_of(reference.value());
                        ExpressionResult::typed(reg, ty)
                    }

                    _ => todo!("Ondersteun aanroepexpressies met linkerzijde: {:#?}", self.lhs)
                }
            }

            PostfixExpressionKind::Member(member) => {
                let lhs = self.lhs.compile(builder, ctx);

                let (base_ptr, ty) = lhs.to_readable_and_type(builder);

                let field = builder.layout_of(ty.type_id()).field(&member);
                let offset = field.offset() as isize;
                let field_type = field.type_id();
                let primitive_typ = field.primitive_type();

                ExpressionResult::pointer(base_ptr, Operand::Immediate(Immediate::Integer64(offset as _)), field_type, primitive_typ)
            }

            PostfixExpressionKind::MethodCall(method) => {
                let lhs = self.lhs.compile(builder, ctx);
                let struct_ty = lhs.type_info.type_id();

                let mut arguments = method.call.arguments.compile(builder, ctx);

                let (this_register, this_type) = lhs.to_readable_and_type(builder);
                let this_primitive_type = builder.primitive_type_of(&this_type);
                arguments.insert(0, FunctionArgument::new(
                    this_register,
                    this_type,
                    this_primitive_type,
                ));

                let name = create_mangled_method_name(
                    builder.layout_of(struct_ty).name(),
                    &method.method_name
                );

                let ty = builder.return_type_of(&name);
                let reg = builder.call(name, arguments);
                ExpressionResult::typed(reg, ty)
            }

            PostfixExpressionKind::Subscript(subscript) => {
                let base_ptr = self.lhs.compile(builder, ctx);

                let (base_ptr, ty) = base_ptr.to_readable_and_type(builder);

                match ty {
                    TypeInfo::Array(item_ty) | TypeInfo::Pointer(item_ty) => {
                        let offset = subscript.compile(builder, ctx).to_readable(builder);

                        let size = builder.size_of_type_info(&item_ty);
                        let offset_typ = builder.pointer_primitive_type();
                        let offset = builder.math(MathOperation::Multiply, offset_typ, offset, Immediate::Integer64(size as _));
                        // let offset = builder.math(MathOperation::Add, offset_typ, offset, Immediate::Integer64(builder.pointer_size() as _));
                        let offset = Operand::Register(offset);

                        let typ = builder.primitive_type_of(&item_ty);

                        log::trace!("Haalopp: {item_ty:#?} or {typ:#?}");
                        ExpressionResult::pointer(base_ptr, offset, *item_ty, typ)
                    }

                    TypeInfo::Plain(inner) => {
                        if inner == TypeId::SLINGER {
                            let offset = subscript.compile(builder, ctx).to_readable(builder);
                            let item_ty = TypeInfo::Plain(TypeId::TEKEN);

                            let size = 1;
                            let offset_typ = builder.pointer_primitive_type();
                            let offset = builder.math(MathOperation::Multiply, offset_typ, offset, Immediate::Integer64(size as _));
                            let offset = Operand::Register(offset);

                            let typ = PrimitiveType::new(1, false);
                            ExpressionResult::pointer(base_ptr, offset, item_ty, typ)
                        } else {
                            todo!("Ondersteun indexering van typen naast opeenvolgingen en slingers. Type: {}. Locatie: {}",
                                builder.name_of_type_id(inner), self.lhs.range().start())
                        }

                    }
                }
            }
        }
    }
}

impl CompileExpression for PrimaryExpression {
    fn compile(&self, builder: &mut FunctionBuilder, ctx: &mut FunctionContext) -> ExpressionResult {
        match self {
            Self::Boolean(b) => {
                ExpressionResult::typed(
                    builder.load_immediate(Immediate::Integer32(*b as i32), PrimitiveType::S32),
                    TypeInfo::Plain(TypeId::G32)
                )
            }

            Self::CharacterLiteral(c) => {
                ExpressionResult::typed(
                    builder.load_immediate(Immediate::Integer32(*c as i32), PrimitiveType::U32),
                    TypeInfo::Plain(TypeId::G32)
                )
            }

            Self::IntegerLiteral(i) => {
                ExpressionResult::typed(
                    builder.load_immediate(Immediate::Integer64(*i), PrimitiveType::S64),
                    TypeInfo::Plain(TypeId::G64)
                )
            }

            Self::Parenthesized(expression) => {
                expression.compile(builder, ctx)
            }

            Self::Reference(name) => {
                match builder.load_local(name.value()) {
                    Some(local) => local.into(),
                    None => {
                        error!("Ongeldige lokale variabele: {}", name.value());
                        ExpressionResult::typed(builder.load_immediate(Immediate::Integer64(0), PrimitiveType::U32), TypeInfo::Plain(TypeId::G32))
                    }
                }
            }

            Self::ReferenceThis => {
                builder.load_this().expect("`dit` is niet gebruikt binnen methode??").into()
            }

            Self::SizedArrayInitializer { typ, size } => {
                let Expression::Primary(PrimaryExpression::IntegerLiteral(size)) = size.value() else {
                    todo!("Ondersteun variabele-grootte lijsten");
                };

                assert!(*size > 0);
                let size = *size as usize;

                let (layout, ptr) = builder.allocate_array(&typ.specifier.unqualified_name(), size);
                let result = ExpressionResult::array(ptr, *layout.type_id());

                let mut space_to_zero = layout.size() * size;
                let mut offset = builder.pointer_size();
                let pointer_ty = builder.pointer_primitive_type();
                let zero = builder.load_immediate(Immediate::Integer64(0), pointer_ty);
                let pointer_size = builder.pointer_size();
                while space_to_zero >= builder.pointer_size() {
                    builder.store_ptr(ptr, Operand::Immediate(Immediate::Integer64(offset as i64)), zero, PrimitiveType::new(pointer_size, false));
                    space_to_zero -= pointer_size;
                    offset += pointer_size;
                }

                while space_to_zero > 0 {
                    builder.store_ptr(ptr, Operand::Immediate(Immediate::Integer64(offset as i64)), zero, PrimitiveType::new(1, false));
                    space_to_zero -= 1;
                    offset += 1;
                }

                result
            }

            Self::StringLiteral(literal) => {
                builder.load_string(&literal).into()
            }

            Self::StructureInstantiation(expression) => {
                expression.compile(builder, ctx)
            }

            Self::TemplateString { parts } => {
                _ = parts;
                todo!()
            }
        }
    }
}

impl CompileExpression for StructureInstantiationExpression {
    fn compile(&self, builder: &mut FunctionBuilder, ctx: &mut FunctionContext) -> ExpressionResult {
        let (layout, register) = builder.allocate_structure(self.name.value());

        let ty = layout.type_id().clone();

        let default_values: Vec<(isize, PrimitiveType, Rc<Expression>)> = layout
            .fields()
            .iter()
            .filter_map(|field| {
                let offset = field.offset();
                let typ = field.primitive_type();
                let default_value_expression = Rc::clone(field.default_value_expression()?);
                Some((offset, typ, default_value_expression))
            })
            .collect();

        let fields: Vec<(isize, PrimitiveType, &FieldInstantiation)> = self.fields
            .iter()
            .map(|field| {
                let layout = layout.field(&field.name);
                (layout.offset(), layout.primitive_type(), field)
            })
            .collect();

        for (offset, typ, expression) in default_values {
            let value = expression.compile(builder, ctx).to_readable(builder);
            builder.store_ptr(register, Operand::Immediate(Immediate::Integer64(offset as _)), value, typ);
        }

        for (offset, typ,field) in fields {
            let value = field.value.compile(builder, ctx).to_readable(builder);
            builder.store_ptr(register, Operand::Immediate(Immediate::Integer64(offset as _)), value, typ);
        }

        ExpressionResult::typed(register, ty)
    }
}

impl CompileExpression for UnaryExpression {
    fn compile(&self, builder: &mut FunctionBuilder, ctx: &mut FunctionContext) -> ExpressionResult {
        let value = self.rhs.compile(builder, ctx);
        match self.kind.value() {
            UnaryExpressionKind::AddressOf => {
                let (register, type_info) = value.to_register_and_type(builder);
                if let Expression::Primary(PrimaryExpression::Reference(reference)) = self.rhs.value() {
                    if let TypeInfo::Plain(_) = type_info {
                        if type_info.type_id().is_primitive() {
                            let (reg, _) = builder.promote_to_stack(reference.value().clone());
                            let typ = builder.primitive_type_of(&type_info);
                            return ExpressionResult::pointer_explicit(reg, Operand::Immediate(Immediate::Integer64(0)), type_info, typ);
                        }
                    }
                }
                ExpressionResult::typed(register, TypeInfo::Plain(TypeId::G64))
            }

            UnaryExpressionKind::Negate => {
                let (value, type_info) = value.to_readable_and_type(builder);
                let typ = builder.primitive_type_of(&type_info);
                ExpressionResult::typed(builder.unary_negate(typ, value), type_info)
            }

            UnaryExpressionKind::Not => {
                let (value, typ) = value.to_readable_and_type(builder);
                let typ = builder.primitive_type_of(&typ);
                let reg = builder.math(MathOperation::Xor, typ, value, Operand::Immediate(Immediate::Integer8(1)));
                ExpressionResult::typed(reg, TypeId::BOOL)
            }
        }
    }
}

#[derive(Debug, Clone)]
struct ExpressionResult {
    kind: ExpressionResultKind,
    type_info: TypeInfo,
}

impl ExpressionResult {
    #[must_use]
    pub fn typed(register: Register, type_info: impl Into<TypeInfo>) -> Self {
        Self {
            kind: ExpressionResultKind::Register(register),
            type_info: type_info.into(),
        }
    }

    #[must_use]
    pub fn array(register: Register, type_info: impl Into<TypeInfo>) -> Self {
        Self {
            kind: ExpressionResultKind::Register(register),
            type_info: TypeInfo::Array(Box::new(type_info.into())),
        }
    }

    #[must_use]
    pub fn pointer(base_ptr: Register, offset: Operand, type_info: impl Into<TypeInfo>, typ: PrimitiveType) -> Self {
        Self {
            kind: ExpressionResultKind::PointerRegister { base_ptr, offset, typ },
            type_info: type_info.into(),
        }
    }

    #[must_use]
    pub fn pointer_explicit(base_ptr: Register, offset: Operand, type_info: impl Into<TypeInfo>, typ: PrimitiveType) -> Self {
        Self {
            kind: ExpressionResultKind::PointerRegisterExplicit { base_ptr, offset, typ },
            type_info: type_info.into(),
        }
    }

    #[must_use]
    pub fn to_comparison(self, builder: &mut FunctionBuilder) -> Comparison {
        self.kind.to_comparison(builder, &self.type_info)
    }

    #[must_use]
    pub fn to_readable(self, builder: &mut FunctionBuilder) -> Register {
        self.kind.to_readable(builder)
    }

    #[must_use]
    pub fn to_function_argument(self, builder: &mut FunctionBuilder) -> FunctionArgument {
        let primitive_type = match &self.kind {
            ExpressionResultKind::PointerRegister { .. } => PrimitiveType::new(builder.pointer_size(), false),
            _ => builder.layout_of(self.type_info.type_id()).primitive_type(),
        };
        let register = self.kind.to_readable(builder);
        FunctionArgument::new(register, self.type_info, primitive_type)
    }

    #[must_use]
    pub fn to_register_and_type(self, builder: &mut FunctionBuilder) -> (Register, TypeInfo) {
        (self.kind.to_register(builder), self.type_info)
    }

    #[must_use]
    pub fn to_readable_and_type(self, builder: &mut FunctionBuilder) -> (Register, TypeInfo) {
        (self.kind.to_readable(builder), self.type_info)
    }
}

impl From<Comparison> for ExpressionResult {
    fn from(value: Comparison) -> Self {
        Self {
            kind: ExpressionResultKind::Comparison(value),
            type_info: TypeId::G32.into(),
        }
    }
}

impl From<&Comparison> for ExpressionResult {
    fn from(value: &Comparison) -> Self {
        Self {
            kind: ExpressionResultKind::Comparison(*value),
            type_info: TypeId::G32.into(),
        }
    }
}

impl From<&Register> for ExpressionResult {
    fn from(value: &Register) -> Self {
        Self {
            kind: ExpressionResultKind::Register(*value),
            type_info: TypeId::G32.into(),
        }
    }
}

impl From<(TypeId, Register)> for ExpressionResult {
    fn from(value: (TypeId, Register)) -> Self {
        Self::typed(value.1, value.0)
    }
}

impl From<(TypeInfo, Register, PrimitiveType)> for ExpressionResult {
    fn from(value: (TypeInfo, Register, PrimitiveType)) -> Self {
        let (type_info, register, primitive) = value;

        match type_info {
            TypeInfo::Array(el_ty) => {
                Self::array(register, *el_ty)
            }

            TypeInfo::Pointer(type_info) => {
                Self::pointer(register, Operand::zero(), *type_info, primitive)
            }

            TypeInfo::Plain(..) => {
                Self::typed(register, type_info)
            }
        }
    }
}

#[derive(Debug, Clone)]
enum ExpressionResultKind {
    Comparison(Comparison),
    Register(Register),
    PointerRegister { base_ptr: Register, offset: Operand, typ: PrimitiveType },
    /// TODO: these names kind of suck...
    PointerRegisterExplicit { base_ptr: Register, offset: Operand, typ: PrimitiveType },
}

impl ExpressionResultKind {
    #[must_use]
    fn to_comparison(self, builder: &mut FunctionBuilder<'_>, type_info: &TypeInfo) -> Comparison {
        let typ = builder.primitive_type_of(type_info);
        match self {
            Self::Comparison(comparison) => comparison,
            Self::Register(register) => {
                // if the value is a register, we want to branch/jump when the value isn't 0 (false)
                builder.compare(register, Operand::Immediate(Immediate::Integer64(0)), typ);
                Comparison::Inequality
            }
            Self::PointerRegister { base_ptr, offset, typ } => {
                // if the value is a register, we want to branch/jump when the value isn't 0 (false)
                let register = builder.load_ptr(base_ptr, offset, typ);
                builder.compare(register, Operand::Immediate(Immediate::Integer64(0)), typ);
                Comparison::Inequality
            }

            Self::PointerRegisterExplicit { base_ptr, offset, typ } => {
                // if the value is a register, we want to branch/jump when the value isn't 0 (false)
                let register = builder.math(MathOperation::Add, typ, base_ptr, offset);
                builder.compare(register, Operand::Immediate(Immediate::Integer64(0)), typ);
                Comparison::Inequality
            }
        }
    }

    #[must_use]
    fn to_readable(self, builder: &mut FunctionBuilder<'_>) -> Register {
        match self {
            Self::Comparison(..) => {
                self.to_register(builder)
            }

            Self::Register(reg) => reg,

            Self::PointerRegister { base_ptr, offset, typ } => {
                builder.load_ptr(base_ptr, offset, typ)
            }

            Self::PointerRegisterExplicit { base_ptr, offset, typ } => {
                builder.math(MathOperation::Add, typ, base_ptr, offset)
            }
        }
    }

    #[must_use]
    fn to_register(self, builder: &mut FunctionBuilder<'_>) -> Register {
        match self {
            Self::Comparison(comparison) => {
                builder.load_condition(comparison.into())
            }

            Self::Register(reg) => reg,

            Self::PointerRegister { base_ptr, offset, .. } => {
                let typ = builder.pointer_primitive_type();
                builder.math(MathOperation::Add, typ, base_ptr, offset)
            }

            Self::PointerRegisterExplicit { base_ptr, offset, typ } => {
                builder.math(MathOperation::Add, typ, base_ptr, offset)
            }
        }
    }
}

trait CompileSome {
    type Output;

    fn compile(&self, builder: &mut FunctionBuilder, ctx: &mut FunctionContext) -> Self::Output;
}

impl CompileSome for Vec<Ranged<Expression>> {
    type Output = Vec<FunctionArgument>;

    fn compile(&self, builder: &mut FunctionBuilder, ctx: &mut FunctionContext) -> Self::Output {
        self.iter()
            .map(|x| {
                x.compile(builder, ctx).to_function_argument(builder)
            })
            .collect()
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum CallingConvention {
    Regular,
    Method {
        this: TypeId,
    },
}

#[derive(Debug, Default)]
struct FunctionContext {
    loops: Vec<LoopContext>,
}

#[derive(Debug)]
struct LoopContext {
    /// The begin of the loop body
    body: Label,

    /// Instruction after the loop
    after: Label,
    next_start: Label,
}
