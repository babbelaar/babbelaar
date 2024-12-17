// Copyright (C) 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

//! The compiler class takes a [`ParseTree`] as input, turns it into IR, runs
//! the optimizer, and generates machine code.

use std::rc::Rc;

use babbelaar::*;
use log::debug;

use crate::{ir::FunctionArgument, optimize_program, ArgumentList, FunctionAttribute, FunctionBuilder, Immediate, JumpCondition, MathOperation, Operand, PrimitiveType, Program, ProgramBuilder, Register, TypeId, TypeInfo};

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
                panic!();
            };

            self.program_builder.add_structure(structure);
        }
    }

    fn add_function_declarations(&mut self, trees: &[ParseTree]) {
        for statement in trees.iter().flat_map(|t| t.functions()) {
            let StatementKind::Function(func) = &statement.kind else {
                panic!();
            };

            self.analyze_function_attributes(func, func.name.value().clone(), &statement.attributes);
        }
    }

    fn compile_functions(&mut self, trees: &[ParseTree]) {
        for statement in trees.iter().flat_map(|t| t.functions()) {
            let StatementKind::Function(func) = &statement.kind else {
                panic!();
            };

            self.compile_function(func, func.name.value().clone(), CallingConvention::Regular);
        }
    }

    fn compile_methods(&mut self, trees: &[ParseTree]) {
        for statement in trees.iter().flat_map(|t| t.structures()) {
            let StatementKind::Structure(structure) = &statement.kind else {
                panic!();
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

        let mut arguments = ArgumentList::new();

        match call_convention {
            CallingConvention::Regular => (),
            CallingConvention::Method { this } => {
                arguments.add_this(this);
            }
        }

        for parameter in &func.parameters {
            let type_id = self.program_builder.type_id_for_structure(&parameter.ty.specifier.unqualified_name());
            arguments.add(parameter.name.value(), type_id);
        }

        self.program_builder.build_function(name, arguments, |builder| {
            for statement in body {
                statement.compile(builder);
            }

            // Make sure the main function always returns a known value. If there was a return statement,
            // this will be removed by DCE.
            if func.name.value().as_str() == Constants::MAIN_FUNCTION {
                let fallback_value_zero = builder.load_immediate(Immediate::Integer8(0));
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
    fn compile(&self, builder: &mut FunctionBuilder);
}

impl CompileStatement for Statement {
    fn compile(&self, builder: &mut FunctionBuilder) {
        match &self.kind {
            StatementKind::Assignment(statement) => {
                statement.compile(builder);
            }

            StatementKind::Expression(expression) => {
                _ = expression.compile(builder);
            }

            StatementKind::Extension(statement) => {
                statement.compile(builder);
            }

            StatementKind::Function(statement) => {
                statement.compile(builder);
            }

            StatementKind::For(statement) => {
                statement.compile(builder);
            }

            StatementKind::If(statement) => {
                statement.compile(builder);
            }

            StatementKind::Interface(statement) => {
                statement.compile(builder);
            }

            StatementKind::Return(statement) => {
                statement.compile(builder);
            }

            StatementKind::Structure(statement) => {
                _ = statement;
            }

            StatementKind::Variable(statement) => {
                statement.compile(builder);
            }
        }
    }
}

impl CompileStatement for AssignStatement {
    fn compile(&self, builder: &mut FunctionBuilder) {
        let (rhs, rhs_ty) = self.source.compile(builder).to_readable_and_type(builder);

        let source = match self.kind.value() {
            AssignKind::Regular => rhs,
            AssignKind::Math(op) => {
                let lhs = self.destination.compile(builder).to_readable_and_type(builder);
                compile_math_op(builder, *op, lhs, (rhs, rhs_ty)).to_readable(builder)
            }
        };

        if self.destination.value().as_identifier() == Some(&Constants::DISCARDING_IDENT) {
            return;
        }

        let dst = self.destination.compile(builder);
        debug!("Dst is: {dst:#?}");

        match dst.kind {
            ExpressionResultKind::Register(destination) => {
                builder.move_register(destination, source);
            }

            ExpressionResultKind::Comparison(..) => todo!(),

            ExpressionResultKind::PointerRegister { base_ptr, offset, typ } => {
                builder.store_ptr(base_ptr, offset, source, typ);
            }
        }
    }
}

impl CompileStatement for ExtensionStatement {
    fn compile(&self, builder: &mut FunctionBuilder) {
        _ = builder;
        todo!();
    }
}

impl CompileStatement for FunctionStatement {
    fn compile(&self, builder: &mut FunctionBuilder) {
        _ = builder;
        todo!();
    }
}

impl CompileStatement for ForStatement {
    fn compile(&self, builder: &mut FunctionBuilder) {
        let after = builder.create_label("na-volg");

        match self.iterable.value() {
            ForIterableKind::Expression(expression) => {
                let (reg, ty) = expression.compile(builder).to_readable_and_type(builder);

                if ty.type_id() == TypeId::SLINGER {
                    let str_ptr_reg = reg;
                    let current_value = builder.load_immediate(Immediate::Integer64(0));

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

                    builder.compare(current_value, end);
                    builder.jump_if_greater_or_equal(after);

                    let body = builder.create_label_and_link_here("volg-lichaam");

                    let char_reg = builder.load_ptr(str_ptr_reg, Operand::Register(current_value), PrimitiveType::new(4, false));
                    builder.associate_register_to_local(char_reg, self.iterator_name.value(), ty);


                    for statement in &self.body {
                        statement.compile(builder);
                    }

                    builder.increment(current_value);

                    builder.compare(current_value, end);
                    builder.jump_if_less(body);
                } else {
                    println!("{builder:#?}");
                    todo!("ondersteun Doorloper-gebaseerde expressies met `volg` en type {ty:?}");
                }
            }

            ForIterableKind::Range(range) => {
                let (current_value, ty) = range.start.compile(builder).to_readable_and_type(builder);

                builder.associate_register_to_local(current_value, self.iterator_name.value(), ty);

                let end = range.end.compile(builder).to_readable(builder);

                builder.compare(current_value, end);
                builder.jump_if_greater_or_equal(after);

                let body = builder.create_label_and_link_here("volg-lichaam");

                for statement in &self.body {
                    statement.compile(builder);
                }

                builder.increment(current_value);

                builder.compare(current_value, end);
                builder.jump_if_less(body);
                // otherwise, fall through to the after the for-statement
            }
        }

        builder.link_label_here(after);
    }
}

impl CompileStatement for IfStatement {
    fn compile(&self, builder: &mut FunctionBuilder) {
        let after_block = builder.create_label(format!("na-als"));
        let then_block = builder.create_label(format!("als {}", self.condition.value()));

        let comparison = self.condition.compile(builder).to_comparison(builder);

        match comparison {
            Comparison::Equality => {
                builder.jump_if_equal(then_block);
                builder.jump(after_block);
            }

            Comparison::Inequality => {
                builder.jump_if_not_equal(then_block);
                builder.jump(after_block);
            }

            _ => todo!(),
        };

        builder.link_label_here(then_block);
        for statement in &self.body {
            statement.compile(builder);
        }

        builder.link_label_here(after_block);
    }
}

impl CompileStatement for InterfaceStatement {
    fn compile(&self, builder: &mut FunctionBuilder) {
        _ = builder;
        todo!();
    }
}

impl CompileStatement for ReturnStatement {
    fn compile(&self, builder: &mut FunctionBuilder) {
        match &self.expression {
            Some(expression) => {
                let register = expression.compile(builder).to_readable(builder);
                builder.ret_with(register);
            }

            None => {
                builder.ret();
            }
        }
    }
}

impl CompileStatement for VariableStatement {
    fn compile(&self, builder: &mut FunctionBuilder) {
        let (register, ty) = self.expression.compile(builder).to_readable_and_type(builder);
        builder.associate_register_to_local(register, self.name.value(), ty);
    }
}

trait CompileExpression {
    fn compile(&self, builder: &mut FunctionBuilder) -> ExpressionResult;
}

impl CompileExpression for Expression {
    fn compile(&self, builder: &mut FunctionBuilder) -> ExpressionResult {
        match self {
            Expression::BiExpression(expression) => {
                expression.compile(builder)
            }

            Expression::Postfix(expression) => {
                expression.compile(builder)
            }

            Expression::Primary(expression) => {
                expression.compile(builder)
            }

            Expression::Unary(expression) => {
                expression.compile(builder)
            }
        }
    }
}

impl CompileExpression for BiExpression {
    fn compile(&self, builder: &mut FunctionBuilder) -> ExpressionResult {
        let (lhs, lhs_ty) = self.lhs.compile(builder).to_readable_and_type(builder);
        let (rhs, rhs_ty) = self.rhs.compile(builder).to_readable_and_type(builder);

        match self.operator.value() {
            BiOperator::Math(math) => {
                compile_math_op(builder, *math, (lhs, lhs_ty), (rhs, rhs_ty))
            }

            BiOperator::Comparison(comparison) => {
                assert!(lhs_ty.type_id().is_integer(), "Ongeldige type aan de linkerhand: {lhs_ty:?}");
                assert!(rhs_ty.type_id().is_integer(), "Ongeldige type aan de rechterhand: {rhs_ty:?}");
                builder.compare(lhs, rhs);
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
        assert_eq!(rhs_ty.type_id(), TypeId::SLINGER);

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
    builder.math(math_operation, lhs, rhs).into()
}

impl CompileExpression for PostfixExpression {
    fn compile(&self, builder: &mut FunctionBuilder) -> ExpressionResult {
        match self.kind.value() {
            PostfixExpressionKind::Call(call) => {
                let arguments = call.arguments.compile(builder);

                match self.lhs.value() {
                    Expression::Primary(PrimaryExpression::Reference(reference)) => {
                        builder.call(reference.value().clone(), arguments).into()
                    }

                    _ => todo!("Ondersteun aanroepexpressies met linkerzijde: {:#?}", self.lhs)
                }
            }

            PostfixExpressionKind::Member(member) => {
                let lhs = self.lhs.compile(builder);

                let (base_ptr, ty) = lhs.to_readable_and_type(builder);

                let field = builder.layout_of(ty.type_id()).field(&member);
                let offset = field.offset() as isize;
                let field_type = field.type_id();
                let primitive_typ = field.primitive_type();

                ExpressionResult::pointer(base_ptr, Operand::Immediate(Immediate::Integer64(offset as _)), field_type, primitive_typ)
            }

            PostfixExpressionKind::MethodCall(method) => {
                let lhs = self.lhs.compile(builder);
                let struct_ty = lhs.type_info.type_id();

                let mut arguments = method.call.arguments.compile(builder);

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

                builder.call(name, arguments).into()
            }

            PostfixExpressionKind::Subscript(subscript) => {
                let base_ptr = self.lhs.compile(builder);

                let (base_ptr, ty) = base_ptr.to_readable_and_type(builder);

                match ty {
                    TypeInfo::Array(item_ty) => {
                        let offset = subscript.compile(builder).to_readable(builder);

                        let size = builder.size_of_type_info(&item_ty);
                        let offset = builder.math(MathOperation::Multiply, offset, Immediate::Integer64(size as _));
                        let offset = Operand::Register(offset);

                        let typ = builder.primitive_type_of(&item_ty);
                        ExpressionResult::pointer(base_ptr, offset, *item_ty, typ)
                    }

                    TypeInfo::Plain(..) => todo!("Subscript for types"),
                }
            }
        }
    }
}

impl CompileExpression for PrimaryExpression {
    fn compile(&self, builder: &mut FunctionBuilder) -> ExpressionResult {
        match self {
            Self::Boolean(b) => {
                builder.load_immediate(Immediate::Integer32(*b as i32)).into()
            }

            Self::CharacterLiteral(c) => {
                builder.load_immediate(Immediate::Integer32(*c as i32)).into()
            }

            Self::IntegerLiteral(i) => {
                builder.load_immediate(Immediate::Integer64(*i)).into()
            }

            Self::Parenthesized(expression) => {
                expression.compile(builder)
            }

            Self::Reference(ranged) => {
                builder.load_local(ranged.value()).into()
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
                let mut offset = 0;
                let zero = builder.load_immediate(Immediate::Integer64(0));
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
                expression.compile(builder)
            }

            Self::TemplateString { parts } => {
                _ = parts;
                todo!()
            }
        }
    }
}

impl CompileExpression for StructureInstantiationExpression {
    fn compile(&self, builder: &mut FunctionBuilder) -> ExpressionResult {
        let (layout, register) = builder.allocate_structure(self.name.value());

        let ty = layout.type_id().clone();

        let default_values: Vec<(usize, PrimitiveType, Rc<Expression>)> = layout
            .fields()
            .iter()
            .filter_map(|field| {
                let offset = field.offset();
                let typ = field.primitive_type();
                let default_value_expression = Rc::clone(field.default_value_expression()?);
                Some((offset, typ, default_value_expression))
            })
            .collect();

        let fields: Vec<(usize, PrimitiveType, &FieldInstantiation)> = self.fields
            .iter()
            .map(|field| {
                let layout = layout.field(&field.name);
                (layout.offset(), layout.primitive_type(), field)
            })
            .collect();

        for (offset, typ, expression) in default_values {
            let value = expression.compile(builder).to_readable(builder);
            builder.store_ptr(register, Operand::Immediate(Immediate::Integer64(offset as _)), value, typ);
        }

        for (offset, typ,field) in fields {
            let value = field.value.compile(builder).to_readable(builder);
            builder.store_ptr(register, Operand::Immediate(Immediate::Integer64(offset as _)), value, typ);
        }

        ExpressionResult::typed(register, ty)
    }
}

impl CompileExpression for UnaryExpression {
    fn compile(&self, builder: &mut FunctionBuilder) -> ExpressionResult {
        let value = self.rhs.compile(builder);
        match self.kind.value() {
            UnaryExpressionKind::AddressOf => {
                let (register, _) = value.to_register_and_type(builder);
                ExpressionResult::typed(register, TypeInfo::Plain(TypeId::G64))
            }

            UnaryExpressionKind::Negate => {
                let value = value.to_readable(builder);
                builder.unary_negate(value).into()
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
    pub fn to_comparison(self, builder: &mut FunctionBuilder) -> Comparison {
        self.kind.to_comparison(builder)
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

impl From<Register> for ExpressionResult {
    fn from(value: Register) -> Self {
        Self {
            kind: ExpressionResultKind::Register(value),
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

impl From<(TypeInfo, Register)> for ExpressionResult {
    fn from(value: (TypeInfo, Register)) -> Self {
        Self::typed(value.1, value.0)
    }
}

#[derive(Debug, Clone)]
enum ExpressionResultKind {
    Comparison(Comparison),
    Register(Register),
    PointerRegister { base_ptr: Register, offset: Operand, typ: PrimitiveType },
}

impl ExpressionResultKind {
    #[must_use]
    fn to_comparison(self, builder: &mut FunctionBuilder<'_>) -> Comparison {
        match self {
            Self::Comparison(comparison) => comparison,
            Self::Register(register) => {
                // if the value is a register, we want to branch/jump when the value isn't 0 (false)
                builder.compare(register, Operand::Immediate(Immediate::Integer64(0)));
                Comparison::Inequality
            }
            Self::PointerRegister { base_ptr, offset, typ } => {
                // if the value is a register, we want to branch/jump when the value isn't 0 (false)
                let register = builder.load_ptr(base_ptr, offset, typ);
                builder.compare(register, Operand::Immediate(Immediate::Integer64(0)));
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
        }
    }

    #[must_use]
    fn to_register(self, builder: &mut FunctionBuilder<'_>) -> Register {
        match self {
            Self::Comparison(comparison) => {
                builder.load_condition(match comparison {
                    Comparison::Equality => JumpCondition::Equal,
                    Comparison::GreaterThan => JumpCondition::Greater,
                    Comparison::GreaterThanOrEqual => JumpCondition::GreaterOrEqual,
                    Comparison::Inequality => JumpCondition::NotEqual,
                    Comparison::LessThan => JumpCondition::Less,
                    Comparison::LessThanOrEqual => JumpCondition::LessOrEqual,
                })
            }

            Self::Register(reg) => reg,

            Self::PointerRegister { base_ptr, offset, .. } => {
                builder.math(MathOperation::Add, base_ptr, offset)
            }
        }
    }
}

trait CompileSome {
    type Output;

    fn compile(&self, builder: &mut FunctionBuilder) -> Self::Output;
}

impl CompileSome for Vec<Ranged<Expression>> {
    type Output = Vec<FunctionArgument>;

    fn compile(&self, builder: &mut FunctionBuilder) -> Self::Output {
        self.iter()
            .map(|x| {
                x.compile(builder).to_function_argument(builder)
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
