// Copyright (C) 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

use std::{collections::HashMap, path::{Path, PathBuf}, process::Command};

use babbelaar::*;
use inkwell::{
    basic_block::BasicBlock,
    builder::Builder,
    context::Context,
    module::Module,
    passes::PassBuilderOptions,
    targets::*,
    types::*,
    values::*,
    AddressSpace,
    IntPredicate,
    OptimizationLevel,
};

use log::error;

use crate::CompileError;

pub struct LlvmContext {
    context: Context,
    compiled_object_paths: Vec<PathBuf>,
    link_directories: Vec<PathBuf>,
    linked_object_paths: Vec<String>,
}

impl LlvmContext {
    pub fn new() -> Self {
        Self {
            context: Context::create(),
            compiled_object_paths: Vec::new(),
            link_directories: vec![
                "target/debug/".into(),
                "/usr/lib/".into(),
                "/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/lib".into(),
            ],
            linked_object_paths: vec![
                "babbelaar_builtin".into(),
                "System".into(),
            ]
        }
    }

    pub fn parse_tree(&mut self, tree: &ParseTree<'_>) -> Result<(), CompileError> {
        let module = self.context.create_module(&tree.module_name());
        let builder = self.context.create_builder();
        let mut compiler = Compiler::new(&self.context, &builder, &module);

        // Globals/declarations
        for statement in tree.functions() {
            match &statement.kind {
                StatementKind::Function(function) => {
                    _ = compiler.compile_prototype(&function.name, &function.parameters);
                }

                _ => (),
            }
        }

        // Definitions
        for statement in tree.statements() {
            let StatementKind::Function(func) = &statement.kind else {
                error!("Illegal top-level statement: {statement:#?}");
                return Err(CompileError::IllegalParseTreeState);
            };

            compiler.compile(func);
        }

        if let Err(e) = module.verify() {
            eprintln!("Module verification failed: {}", e.to_string());
            eprintln!("\n===== Module =====\n");
            module.print_to_stderr();
            panic!("Verification Failed");
        }

        let path = self.code_gen(&module);
        self.compiled_object_paths.push(path);

        Ok(())
    }

    pub fn finish(self) {
        let mut cmd = Command::new("ld");
        print!("ld ");

        for path in self.compiled_object_paths {
            cmd.arg(path.as_os_str());
            print!("{} ", path.display());
        }

        for path in self.link_directories {
            cmd.arg("-L");
            cmd.arg(path.as_os_str());
            print!("-L {} ", path.display());
        }

        for path in self.linked_object_paths {
            print!("-l{path} ");
            cmd.arg("-l");
            cmd.arg(path);
        }

        cmd.args(["-o", "babbelaar-programma"]);
        println!("-o babbelaar-programma");

        let mut child = cmd.spawn().unwrap();
        child.wait().unwrap();
    }

    fn code_gen(&self, module: &Module) -> PathBuf {
        Target::initialize_all(&InitializationConfig::default());
        let target_triple = TargetMachine::get_default_triple();
        let target = Target::from_triple(&target_triple).unwrap();
        let target_machine = target
            .create_target_machine(
                &target_triple,
                "generic",
                "",
                OptimizationLevel::None,
                RelocMode::PIC,
                CodeModel::Default,
            )
            .unwrap();

        let passes: &[&str] = &[
            "instcombine",
            "reassociate",
            "gvn",
            "simplifycfg",
            // "basic-aa",
            "mem2reg",
        ];

        module
            .run_passes(passes.join(",").as_str(), &target_machine, PassBuilderOptions::create())
            .unwrap();

        let path = format!("{}.babbelaar-object", module.get_name().to_string_lossy());
        target_machine.write_to_file(module, FileType::Object, Path::new(&path)).unwrap();

        PathBuf::from(path)
    }
}

struct Compiler<'a, 'ctx> {
    context: &'ctx Context,
    builder: &'a Builder<'ctx>,
    module: &'a Module<'ctx>,
    fn_value: Option<FunctionValue<'ctx>>,
    variables: Vec<HashMap<String, AnyValueEnum<'ctx>>>,
}

impl<'a, 'ctx> Compiler<'a, 'ctx> {
    pub fn new(
        context: &'ctx Context,
        builder: &'a Builder<'ctx>,
        module: &'a Module<'ctx>,
    ) -> Self {
        Self {
            context,
            builder,
            module,
            fn_value: None,
            variables: vec![HashMap::new()],
        }
    }

    pub fn compile(&mut self, func: &FunctionStatement<'_>) {
        let range = FileRange::default();
        self.compile_prototype("schrijf", &[
            Parameter {
                name: Ranged::new(range, "s".into()),
                ty: Ranged::new(range, Type {
                    specifier: Ranged::new(range, TypeSpecifier::BuiltIn(BuiltinType::Slinger)),
                }),
            }
        ]);

        self.compile_function(func)
    }

    fn compile_statement(&mut self, function: &FunctionValue, block: &BasicBlock, statement: &Statement) {
        match &statement.kind {
            StatementKind::Assignment(stmt) => self.compile_statement_assign(function, block, stmt),
            StatementKind::Function(..) => todo!(),
            StatementKind::Expression(expr) => { self.compile_expression(function, block, expr.value()); }
            StatementKind::For(stmt) => self.compile_statement_for(function, block, stmt),
            StatementKind::If(stmt) => self.compile_statement_if(function, block, stmt),
            StatementKind::Return(stmt) => self.compile_statement_return(function, block, stmt),
            StatementKind::Structure(..) => todo!("Structuren zijn nog niet ondersteund met LLVM"),
            StatementKind::Variable(stmt) => self.compile_statement_variable(function, block, stmt),
        }
    }

    fn compile_prototype(&mut self, name: &str, parameters: &[Parameter<'_>]) -> FunctionValue<'ctx> {
        let mut param_types = Vec::new();

        for param in parameters.iter() {
            param_types.push(self.create_basic_metadata_type(param.ty.value()));
        }

        let fn_type = self.context.void_type().fn_type(&param_types, false);

        self.module.add_function(&name, fn_type, None)
    }

    fn compile_function(&mut self, func: &FunctionStatement) {
        let Some(body) = &func.body else {
            return;
        };

        let function = self.module.get_function(&func.name).unwrap();

        let mut locals = HashMap::new();
        for (param, value) in func.parameters.iter().zip(function.get_params()) {
            locals.insert(param.name.value().clone(), value.as_any_value_enum());
        }
        self.variables.push(locals);

        self.fn_value = Some(function);

        let block = self.context.append_basic_block(function, "entry");
        self.builder.position_at_end(block);

        for statement in body {
            self.compile_statement(&function, &block, statement)
        }

        self.variables.pop().expect("We pushed, so we pop");
    }

    fn create_basic_metadata_type(&mut self, ty: &Type) -> BasicMetadataTypeEnum<'ctx> {
        match ty.specifier.value() {
           TypeSpecifier::BuiltIn(BuiltinType::Bool) => BasicMetadataTypeEnum::IntType(self.context.bool_type()),
           TypeSpecifier::BuiltIn(BuiltinType::G32) => BasicMetadataTypeEnum::IntType(self.context.i64_type()),
           TypeSpecifier::BuiltIn(BuiltinType::Null) => BasicMetadataTypeEnum::PointerType(self.context.ptr_type(AddressSpace::default())),
           TypeSpecifier::BuiltIn(BuiltinType::Slinger) => BasicMetadataTypeEnum::PointerType(self.context.ptr_type(AddressSpace::default())),
           TypeSpecifier::Custom { .. } => todo!(),
        }
    }

    fn compile_expression(&self, function: &FunctionValue, block: &BasicBlock, expr: &Expression) -> AnyValueEnum<'ctx> {
        match expr {
            Expression::Primary(expr) => self.compile_expression_primary(function, block, expr),

            Expression::BiExpression(bi) => {
                let lhs = self.compile_expression(function, block, bi.lhs.value());
                let rhs = self.compile_expression(function, block, bi.rhs.value());

                let AnyValueEnum::IntValue(lhs) = lhs else {
                    panic!("Invalid LHS: {lhs:#?}");
                };

                let AnyValueEnum::IntValue(rhs) = rhs else {
                    panic!("Invalid RHS: {rhs:#?}");
                };

                let value = match bi.operator.value() {
                    BiOperator::Comparison(comparison) => {
                        let op = match comparison {
                            Comparison::Equality => IntPredicate::EQ,
                            Comparison::Inequality => IntPredicate::NE,
                            Comparison::GreaterThan => IntPredicate::SGT, // TODO signed vs unsigned
                            Comparison::GreaterThanOrEqual => IntPredicate::SGE, // TODO signed vs unsigned
                            Comparison::LessThan => IntPredicate::SLT, // TODO signed vs unsigned
                            Comparison::LessThanOrEqual => IntPredicate::SLE, // TODO signed vs unsigned
                        };

                        self.builder.build_int_compare(op, lhs, rhs, "compare").unwrap()
                    }

                    BiOperator::Add => self.builder.build_int_add(lhs, rhs, "add").unwrap(),
                    BiOperator::Subtract => self.builder.build_int_sub(lhs, rhs, "add").unwrap(),
                    BiOperator::Divide => self.builder.build_int_signed_div(lhs, rhs, "add").unwrap(),
                    BiOperator::Multiply => self.builder.build_int_mul(lhs, rhs, "add").unwrap(),
                    BiOperator::Modulo => self.builder.build_int_signed_rem(lhs, rhs, "add").unwrap(),
                };

                AnyValueEnum::IntValue(value)
            }

            Expression::Postfix(postfix) => {
                match &postfix.kind {
                    PostfixExpressionKind::Call(call) => {
                        let args: Vec<BasicMetadataValueEnum> = call.arguments.iter()
                            .map(|x| self.compile_expression(function, block, x.value()))
                            .map(|x| self.create_basic_metadata_value_enum(x))
                            .collect();

                        let retval = match postfix.lhs.value() {
                            Expression::Primary(PrimaryExpression::Reference(reference)) => {
                                let func = self.module.get_function(reference.value()).expect(&format!("ICE: Werkwijze niet gevonden {}", reference.value()));
                                self.builder.build_direct_call(func, &args, "callExpr").expect(&format!("Failed to call: {}", reference.value()))
                            }

                            other_expr => {
                                let lhs = self.compile_expression(function, block, other_expr).into_pointer_value();
                                let ty = self.context.i8_type().fn_type(&[], false); // TODO
                                self.builder.build_indirect_call(ty, lhs, &args, "callExpr").unwrap()
                            }
                        };

                        retval.as_any_value_enum()
                    }

                    _ => todo!()
                }
            }
        }
    }

    fn compile_expression_primary(&self, function: &FunctionValue, block: &BasicBlock, expr: &PrimaryExpression) -> AnyValueEnum<'ctx> {
        match expr {
            PrimaryExpression::Boolean(b) => {
                AnyValueEnum::IntValue(self.context.bool_type().const_int(*b as u64, false))
            }

            PrimaryExpression::IntegerLiteral(i) => {
                AnyValueEnum::IntValue(self.context.i64_type().const_int(*i as u64, false))
            }

            PrimaryExpression::Parenthesized(expr) => {
                self.compile_expression(function, block, expr.value())
            }

            PrimaryExpression::Reference(reference) => {
                let name: &str = reference.value();
                self.variables.iter().rev().flat_map(|x| x.get(name))
                    .next()
                    .copied()
                    .expect("variable not stored yet, SemanticAnalysis what are you doing?")
            }

            PrimaryExpression::StringLiteral(literal) => {
                self.builder.build_global_string_ptr(&literal, "value").unwrap().as_pointer_value().into()
            }

            PrimaryExpression::StructureInstantiation(structure) => {
                _ = structure;
                todo!("Structuren zijn nog niet ondersteund met LLVM")
            }

            PrimaryExpression::ReferenceThis => {
                todo!("`dit` is nog niet ondersteund met LLVM")
            }

            PrimaryExpression::TemplateString { parts } => {
                _ = parts;
                todo!()
            }
        }
    }

    fn compile_statement_assign(&mut self, function: &FunctionValue, block: &BasicBlock, stmt: &AssignStatement) {
        todo!("Toewijzingen zijn nog niet ondersteund met LLVM")

        // for stack in self.variables.iter().rev() {
        //     let Some(variable) = stack.get(&stmt.name.to_string()) else {
        //         continue;
        //     };

        //     let value = self.compile_expression(function, block, stmt.expression.value());
        //     let value = self.create_basic_value(value);

        //     self.builder.build_store(variable.clone().into_pointer_value(), value).unwrap();
        //     break;
        // }
    }

    fn compile_statement_for(&mut self, function: &FunctionValue, block: &BasicBlock, stmt: &ForStatement) {
        let body_block = self.context.append_basic_block(*function, "for");

        let start = self.compile_expression_primary(function, block, &stmt.range.start);
        let start = self.create_basic_value(start);

        let BasicValueEnum::IntValue(start) = start else {
            panic!("Invalid start: {start:#?}");
        };

        self.builder.build_unconditional_branch(body_block).unwrap();
        self.builder.position_at_end(body_block);

        let variable = self.builder.build_phi(self.context.i64_type(), &stmt.iterator_name).unwrap();
        variable.add_incoming(&[(&start, *block)]);

        self.variables.last_mut().unwrap().insert(stmt.iterator_name.to_string(), variable.as_any_value_enum());

        for statement in &stmt.body {
            self.compile_statement(function, &body_block, statement);
        }

        let end = self.compile_expression_primary(function, block, &stmt.range.end);
        let end = self.create_basic_value(end);

        let BasicValueEnum::IntValue(end) = end else {
            panic!("Invalid end: {end:#?}");
        };

        let current = variable.as_basic_value();
        let BasicValueEnum::IntValue(current) = current else {
            panic!("Invalid current: {current:#?}");
        };

        let new_value = self.builder.build_int_add(current, self.context.i64_type().const_int(1, false), "increment").unwrap();
        variable.add_incoming(&[(&new_value, body_block)]);

        let after_loop = self.context.append_basic_block(*function, "after loop");

        let comp = self.builder.build_int_compare(IntPredicate::SLT, new_value, end, "range comparison").unwrap();

        self.builder.build_conditional_branch(comp, body_block, after_loop).unwrap();

        self.builder.position_at_end(after_loop);
    }

    fn compile_statement_if(&mut self, function: &FunctionValue, block: &BasicBlock, stmt: &IfStatement) {
        let then_block = self.context.append_basic_block(*function, "then");
        let else_block = self.context.append_basic_block(*function, "else");
        let comparison = self.compile_expression(function, block, &stmt.condition);

        let AnyValueEnum::IntValue(comparison) = comparison else {
            panic!("Invalid comparison: {comparison:#?}");
        };

        self.builder.build_conditional_branch(comparison, then_block, else_block).unwrap();

        self.builder.position_at_end(then_block);
        for statement in &stmt.body {
            self.compile_statement(function, &then_block, statement);
        }

        self.builder.position_at_end(*block);
    }

    fn compile_statement_return(&mut self, function: &FunctionValue, block: &BasicBlock, stmt: &ReturnStatement) {
        if let Some(expr) = stmt.expression.as_ref() {
            let value = self.compile_expression(function, block, expr.value());
            let value = self.create_basic_value(value);
            self.builder.build_return(Some(&value)).unwrap();
        } else {
            self.builder.build_return(None).unwrap();
        }
    }

    fn compile_statement_variable(&mut self, function: &FunctionValue, block: &BasicBlock, stmt: &VariableStatement) {
        let storage = self.create_entry_block_alloca("variableStorage");
        let value = self.compile_expression(function, block, stmt.expression.value());
        let value = self.create_basic_value(value);
        self.builder.build_store(storage, value).unwrap();

        self.variables.last_mut().unwrap().insert(stmt.name.to_string(), storage.into());
    }

    fn create_entry_block_alloca(&self, name: &str) -> PointerValue<'ctx> {
        let builder = self.context.create_builder();

        let entry = self.fn_value.unwrap().get_first_basic_block().unwrap();

        match entry.get_first_instruction() {
            Some(first_instr) => builder.position_before(&first_instr),
            None => builder.position_at_end(entry),
        }

        builder.build_alloca(self.context.f64_type(), name).unwrap()
    }

    fn create_basic_metadata_value_enum(&self, value: AnyValueEnum<'ctx>) -> BasicMetadataValueEnum<'ctx> {
        match value {
            AnyValueEnum::ArrayValue(value) => BasicMetadataValueEnum::ArrayValue(value),
            AnyValueEnum::IntValue(value) => BasicMetadataValueEnum::IntValue(value),
            AnyValueEnum::FloatValue(value) => BasicMetadataValueEnum::FloatValue(value),
            AnyValueEnum::PointerValue(value) => BasicMetadataValueEnum::PointerValue(value),
            AnyValueEnum::StructValue(value) => BasicMetadataValueEnum::StructValue(value),
            AnyValueEnum::VectorValue(value) => BasicMetadataValueEnum::VectorValue(value),
            AnyValueEnum::MetadataValue(value) => BasicMetadataValueEnum::MetadataValue(value),
            AnyValueEnum::PhiValue(value) => value.as_basic_value().into(),

            AnyValueEnum::FunctionValue(..) => {
                todo!("Cannot coerce FunctionValue to BasicMetadataValueEnum")
            }

            AnyValueEnum::InstructionValue(..) => {
                todo!("Cannot coerce InstructionValue to BasicMetadataValueEnum");
            }
        }
    }

    fn create_basic_value(&self, value: AnyValueEnum<'ctx>) -> BasicValueEnum<'ctx> {
        match value {
            AnyValueEnum::ArrayValue(value) => BasicValueEnum::ArrayValue(value),
            AnyValueEnum::IntValue(value) => BasicValueEnum::IntValue(value),
            AnyValueEnum::FloatValue(value) => BasicValueEnum::FloatValue(value),
            AnyValueEnum::PointerValue(value) => BasicValueEnum::PointerValue(value),
            AnyValueEnum::StructValue(value) => BasicValueEnum::StructValue(value),
            AnyValueEnum::VectorValue(value) => BasicValueEnum::VectorValue(value),
            AnyValueEnum::PhiValue(value) => value.as_basic_value().into(),

            AnyValueEnum::FunctionValue(..) => {
                todo!("Cannot coerce FunctionValue to BasicValueEnum")
            }

            AnyValueEnum::MetadataValue(..) => {
                todo!("Cannot coerce MetadataValue to BasicValueEnum")
            }

            AnyValueEnum::InstructionValue(..) => {
                todo!("Cannot coerce InstructionValue to BasicValueEnum");
            }
        }
    }
}
