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

        for statement in tree.statements() {
            let StatementKind::Function(func) = &statement.kind else {
                error!("Illegal top-level statement: {statement:#?}");
                return Err(CompileError::IllegalParseTreeState);
            };

            Compiler::compile(&self.context, &builder, &module, func);
        }

        if let Err(e) = module.verify() {
            eprintln!("Module verification failed: {}", e.to_string());
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
    variables: Vec<HashMap<String, PointerValue<'ctx>>>,
}

impl<'a, 'ctx> Compiler<'a, 'ctx> {
    pub fn compile(
        context: &'ctx Context,
        builder: &'a Builder<'ctx>,
        module: &'a Module<'ctx>,
        func: &FunctionStatement<'_>,
    ) {
        let mut this = Self {
            context,
            builder,
            module,
            fn_value: None,
            variables: vec![HashMap::new()],
        };

        let range = FileRange::default();
        this.compile_prototype("schrijf", &[
            Parameter {
                name: Ranged::new(range, "s".into()),
                ty: Ranged::new(range, Type {
                    specifier: Ranged::new(range, TypeSpecifier::BuiltIn(BuiltinType::Slinger)),
                }),
            }
        ]);

        this.compile_function(func)
    }

    fn compile_statement(&mut self, function: &FunctionValue, block: &BasicBlock, statement: &Statement) {
        match &statement.kind {
            StatementKind::Function(..) => todo!(),
            StatementKind::Expression(expr) => { self.compile_expression(function, block, expr.value()); }
            StatementKind::For(stmt) => self.compile_statement_for(function, block, stmt),
            StatementKind::If(stmt) => self.compile_statement_if(function, block, stmt),
            StatementKind::Return(stmt) => self.compile_statement_return(function, block, stmt),
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
        let function = self.compile_prototype(&func.name, &func.parameters);

        self.fn_value = Some(function);

        let block = self.context.append_basic_block(function, "entry");
        self.builder.position_at_end(block);

        for statement in &func.body {
            self.compile_statement(&function, &block, statement)
        }
    }

    fn create_basic_metadata_type(&mut self, ty: &Type) -> BasicMetadataTypeEnum<'ctx> {
        match ty.specifier.value() {
           TypeSpecifier::BuiltIn(BuiltinType::Bool) => BasicMetadataTypeEnum::IntType(self.context.bool_type()),
           TypeSpecifier::BuiltIn(BuiltinType::G32) => BasicMetadataTypeEnum::IntType(self.context.i32_type()),
           TypeSpecifier::BuiltIn(BuiltinType::Null) => BasicMetadataTypeEnum::PointerType(self.context.opaque_struct_type("null").ptr_type(AddressSpace::default())),
           TypeSpecifier::BuiltIn(BuiltinType::Slinger) => BasicMetadataTypeEnum::PointerType(self.context.i8_type().ptr_type(AddressSpace::default())),
           TypeSpecifier::Custom { .. } => todo!(),
        }
    }

    fn compile_expression(&self, function: &FunctionValue, block: &BasicBlock, expr: &Expression) -> BasicValueEnum {
        match expr {
            Expression::Primary(PrimaryExpression::Boolean(b)) => {
                BasicValueEnum::IntValue(self.context.bool_type().const_int(*b as u64, false))
            }

            Expression::Primary(PrimaryExpression::IntegerLiteral(i)) => {
                BasicValueEnum::IntValue(self.context.i64_type().const_int(*i as u64, false))
            }

            Expression::Primary(PrimaryExpression::Parenthesized(expr)) => {
                self.compile_expression(function, block, expr.value())
            }

            Expression::Primary(PrimaryExpression::Reference(reference)) => {
                let name: &str = reference.value();
                let var_ptr = self.variables.iter().rev().flat_map(|x: &HashMap<String, PointerValue<'ctx>>| x.get(name))
                    .next()
                    .copied()
                    .expect("variable not stored yet, SemanticAnalysis what are you doing?");
                let pointee_ty = self.context.i8_type();
                self.builder.build_load(pointee_ty, var_ptr, "load variable").unwrap()
            }

            Expression::Primary(PrimaryExpression::StringLiteral(literal)) => {
                self.builder.build_global_string_ptr(&literal, "value").unwrap().as_pointer_value().into()
            }

            Expression::Primary(PrimaryExpression::TemplateString { parts }) => {
                _ = parts;
                todo!()
            }

            Expression::BiExpression(bi) => {
                let lhs = self.compile_expression(function, block, bi.lhs.value());
                let rhs = self.compile_expression(function, block, bi.rhs.value());

                let BasicValueEnum::IntValue(lhs) = lhs else {
                    panic!("Invalid LHS: {lhs:#?}");
                };

                let BasicValueEnum::IntValue(rhs) = rhs else {
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

                BasicValueEnum::IntValue(value)
            }

            Expression::Postfix(postfix) => {
                match &postfix.kind {
                    PostfixExpressionKind::Call(call) => {
                        let args: Vec<BasicMetadataValueEnum> = call.arguments.iter()
                            .map(|x| self.compile_expression(function, block, x.value()))
                            .map(|x| BasicMetadataValueEnum::from(x))
                            .collect();

                        let retval = match postfix.lhs.value() {
                            Expression::Primary(PrimaryExpression::Reference(reference)) => {
                                let func = self.module.get_function(reference.value()).expect(&format!("Function not found: {}", reference.value()));
                                self.builder.build_direct_call(func, &args, "callExpr").expect(&format!("Failed to call: {}", reference.value()))
                            }

                            other_expr => {
                                let lhs = self.compile_expression(function, block, other_expr).into_pointer_value();
                                let ty = self.context.i8_type().fn_type(&[], false); // TODO
                                self.builder.build_indirect_call(ty, lhs, &args, "callExpr").unwrap()
                            }
                        };

                        retval.try_as_basic_value().left_or_else(|a| {
                            _ = a; // println!("Unknown: {a:#?}");
                            BasicValueEnum::IntValue(self.context.i8_type().const_zero())
                        })
                    }

                    _ => todo!()
                }
            }
        }
    }

    fn compile_statement_for(&mut self, function: &FunctionValue, block: &BasicBlock, stmt: &ForStatement) {
        _ = function;
        _ = block;
        _ = stmt;
        todo!()
    }

    fn compile_statement_if(&mut self, function: &FunctionValue, block: &BasicBlock, stmt: &IfStatement) {
        let then_block = self.context.append_basic_block(*function, "then");
        let else_block = self.context.append_basic_block(*function, "else");
        let comparison = self.compile_expression(function, block, &stmt.condition);

        let BasicValueEnum::IntValue(comparison) = comparison else {
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
            self.builder.build_return(Some(&value)).unwrap();
        } else {
            self.builder.build_return(None).unwrap();
        }
    }

    fn compile_statement_variable(&mut self, function: &FunctionValue, block: &BasicBlock, stmt: &VariableStatement) {
        let storage = self.create_entry_block_alloca("variableStorage");
        let value = self.compile_expression(function, block, stmt.expression.value());
        self.builder.build_store(storage, value).unwrap();

        self.variables.last_mut().unwrap().insert(stmt.name.to_string(), storage);
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
}
