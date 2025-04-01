use crate::ast::*;
use crate::types::*;
use crate::Statement;
use std::collections::HashMap;
use std::f32::consts::E;
use std::fmt::format;
use std::process::Output;

type Stack<T> = Vec<T>;

#[derive(Debug, Clone)]
pub struct TypeChecker {
    pub type_enviroment: Stack<HashMap<String, Type>>,
    pub ast: AST,
}
impl TypeChecker {
    pub fn new(ast: AST) -> Self {
        Self {
            type_enviroment: Stack::new(),
            ast
        }
    }

    pub fn check(&mut self, ast: &AST) -> Result<(), String> {
        //We add the main enviroment
        self.type_enviroment.push(HashMap::new());


        for function in &ast.functions {
            self.type_enviroment.last_mut()
                .ok_or("No main enviroment created")?
                .insert(function.name.clone(), Type::Function(function.get_input_types(), function.get_output_types()));
            self.check_function(function)?;
        }

        // Check all templates
        for template in &ast.templates {
            self.check_template(template)?;
        }

        Ok(())
    }

    fn check_template(&mut self, template: &Template) -> Result<(), String> {
        // Create a new environment for the template
        let template_env = HashMap::new();

        // Push the new environment onto the stack
        self.type_enviroment.push(template_env);

        // Check the body of the template
        for node in &template.body {
            self.check_node(node)?;
        }

        // Pop the environment after checking the template
        self.type_enviroment.pop();

        Ok(())
    }

    fn check_function(&mut self, function: &Function) -> Result<(), String> {
        let mut function_env = HashMap::new();

        for (input, input_type) in function.inputs.iter().zip(function.get_input_types()) {
            function_env.insert(input.clone(), Type::Variable(input_type));
        }

        for (output, output_type) in function.outputs.iter().zip(function.get_output_types()) {
            function_env.insert(output.clone(), Type::Variable(output_type));
        }

        self.type_enviroment.push(function_env);

        for node in &function.body {
            self.check_node(node)?;
        }

        self.type_enviroment.pop();

        Ok(())
    }

    fn check_node(&mut self, node: &ASTNode) -> Result<(), String> {
        match node {
            ASTNode::Operation { num_type, operator, output, operands } => {
                self.check_operation(num_type, operator, output, operands)
            },
            ASTNode::IfThenElse { condition, if_case, else_case } => {
                self.type_expression(condition)?;
                //TODO: Check if a condition has a certain type

                let if_case_env = HashMap::new();
                self.type_enviroment.push(if_case_env);
                for inner_node in if_case {
                    self.check_node(inner_node)?;
                }
                self.type_enviroment.pop();

                if let Some(else_case) = else_case {
                    let else_case_env = HashMap::new();
                    self.type_enviroment.push(else_case_env);
                    for inner_node in else_case {
                        self.check_node(inner_node)?;
                    }
                    self.type_enviroment.pop();
                }

                Ok(())
            }
            ASTNode::Loop { body } => {
                let loop_env = HashMap::new();
                self.type_enviroment.push(loop_env);
                for inner_node in body {
                    self.check_node(inner_node)?;
                }
                self.type_enviroment.pop();
                Ok(())
            }
            _ => {
                Ok(())
            }
        }
    }

    fn check_operation(
        &mut self,
        num_type: &Option<NumericType>,
        operator: &Option<Operator>,
        output: &Option<String>,
        operands: &Vec<Expression>,
    ) -> Result<(), String> {
        if let Some(NumericType::FiniteField) = num_type {
            match operator {
                Some(Operator::Add) | Some(Operator::Sub) | Some(Operator::Mul)
                | Some(Operator::Div) | Some(Operator::IDiv) | Some(Operator::Rem) | Some(Operator::Pow)
                | Some(Operator::Greater) | Some(Operator::GreaterEqual)
                | Some(Operator::Less) | Some(Operator::LessEqual)
                | Some(Operator::Equal) | Some(Operator::NotEqual)
                | Some(Operator::And) | Some(Operator::Or) | Some(Operator::BitAnd)
                | Some(Operator::BitOr) | Some(Operator::BitXor) => {
                    if operands.len() != 2 {
                        return Err(format!(
                            "Operator {:?} requires exactly 2 operands, but {} were provided.",
                            operator,
                            operands.len()
                        ));
                    }
                    if let (Some(f), Some(s)) = (operands.get(0), operands.get(1)) {  
                        if self.type_expression(f)? != Type::Variable(NumericType::FiniteField) {
                            return Err(format!(
                                "First operand {:?} does not match the required type FiniteField.",
                                f
                            ));
                        }
                        if self.type_expression(s)? != Type::Variable(NumericType::FiniteField) {
                            return Err(format!(
                                "Second operand {:?} does not match the required type FiniteField.",
                                s
                            ));
                        } 
                    }
                },
                Some(Operator::EqualZero) | Some(Operator::ShiftLeft) | Some(Operator::ShiftRight)
                | Some(Operator::BitNot) | Some(Operator::Wrap) | Some(Operator::Return) => {
                    if operands.len() != 1 {
                        return Err(format!(
                            "Operator {:?} requires exactly 1 operand, but {} were provided.",
                            operator,
                            operands.len()
                        ));
                    }
                    if let Some(op) = operands.get(0) {
                        if self.type_expression(op)? != Type::Variable(NumericType::FiniteField) {
                            return Err(format!(
                                "Operand {:?} does not match the required type FiniteField.",
                                op
                            ));
                        }
                    }
                },
                Some(Operator::Load) => {
                    if operands.len() != 1 {
                        return Err(format!(
                            "Operator {:?} requires exactly 1 operand, but {} were provided.",
                            operator,
                            operands.len()
                        ));
                    }
                    if let Some(op) = operands.get(0) {
                        if self.type_expression(op)? != Type::Variable(NumericType::Integer) {
                            return Err(format!(
                                "Operand {:?} does not match the required type Integer.",
                                op
                            ));
                        }
                    }
                },
                Some(Operator::Store) => {
                    if operands.len() != 2 {
                        return Err(format!(
                            "Operator {:?} requires exactly 2 operands, but {} were provided.",
                            operator,
                            operands.len()
                        ));
                    }
                    if let (Some(f), Some(s)) = (operands.get(0), operands.get(1)) {  
                        if self.type_expression(f)? != Type::Variable(NumericType::Integer) {
                            return Err(format!(
                                "First operand {:?} does not match the required type Integer.",
                                f
                            ));
                        }
                        if self.type_expression(s)? != Type::Variable(NumericType::FiniteField) {
                            return Err(format!(
                                "Second operand {:?} does not match the required type FiniteField.",
                                s
                            ));
                        } 
                    }
                },
                Some(Operator::Call) => {
                    if operands.len() >= 1 {
                        return Err(format!(
                            "Operator {:?} requires at least a function to call, but none was provided.",
                            operator,
                        ));
                    }
                    if let Some(Expression::Atomic(Atomic::Variable(name))) = operands.get(0) {
                        let name = &name[1..];
                        let function_type = self.type_enviroment.iter().rev()
                            .find_map(|env| env.get(name))
                            .ok_or(format!("Function {} not found in environment", name))?;
                        if let Type::Function(input_types, output_types) = function_type {
                            if input_types.len() != operands.len() - 1 {
                                return Err(format!(
                                    "Function {} requires {} inputs, but {} were provided.",
                                    name,
                                    input_types.len(),
                                    operands.len() - 1
                                ));
                            }
                            for (operand, param) in operands.iter().skip(1).zip(input_types) {
                                if self.type_expression(operand)? != Type::Variable(param.clone()) {
                                    return Err(format!(
                                        "Operand {:?} does not match the required type {:?}.",
                                        operand,
                                        param
                                    ));
                                }
                            }
                            if output_types.len() != 1 {
                                //TODO: Implement
                                return Err(format!(
                                    "Functions that output more than one thing are not implemented: {}.",
                                    name,
                                ));
                            }
                            if let Some(output_type) = output_types.get(0) {
                                if *output_type != NumericType::FiniteField {
                                    return Err(format!(
                                        "Function {} must output a FiniteField, but it does not.",
                                        name,
                                    ));
                                }
                            } else {
                                return Err(format!(
                                    "Function {} must output a FiniteField, but it does not.",
                                    name,
                                ));
                            }
                        } else {
                            return Err(format!("{} is not a function", name));
                        }
                    }
                    else {
                        return Err(format!(
                            "Operator {:?} requires a function to call, but none was provided.",
                            operator,
                        ));
                    }
                },
                Some(_) => {
                    return Err(format!(
                        "Operator {:?} must not have a type", operator
                    ));
                },
                None => {
                    //This case is for a variable that is equal to another directly (or to a constant)
                }
            }
        }
        else if let Some(NumericType::Integer) = num_type {
            match operator {
                Some(_) => {
                    return Err(format!(
                        "Operator {:?} must not have a type", operator
                    ));
                },
                None => {

                }
            }
        }
        else {
            match operator {
                Some(Operator::GetSignal) => {
                    if operands.len() != 1 {
                        return Err(format!(
                            "Operator {:?} with GetSignal requires exactly 1 operand, but {} were provided.",
                            operator,
                            operands.len()
                        ));
                    }
                    if let Some(op) = operands.first() {
                        //TODO: Change type objective
                        if self.type_expression(op)? != Type::Variable(NumericType::Integer) {
                            return Err(format!(
                                "Operand {:?} does not match the required type NumericType::Integer.",
                                op
                            ));
                        }
                    }
                },
                Some(_) => {
                    return Err(format!(
                        "Operator {:?} requires a numeric type, but none was provided.",
                        operator
                    ));
                },
                None => {
                    
                }
            }
        }
        Ok(())
    }

    fn type_expression(&self, expression: &Expression) -> Result<Type, String> {
        match expression {
            Expression::Atomic(atomic) => {
                // Check the type of the atomic expression
                match atomic {
                    Atomic::Constant(constant) => {
                        // Check the type of the constant expression
                        match constant {
                            ConstantType::FF(_) => Ok(Type::Variable(NumericType::FiniteField)),
                            ConstantType::I64(_) => Ok(Type::Variable(NumericType::Integer)),
                        }
                    }
                    Atomic::Variable(variable) => {
                        // Check if the variable is in the environment
                        if let Some(ty) = self.type_enviroment.iter().rev().find_map(|env| env.get(variable)) {
                            Ok(ty.clone())
                        } else {
                            Err(format!("Variable {} not found in environment", variable))
                        }
                    }
                }
            }
            Expression::Parameter(parameter) => {
                match parameter {
                    Parameter::I64Memory { index: _, size: _ } => {
                        Ok(Type::Variable(NumericType::Integer))
                    },
                    _ => {
                        Ok(Type::Variable(NumericType::FiniteField))
                    }
                }
            }
        }
    }
}