use crate::ast::*;
use crate::types::*;

use std::collections::HashMap;

type Stack<T> = Vec<T>;

#[derive(Debug, Clone)]
pub struct TypeChecker {
    // Crate public to allow testing
    pub(crate) type_enviroment: Stack<HashMap<String, Type>>,
}

impl TypeChecker {
    pub fn new() -> Self {
        // Initialize the type environment with a new stack
        // and push an empty environment onto the stack
        // This will be the main environment where all global variables are stored
        let mut enviroment = Stack::new();
        enviroment.push(HashMap::new());
        Self {
            type_enviroment: enviroment,
        }
    }

    pub fn check(&mut self, ast: &AST) -> Result<(), String> {
        // Check all functions
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

        // Add the predefined registers
        // TODO: Move this to the creation of the function?
        function_env.insert("destination".to_string(), Type::Variable(NumericType::Integer));
        function_env.insert("destination_size".to_string(), Type::Variable(NumericType::Integer));

        self.type_enviroment.push(function_env);

        for node in &function.body {
            self.check_node(node)?;
        }

        self.type_enviroment.pop();

        Ok(())
    }

    // Crate public to allow testing
    pub(crate) fn check_node(&mut self, node: &ASTNode) -> Result<(), String> {
        match node {
            ASTNode::Operation { num_type, operator, output, operands } => {
                self.check_operation(num_type, operator, output, operands)
            },
            ASTNode::IfThenElse { condition, if_case, else_case } => {
                //Check if the condition is an integer, error otherwise
                // self.type_expression(condition)?;
                // if self.type_expression(condition)? != Type::Variable(NumericType::Integer) {
                //     return Err("Condition must be an integer".to_string());
                // }

                // Create a new environment for the if-case
                let if_case_env = HashMap::new();
                self.type_enviroment.push(if_case_env);
                for inner_node in if_case {
                    self.check_node(inner_node)?;
                }
                self.type_enviroment.pop();

                // Check the else-case if it exists
                // Create a new environment for the else-case
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
            ASTNode::Break | ASTNode::Continue => {
                Ok(())
            }
        }
    }

    fn check_operation(
        &mut self,
        num_type: &Option<NumericType>,
        operator: &Option<Operator>,
        output: &Option<String>,
        operands: &[Expression],
    ) -> Result<(), String> {
        let check_len = |expected: usize| -> Result<(), String> {
            if operands.len() != expected {
                Err(format!(
                        "Operator {:?} requires exactly {} operands, but {} were provided.",
                        operator, expected, operands.len()
                ))
            } else {
                Ok(())
            }
        };

        let check_operand = |idx: usize, expected: NumericType| -> Result<(), String> {
            let op = operands.get(idx).ok_or_else(|| "Missing operand".to_string())?;
            if self.type_expression(op)? != Type::Variable(expected.clone()) {
                Err(format!(
                        "Operand at position {} ({:?}) does not match the required type {:?} in operator {:?}.",
                        idx, op, expected, operator
                ))
            } else {
                Ok(())
            }
        };

        let mut var_type;
        match num_type {
            Some(NumericType::FiniteField) => {
                var_type = Type::Variable(NumericType::FiniteField);

                match operator {
                    Some(Operator::Add) | Some(Operator::Sub) | Some(Operator::Mul)
                        | Some(Operator::Div) | Some(Operator::IDiv) | Some(Operator::Rem) | Some(Operator::Pow)
                        | Some(Operator::Greater) | Some(Operator::GreaterEqual)
                        | Some(Operator::Less) | Some(Operator::LessEqual)
                        | Some(Operator::Equal) | Some(Operator::NotEqual)
                        | Some(Operator::And) | Some(Operator::Or) | Some(Operator::BitAnd)
                        | Some(Operator::BitOr) | Some(Operator::BitXor)
                        | Some(Operator::ShiftLeft) | Some(Operator::ShiftRight) 
                    => {
                        check_len(2)?;
                        check_operand(0, NumericType::FiniteField)?;
                        check_operand(1, NumericType::FiniteField)?;
                    }
                    Some(Operator::EqualZero) | Some(Operator::BitNot) | Some(Operator::Return) => {
                        check_len(1)?;
                        check_operand(0, NumericType::FiniteField)?;
                    }
                    Some(Operator::Load) => {
                        check_len(1)?;
                        check_operand(0, NumericType::Integer)?;
                    }
                    Some(Operator::Store) => {
                        check_len(2)?;
                        check_operand(0, NumericType::Integer)?;
                        check_operand(1, NumericType::FiniteField)?;
                    }
                    Some(Operator::Call) => {
                        self.check_call(operator, operands, NumericType::FiniteField)?;
                    }
                    Some(Operator::Wrap) => {
                        check_len(1)?;
                        check_operand(0, NumericType::FiniteField)?;
                        var_type = Type::Variable(NumericType::Integer);
                    }
                    Some(Operator::Extend) => {
                        return Err("Extend operator must have type Integer".to_string());
                    }
                    Some(Operator::GetSignal) | Some(Operator::GetCmpSignal) | Some(Operator::SetSignal)
                        | Some(Operator::SetCmpIn) | Some(Operator::SetCmpInCnt) | Some(Operator::SetCmpInRun)
                        | Some(Operator::SetCmpInCntCheck) | Some(Operator::GetTemplateId) | Some(Operator::GetTemplateSignalPosition)
                        | Some(Operator::GetTemplateSignalSize) | Some(Operator::Error) 
                    => {
                        return Err(format!(
                            "Operator {:?} must not have a type", operator
                        ));
                    }
                    None => {
                        if !matches!(operands.first(), Some(Expression::Atomic(Atomic::Constant(ConstantType::FF(_))))) {
                            return Err("Operand must be a constant number in finite field for finite field assignment.".to_string());
                        }
                    }
                }
            }
            Some(NumericType::Integer) => {
                var_type = Type::Variable(NumericType::Integer);

                match operator {
                    Some(Operator::Add) | Some(Operator::Sub) | Some(Operator::Mul)
                        | Some(Operator::Div) | Some(Operator::IDiv) | Some(Operator::Rem) | Some(Operator::Pow)
                        | Some(Operator::Greater) | Some(Operator::GreaterEqual)
                        | Some(Operator::Less) | Some(Operator::LessEqual)
                        | Some(Operator::Equal) | Some(Operator::NotEqual)
                        | Some(Operator::And) | Some(Operator::Or) | Some(Operator::BitAnd)
                        | Some(Operator::BitOr) | Some(Operator::BitXor) | Some(Operator::Store)=> {
                            check_len(2)?;
                            check_operand(0, NumericType::Integer)?;
                            check_operand(1, NumericType::Integer)?;
                        }
                    Some(Operator::EqualZero) | Some(Operator::ShiftLeft) | Some(Operator::ShiftRight)
                        | Some(Operator::BitNot) | Some(Operator::Return) => {
                            check_len(1)?;
                            check_operand(0, NumericType::Integer)?;
                        }
                    Some(Operator::Load) => {
                        check_len(1)?;
                        check_operand(0, NumericType::Integer)?;
                    }
                    Some(Operator::Call) => {
                        self.check_call(operator, operands, NumericType::Integer)?;
                    }
                    Some(Operator::Extend) => {
                        check_len(1)?;
                        check_operand(0, NumericType::Integer)?;
                        var_type = Type::Variable(NumericType::FiniteField);
                    }
                    Some(Operator::Wrap) => {
                        return Err("Wrap operator must have type Finite Field".to_string());
                    }
                    Some(Operator::GetSignal) | Some(Operator::GetCmpSignal) | Some(Operator::SetSignal)
                        | Some(Operator::SetCmpIn) | Some(Operator::SetCmpInCnt) | Some(Operator::SetCmpInRun)
                        | Some(Operator::SetCmpInCntCheck) | Some(Operator::GetTemplateId) | Some(Operator::GetTemplateSignalPosition)
                        | Some(Operator::GetTemplateSignalSize) | Some(Operator::Error)
                    => {
                        return Err(format!(
                            "Operator {:?} must not have a type", operator
                        ));
                    }
                    None => {
                        //Case x = i64.number
                        if operands.len() != 1 {
                            return Err("Assignment of an integer to a variable must only have one operand (the value).".to_string());
                        }
                        if !matches!(operands.first(), Some(Expression::Atomic(Atomic::Constant(ConstantType::I64(_))))) {
                            return Err("Operand must be a constant integer for integer assignment.".to_string());
                        }
                    }
                }
            }
            None => {
                var_type = Type::Variable(NumericType::FiniteField);
                
                match operator {
                    Some(Operator::GetSignal) | Some(Operator::Error) => {
                        check_len(1)?;
                        check_operand(0, NumericType::Integer)?;
                    }
                    Some(Operator::GetCmpSignal) => {
                        check_len(2)?;
                        check_operand(0, NumericType::Integer)?;
                        check_operand(1, NumericType::Integer)?;
                    }
                    Some(Operator::GetTemplateId) => {
                        check_len(1)?;
                        check_operand(0, NumericType::Integer)?;
                        var_type = Type::Variable(NumericType::Integer);
                    }
                    Some(Operator::SetSignal) => {
                        check_len(2)?;
                        check_operand(0, NumericType::Integer)?;
                        check_operand(1, NumericType::FiniteField)?;
                    }
                    Some(Operator::SetCmpIn) | Some(Operator::SetCmpInCnt) | Some(Operator::SetCmpInRun) | Some(Operator::SetCmpInCntCheck) => {
                        check_len(3)?;
                        check_operand(0, NumericType::Integer)?;
                        check_operand(1, NumericType::Integer)?;
                        check_operand(2, NumericType::FiniteField)?;
                    }
                    Some(Operator::GetTemplateSignalSize) | Some(Operator::GetTemplateSignalPosition) => {
                        check_len(2)?;
                        check_operand(0, NumericType::Integer)?;
                        check_operand(1, NumericType::Integer)?;
                        var_type = Type::Variable(NumericType::Integer);
                    }
                    Some(Operator::Return) => {
                        check_len(2)?;
                        check_operand(0, NumericType::Integer)?;
                        check_operand(1, NumericType::Integer)?;
                    }
                    Some(_) => {
                        return Err(format!(
                                "Operator {:?} requires a numeric type, but none was provided.",
                                operator
                        ));
                    },
                    None => {
                        //Case x = y
                        if operands.len() != 1 {
                            return Err("Assignment of a variable to another variable must only have one operand (the value).".to_string());
                        }
                        if let Some(Expression::Atomic(Atomic::Variable(variable))) = operands.first() {
                            let input_type = self.type_enviroment.iter().rev()
                                .find_map(|env| env.get(variable));
                            var_type = input_type
                                .ok_or(format!("Variable {} not found in environment", variable))?
                                .clone();
                            } else {
                                return Err(format!(
                                    "Operand {:?} must be a variable for variable assignment.",
                                    operands.first()
                                ));
                        }
                    }
                }
            }
        }

        if let Some(o) = output {
            if let Some(env) = self.type_enviroment.last_mut() {
                env.insert(o.clone(), var_type);
            }
            else {
                return Err("No enviroment found".to_string());
            }
        }
        Ok(())
    }

    fn check_call(&mut self, operator: &Option<Operator>, operands: &[Expression], expected_type: NumericType) -> Result<(), String> {
        if !operands.is_empty() {
            return Err(format!(
                    "Operator {:?} requires at least a function to call, but none was provided.",
                    operator,
            ));
        }
        if let Some(Expression::Atomic(Atomic::Variable(name))) = operands.first() {
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
                if let Some(output_type) = output_types.first() {
                    if *output_type != expected_type {
                        return Err(format!(
                                "Function {} must output a {:?}, but it does not.",
                                name, expected_type
                        ));
                    }
                } else {
                    return Err(format!(
                            "Function {} must output a {:?}, but it does not.",
                            name, expected_type
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
        };
        Ok(())
    }
    
    fn type_expression(&self, expression: &Expression) -> Result<Type, String> {
        match expression {
            Expression::Atomic(Atomic::Constant(constant)) => {
                // Check the type of the constant expression
                match constant {
                    ConstantType::FF(_) => Ok(Type::Variable(NumericType::FiniteField)),
                    ConstantType::I64(_) => Ok(Type::Variable(NumericType::Integer)),
                }
            }
            Expression::Atomic(Atomic::Variable(variable)) => {
                // Check if the variable is in the environment
                if let Some(ty) = self.type_enviroment.iter().rev().find_map(|env| env.get(variable)) {
                    Ok(ty.clone())
                } else {
                    Err(format!("Variable {} not found in environment", variable))
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
