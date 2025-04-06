use crate::ast::*;
use crate::types::*;

use std::collections::HashMap;

type Stack<T> = Vec<T>;

#[derive(Debug, Clone)]
pub struct TypeChecker {
    type_enviroment: Stack<HashMap<String, Type>>,
}

impl TypeChecker {
    pub fn new() -> Self {
        Self {
            type_enviroment: Stack::new(),
        }
    }

    pub fn check(&mut self, ast: &AST) -> Result<(), String> {
        //We add the main enviroment
        self.type_enviroment.push(HashMap::new());

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
                //Check if the condition is an integer, error otherwise
                self.type_expression(condition)?;
                if self.type_expression(condition)? != Type::Variable(NumericType::Integer) {
                    return Err("Condition must be an integer".to_string());
                }

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
                        operator,
                        expected,
                        operands.len()
                ))
            } else {
                Ok(())
            }
        };

        let check_operand = |idx: usize, expected: NumericType| -> Result<(), String> {
            let op = operands.get(idx).ok_or_else(|| "Missing operand".to_string())?;
            if self.type_expression(op)? != Type::Variable(expected.clone()) {
                Err(format!(
                        "Operand at position {} ({:?}) does not match the required type {:?}.",
                        idx, op, expected
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
                        | Some(Operator::BitOr) | Some(Operator::BitXor) => {
                            check_len(2)?;
                            check_operand(0, NumericType::FiniteField)?;
                            check_operand(1, NumericType::FiniteField)?;
                        }
                    Some(Operator::EqualZero) | Some(Operator::ShiftLeft) | Some(Operator::ShiftRight)
                        | Some(Operator::BitNot) | Some(Operator::Wrap) | Some(Operator::Return) => {
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
                    Some(_) => {
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
                        | Some(Operator::BitNot) | Some(Operator::Wrap) | Some(Operator::Return) => {
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
                    Some(_) => {
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
                    Some(Operator::GetSignal) => {
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
                                return Err("Operand must be a variable for variable assignment.".to_string());
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

#[cfg(test)]
mod tests {
    use num_bigint_dig::BigInt;

    use super::*;

    fn create_checker() -> TypeChecker {
        let mut type_checker = TypeChecker::new();
        type_checker.type_enviroment.push(HashMap::new());
        type_checker
    }

    //Tests for finite field operations
    #[test]
    fn test_ff_add_operation() {
        let mut type_checker = create_checker();
        let node = ASTNode::Operation {
            num_type: Some(NumericType::FiniteField),
            operator: Some(Operator::Add),
            output: Some("result".to_string()),
            operands: vec![
                Expression::Atomic(Atomic::Constant(ConstantType::FF(BigInt::from(1)))),
                Expression::Atomic(Atomic::Constant(ConstantType::FF(BigInt::from(2)))),
            ],
        };
        assert!(type_checker.check_node(&node).is_ok());
    }

    #[test]
    fn test_ff_add_operation_success() {
        let mut type_checker = create_checker();
        let node = ASTNode::Operation {
            num_type: Some(NumericType::FiniteField),
            operator: Some(Operator::Add),
            output: Some("result".to_string()),
            operands: vec![
                Expression::Atomic(Atomic::Constant(ConstantType::FF(BigInt::from(1)))),
                Expression::Atomic(Atomic::Constant(ConstantType::FF(BigInt::from(2)))),
            ],
        };
        assert!(type_checker.check_node(&node).is_ok());
    }

    #[test]
    fn test_ff_add_operation_fail_wrong_operand_type() {
        let mut type_checker = create_checker();
        let node = ASTNode::Operation {
            num_type: Some(NumericType::FiniteField),
            operator: Some(Operator::Add),
            output: Some("result".to_string()),
            operands: vec![
                Expression::Atomic(Atomic::Constant(ConstantType::I64(1))),
                Expression::Atomic(Atomic::Constant(ConstantType::FF(BigInt::from(2)))),
            ],
        };
        assert!(type_checker.check_node(&node).is_err());
    }

    #[test]
    fn test_ff_equal_zero_operation_success() {
        let mut type_checker = create_checker();
        let node = ASTNode::Operation {
            num_type: Some(NumericType::FiniteField),
            operator: Some(Operator::EqualZero),
            output: Some("result".to_string()),
            operands: vec![
                Expression::Atomic(Atomic::Constant(ConstantType::FF(BigInt::from(0)))),
            ],
        };
        assert!(type_checker.check_node(&node).is_ok());
    }

    #[test]
    fn test_ff_equal_zero_operation_fail_wrong_operand_type() {
        let mut type_checker = create_checker();
        let node = ASTNode::Operation {
            num_type: Some(NumericType::FiniteField),
            operator: Some(Operator::EqualZero),
            output: Some("result".to_string()),
            operands: vec![
                Expression::Atomic(Atomic::Constant(ConstantType::I64(0))),
            ],
        };
        assert!(type_checker.check_node(&node).is_err());
    }

    #[test]
    fn test_ff_load_operation_success() {
        let mut type_checker = create_checker();
        let node = ASTNode::Operation {
            num_type: Some(NumericType::FiniteField),
            operator: Some(Operator::Load),
            output: Some("result".to_string()),
            operands: vec![
                Expression::Atomic(Atomic::Constant(ConstantType::I64(1))),
            ],
        };
        assert!(type_checker.check_node(&node).is_ok());
    }

    #[test]
    fn test_ff_load_operation_fail_wrong_operand_type() {
        let mut type_checker = create_checker();
        let node = ASTNode::Operation {
            num_type: Some(NumericType::FiniteField),
            operator: Some(Operator::Load),
            output: Some("result".to_string()),
            operands: vec![
                Expression::Atomic(Atomic::Constant(ConstantType::FF(BigInt::from(1)))),
            ],
        };
        assert!(type_checker.check_node(&node).is_err());
    }

    #[test]
    fn test_ff_store_operation_success() {
        let mut type_checker = create_checker();
        let node = ASTNode::Operation {
            num_type: Some(NumericType::FiniteField),
            operator: Some(Operator::Store),
            output: None,
            operands: vec![
                Expression::Atomic(Atomic::Constant(ConstantType::I64(1))),
                Expression::Atomic(Atomic::Constant(ConstantType::FF(BigInt::from(2)))),
            ],
        };
        assert!(type_checker.check_node(&node).is_ok());
    }

    #[test]
    fn test_ff_store_operation_fail_wrong_operand_type() {
        let mut type_checker = create_checker();
        let node = ASTNode::Operation {
            num_type: Some(NumericType::FiniteField),
            operator: Some(Operator::Store),
            output: None,
            operands: vec![
                Expression::Atomic(Atomic::Constant(ConstantType::FF(BigInt::from(1)))),
                Expression::Atomic(Atomic::Constant(ConstantType::FF(BigInt::from(2)))),
            ],
        };
        assert!(type_checker.check_node(&node).is_err());
    }

    #[test]
    fn test_ff_assignment_success() {
        let mut type_checker = create_checker();
        let node = ASTNode::Operation {
            num_type: Some(NumericType::FiniteField),
            operator: None,
            output: Some("x".to_string()),
            operands: vec![
                Expression::Atomic(Atomic::Constant(ConstantType::FF(BigInt::from(4)))),
            ],
        };
        assert!(type_checker.check_node(&node).is_ok());
    }

    #[test]
    fn test_ff_assignment_fail_wrong_operand_type() {
        let mut type_checker = create_checker();
        let node = ASTNode::Operation {
            num_type: Some(NumericType::FiniteField),
            operator: None,
            output: Some("x".to_string()),
            operands: vec![
                Expression::Atomic(Atomic::Constant(ConstantType::I64(4))),
            ],
        };
        assert!(type_checker.check_node(&node).is_err());
    }

    //Tests for integer operations
    #[test]
    fn test_integer_add_operation_success() {
        let mut type_checker = create_checker();
        let node = ASTNode::Operation {
            num_type: Some(NumericType::Integer),
            operator: Some(Operator::Add),
            output: Some("result".to_string()),
            operands: vec![
                Expression::Atomic(Atomic::Constant(ConstantType::I64(1))),
                Expression::Atomic(Atomic::Constant(ConstantType::I64(2))),
            ],
        };
        assert!(type_checker.check_node(&node).is_ok());
    }

    #[test]
    fn test_integer_add_operation_fail_wrong_operand_type() {
        let mut type_checker = create_checker();
        let node = ASTNode::Operation {
            num_type: Some(NumericType::Integer),
            operator: Some(Operator::Add),
            output: Some("result".to_string()),
            operands: vec![
                Expression::Atomic(Atomic::Constant(ConstantType::FF(BigInt::from(1)))),
                Expression::Atomic(Atomic::Constant(ConstantType::I64(2))),
            ],
        };
        assert!(type_checker.check_node(&node).is_err());
    }

    #[test]
    fn test_integer_equal_zero_operation_success() {
        let mut type_checker = create_checker();
        let node = ASTNode::Operation {
            num_type: Some(NumericType::Integer),
            operator: Some(Operator::EqualZero),
            output: Some("result".to_string()),
            operands: vec![
                Expression::Atomic(Atomic::Constant(ConstantType::I64(0))),
            ],
        };
        assert!(type_checker.check_node(&node).is_ok());
    }

    #[test]
    fn test_integer_equal_zero_operation_fail_wrong_operand_type() {
        let mut type_checker = create_checker();
        let node = ASTNode::Operation {
            num_type: Some(NumericType::Integer),
            operator: Some(Operator::EqualZero),
            output: Some("result".to_string()),
            operands: vec![
                Expression::Atomic(Atomic::Constant(ConstantType::FF(BigInt::from(0)))),
            ],
        };
        assert!(type_checker.check_node(&node).is_err());
    }

    #[test]
    fn test_integer_load_operation_success() {
        let mut type_checker = create_checker();
        let node = ASTNode::Operation {
            num_type: Some(NumericType::Integer),
            operator: Some(Operator::Load),
            output: Some("result".to_string()),
            operands: vec![
                Expression::Atomic(Atomic::Constant(ConstantType::I64(1))),
            ],
        };
        assert!(type_checker.check_node(&node).is_ok());
    }

    #[test]
    fn test_integer_load_operation_fail_wrong_operand_type() {
        let mut type_checker = create_checker();
        let node = ASTNode::Operation {
            num_type: Some(NumericType::Integer),
            operator: Some(Operator::Load),
            output: Some("result".to_string()),
            operands: vec![
                Expression::Atomic(Atomic::Constant(ConstantType::FF(BigInt::from(1)))),
            ],
        };
        assert!(type_checker.check_node(&node).is_err());
    }

    #[test]
    fn test_integer_store_operation_success() {
        let mut type_checker = create_checker();
        let node = ASTNode::Operation {
            num_type: Some(NumericType::Integer),
            operator: Some(Operator::Store),
            output: None,
            operands: vec![
                Expression::Atomic(Atomic::Constant(ConstantType::I64(1))),
                Expression::Atomic(Atomic::Constant(ConstantType::I64(2))),
            ],
        };
        assert!(type_checker.check_node(&node).is_ok());
    }

    #[test]
    fn test_integer_store_operation_fail_wrong_operand_type() {
        let mut type_checker = create_checker();
        let node = ASTNode::Operation {
            num_type: Some(NumericType::Integer),
            operator: Some(Operator::Store),
            output: None,
            operands: vec![
                Expression::Atomic(Atomic::Constant(ConstantType::FF(BigInt::from(1)))),
                Expression::Atomic(Atomic::Constant(ConstantType::I64(2))),
            ],
        };
        assert!(type_checker.check_node(&node).is_err());
    }

    #[test]
    fn test_integer_assignment_success() {
        let mut type_checker = create_checker();
        let node = ASTNode::Operation {
            num_type: Some(NumericType::Integer),
            operator: None,
            output: Some("x".to_string()),
            operands: vec![
                Expression::Atomic(Atomic::Constant(ConstantType::I64(4))),
            ],
        };
        assert!(type_checker.check_node(&node).is_ok());
    }

    #[test]
    fn test_integer_assignment_fail_wrong_operand_type() {
        let mut type_checker = create_checker();
        let node = ASTNode::Operation {
            num_type: Some(NumericType::Integer),
            operator: None,
            output: Some("x".to_string()),
            operands: vec![
                Expression::Atomic(Atomic::Constant(ConstantType::FF(BigInt::from(4)))),
            ],
        };
        assert!(type_checker.check_node(&node).is_err());
    }

    #[test]
    fn test_simple_if_success() {
        let mut type_checker = create_checker();
        let node = ASTNode::IfThenElse {
            condition: Expression::Atomic(Atomic::Constant(ConstantType::I64(1))),
            if_case: vec![ASTNode::Operation {
                num_type: Some(NumericType::FiniteField),
                operator: Some(Operator::Add),
                output: Some("result".to_string()),
                operands: vec![
                    Expression::Atomic(Atomic::Constant(ConstantType::FF(BigInt::from(1)))),
                    Expression::Atomic(Atomic::Constant(ConstantType::FF(BigInt::from(2)))),
                ],
            }],
            else_case: Some(vec![ASTNode::Operation {
                num_type: Some(NumericType::FiniteField),
                operator: Some(Operator::Sub),
                output: Some("result".to_string()),
                operands: vec![
                    Expression::Atomic(Atomic::Constant(ConstantType::FF(BigInt::from(5)))),
                    Expression::Atomic(Atomic::Constant(ConstantType::FF(BigInt::from(3)))),
                ],
            }]),
        };
        assert!(type_checker.check_node(&node).is_ok());
    }

    #[test]
    fn test_simple_if_fail_wrong_condition_type() {
        let mut type_checker = create_checker();
        let node = ASTNode::IfThenElse {
            condition: Expression::Atomic(Atomic::Constant(ConstantType::FF(BigInt::from(1)))),
            if_case: vec![ASTNode::Operation {
                num_type: Some(NumericType::FiniteField),
                operator: Some(Operator::Add),
                output: Some("result".to_string()),
                operands: vec![
                    Expression::Atomic(Atomic::Constant(ConstantType::FF(BigInt::from(1)))),
                    Expression::Atomic(Atomic::Constant(ConstantType::FF(BigInt::from(2)))),
                ],
            }],
            else_case: None,
        };
        assert!(type_checker.check_node(&node).is_err());
    }

    #[test]
    fn test_simple_for_success() {
        let mut type_checker = create_checker();
        let node = ASTNode::Loop {
            body: vec![ASTNode::Operation {
                num_type: Some(NumericType::Integer),
                operator: Some(Operator::Add),
                output: Some("counter".to_string()),
                operands: vec![
                    Expression::Atomic(Atomic::Variable("counter".to_string())),
                    Expression::Atomic(Atomic::Constant(ConstantType::I64(1))),
                ],
            }],
        };
        type_checker.type_enviroment.last_mut().unwrap().insert(
            "counter".to_string(),
            Type::Variable(NumericType::Integer),
        );
        assert!(type_checker.check_node(&node).is_ok());
    }

    #[test]
    fn test_simple_for_fail_missing_variable_in_env() {
        let mut type_checker = create_checker();
        let node = ASTNode::Loop {
            body: vec![ASTNode::Operation {
                num_type: Some(NumericType::Integer),
                operator: Some(Operator::Add),
                output: Some("counter".to_string()),
                operands: vec![
                    Expression::Atomic(Atomic::Variable("counter".to_string())),
                    Expression::Atomic(Atomic::Constant(ConstantType::I64(1))),
                ],
            }],
        };
        assert!(type_checker.check_node(&node).is_err());
    }
}
