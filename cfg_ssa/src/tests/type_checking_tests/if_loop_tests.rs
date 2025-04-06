#[cfg(test)]
mod if_loop_tests {
    use crate::type_checking::TypeChecker;
    use crate::types::{ConstantType, NumericType, Operator};
    use num_bigint_dig::BigInt;
    use crate::ast::ASTNode;
    use crate::types::*;

    #[test]
    fn test_simple_if_success() {
        let mut type_checker = TypeChecker::new();
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

    // #[test]  TODO: Uncomment when the type of the condition is fixed
    // fn test_simple_if_fail_wrong_condition_type() {
    //     let mut type_checker = TypeChecker::new();
    //     let node = ASTNode::IfThenElse {
    //         condition: Expression::Atomic(Atomic::Constant(ConstantType::FF(BigInt::from(1)))),
    //         if_case: vec![ASTNode::Operation {
    //             num_type: Some(NumericType::FiniteField),
    //             operator: Some(Operator::Add),
    //             output: Some("result".to_string()),
    //             operands: vec![
    //                 Expression::Atomic(Atomic::Constant(ConstantType::FF(BigInt::from(1)))),
    //                 Expression::Atomic(Atomic::Constant(ConstantType::FF(BigInt::from(2)))),
    //             ],
    //         }],
    //         else_case: None,
    //     };
    //     assert!(type_checker.check_node(&node).is_err());
    // }

    #[test]
    fn test_simple_for_success() {
        let mut type_checker = TypeChecker::new();
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
        let mut type_checker = TypeChecker::new();
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