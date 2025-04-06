#[cfg(test)]
mod finite_field_tests {
    use crate::type_checking::TypeChecker;
    use crate::types::{ConstantType, NumericType, Operator};
    use num_bigint_dig::BigInt;
    use crate::ast::ASTNode;
    use crate::types::*;

    //Tests for finite field operations
    #[test]
    fn test_ff_add_operation() {
        let mut type_checker = TypeChecker::new();
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
        let mut type_checker = TypeChecker::new();
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
        let mut type_checker = TypeChecker::new();
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
        let mut type_checker = TypeChecker::new();
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
        let mut type_checker = TypeChecker::new();
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
        let mut type_checker = TypeChecker::new();
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
        let mut type_checker = TypeChecker::new();
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
        let mut type_checker = TypeChecker::new();
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
        let mut type_checker = TypeChecker::new();
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
        let mut type_checker = TypeChecker::new();
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
        let mut type_checker = TypeChecker::new();
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
}
