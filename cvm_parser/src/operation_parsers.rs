use cfg_ssa::{
    ast::ASTNode,
    types::{Atomic, ConstantType, Expression, NumericType, Operator, Parameter},
};
use num_bigint::BigInt;
use crate::parse_variable_name;

use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::{digit1, i64, space0},
    combinator::{map, opt, value},
    multi::separated_list0,
    sequence::{delimited, separated_pair},
    IResult, Parser,
};

fn parse_numeric_type(input: &str) -> IResult<&str, NumericType> {
    alt((value(NumericType::Integer, tag("i64")), value(NumericType::FiniteField, tag("ff"))))
        .parse(input)
}

fn parse_operator(input: &str) -> IResult<&str, Operator> {
    alt([
        value(Operator::SetCmpInCntCheck, tag("set_cmp_input_cnt_check")),
        value(Operator::SetCmpInCnt, tag("set_cmp_input_cnt")),
        value(Operator::SetCmpInRun, tag("set_cmp_input_run")),
        value(Operator::SetCmpIn, tag("set_cmp_input")),
        value(Operator::GetCmpSignal, tag("get_cmp_signal")),
        value(Operator::GetSignal, tag("get_signal")),
        value(Operator::SetSignal, tag("set_signal")),

        value(Operator::Extend, tag("extend_ff")),
        value(Operator::Wrap, tag("wrap_i64")),
        value(Operator::Load, tag("load")),
        value(Operator::Store, tag("store")),
        value(Operator::Return, tag("return")),
        value(Operator::Call, tag("call")),
        value(Operator::Error, tag("error")),


        value(Operator::BitAnd, tag("band")),
        value(Operator::BitOr, tag("bor")),
        value(Operator::BitXor, tag("bxor")),
        value(Operator::BitNot, tag("bnot")),

        value(Operator::Add, tag("add")),
        value(Operator::Sub, tag("sub")),
        value(Operator::Mul, tag("mul")),
        value(Operator::Div, tag("div")),
        value(Operator::Rem, tag("rem")),
        value(Operator::IDiv, tag("idiv")),
        value(Operator::Pow, tag("pow")),
        value(Operator::EqualZero, tag("eqz")),
        value(Operator::NotEqual, tag("neq")),
        value(Operator::And, tag("and")),
        value(Operator::ShiftRight, tag("shr")),
        value(Operator::ShiftLeft, tag("shl")),

        value(Operator::Greater, tag("gt")),
        value(Operator::GreaterEqual, tag("ge")),
        value(Operator::Less, tag("lt")),
        value(Operator::LessEqual, tag("le")),
        value(Operator::Equal, tag("eq")),
        value(Operator::Or, tag("or")),
    ])
    .parse(input)
}

fn parse_constant(input: &str) -> IResult<&str, ConstantType> {
    let (input, num_type) = parse_numeric_type(input)?;
    let (input, _) = tag(".").parse(input)?;
    match num_type {
        NumericType::Integer => {
            map(i64, ConstantType::I64).parse(input)
        },
        NumericType::FiniteField => {
            map(digit1, |prime: &str| {
                ConstantType::FF(BigInt::parse_bytes(prime.as_bytes(), 10).unwrap())
            })
            .parse(input)
        },
    }
}

fn parse_atomic(input: &str) -> IResult<&str, Atomic> {
    alt((map(parse_constant, Atomic::Constant), map(parse_variable_name, Atomic::Variable))).parse(input)
}

//TODO: Change this to parse an variable name or a i64 (no ff)
fn parse_two_atomics(input: &str) -> IResult<&str, (Atomic, Atomic)> {
    delimited(
        tag("("),
        separated_pair(parse_atomic, delimited(space0, tag(","), space0), parse_atomic),
        tag(")"),
    )
    .parse(input)
}

fn parse_signal(input: &str) -> IResult<&str, Parameter> {
    map((tag("signal"), parse_two_atomics), |(_, (index, size))| Parameter::Signal { index, size })
        .parse(input)
}

// Parses: subcmpsignal(component, index, size)
fn parse_subcmp_signal(input: &str) -> IResult<&str, Parameter> {
    map(
        (
            tag("subcmpsignal"),
            delimited(
                tag("("),
                (
                    parse_atomic,
                    delimited(space0, tag(","), space0),
                    parse_atomic,
                    delimited(space0, tag(","), space0),
                    parse_atomic,
                ),
                tag(")"),
            ),
        ),
        |(_, (component, _, index, _, size))| Parameter::SubcmpSignal { component, index, size },
    )
    .parse(input)
}

// Parses: i64.memory(index, size)
fn parse_i64_memory(input: &str) -> IResult<&str, Parameter> {
    map((tag("i64.memory"), parse_two_atomics), |(_, (index, size))| Parameter::I64Memory {
        index,
        size,
    })
    .parse(input)
}

// Parses: ff.memory(index, size)
fn parse_ff_memory(input: &str) -> IResult<&str, Parameter> {
    map((tag("ff.memory"), parse_two_atomics), |(_, (index, size))| Parameter::FfMemory {
        index,
        size,
    })
    .parse(input)
}

fn parse_parameter(input: &str) -> IResult<&str, Parameter> {
    alt((parse_signal, parse_subcmp_signal, parse_i64_memory, parse_ff_memory)).parse(input)
}

pub fn parse_expression(input: &str) -> IResult<&str, Expression> {
    alt((map(parse_parameter, Expression::Parameter), map(parse_atomic, Expression::Atomic)))
        .parse(input)
}

fn parse_operation_no_output(input: &str) -> IResult<&str, ASTNode> {
    let (input, num_type) = opt(parse_numeric_type).parse(input)?;
    let (input, _) = if num_type.is_some() {
        let (input, _) = tag(".")(input)?;
        (input, ())
    } else {
        (input, ())
    };

    let (input, operator) = parse_operator(input)?;
    let (input, _) = space0(input)?;
    let (input, operands) = separated_list0(space0, parse_expression).parse(input)?;

    Ok((
        input,
        ASTNode::Operation { num_type, operator: Some(operator), output: None, operands },
    ))
}

fn parse_operation_with_output(input: &str) -> IResult<&str, ASTNode> {
    let (input, output) = parse_variable_name(input)?;
    let (input, _) = space0(input)?;
    let (input, _) = tag("=")(input)?;
    let (input, _) = space0(input)?;

    let (input, num_type) = opt(parse_numeric_type).parse(input)?;
    let (input, _) = if num_type.is_some() {
        let (input, _) = tag(".")(input)?;
        (input, ())
    } else {
        (input, ())
    };

    let (input, operator) = parse_operator(input)?;
    let (input, _) = space0(input)?;
    let (input, operands) = separated_list0(space0, parse_expression).parse(input)?;

    Ok((
        input,
        ASTNode::Operation {
            num_type,
            operator: Some(operator),
            output: Some(output),
            operands,
        },
    ))
}

fn parse_operation_two_variables(input: &str) -> IResult<&str, ASTNode> {
    let (input, output) = parse_variable_name(input)?;
    let (input, _) = space0(input)?;
    let (input, _) = tag("=")(input)?;
    let (input, _) = space0(input)?;

    let (input, op) = parse_atomic(input)?;

    Ok((
        input,
        ASTNode::Operation {
            num_type: None,
            operator: None,
            output: Some(output),
            operands: vec![Expression::Atomic(op)],
        },
    ))
}

pub fn parse_operation(input: &str) -> IResult<&str, ASTNode> {
    alt((parse_operation_no_output, parse_operation_with_output, parse_operation_two_variables)).parse(input)
}


#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_numeric_type() {
        assert_eq!(parse_numeric_type("i64"), Ok(("", NumericType::Integer)));
        assert_eq!(parse_numeric_type("ff"), Ok(("", NumericType::FiniteField)));
        assert!(parse_numeric_type("unknown").is_err());
    }

    #[test]
    fn test_parse_operator() {
        assert_eq!(parse_operator("add"), Ok(("", Operator::Add)));
        assert_eq!(parse_operator("sub"), Ok(("", Operator::Sub)));
        assert_eq!(parse_operator("mul"), Ok(("", Operator::Mul)));
        assert_eq!(parse_operator("div"), Ok(("", Operator::Div)));
        assert_eq!(parse_operator("rem"), Ok(("", Operator::Rem)));
        assert_eq!(parse_operator("idiv"), Ok(("", Operator::IDiv)));
        assert_eq!(parse_operator("pow"), Ok(("", Operator::Pow)));

        assert_eq!(parse_operator("gt"), Ok(("", Operator::Greater)));
        assert_eq!(parse_operator("ge"), Ok(("", Operator::GreaterEqual)));
        assert_eq!(parse_operator("lt"), Ok(("", Operator::Less)));
        assert_eq!(parse_operator("le"), Ok(("", Operator::LessEqual)));
        assert_eq!(parse_operator("eq"), Ok(("", Operator::Equal)));
        assert_eq!(parse_operator("neq"), Ok(("", Operator::NotEqual)));
        assert_eq!(parse_operator("eqz"), Ok(("", Operator::EqualZero)));

        assert_eq!(parse_operator("and"), Ok(("", Operator::And)));
        assert_eq!(parse_operator("or"), Ok(("", Operator::Or)));

        assert_eq!(parse_operator("shr"), Ok(("", Operator::ShiftRight)));
        assert_eq!(parse_operator("shl"), Ok(("", Operator::ShiftLeft)));
        assert_eq!(parse_operator("band"), Ok(("", Operator::BitAnd)));
        assert_eq!(parse_operator("bor"), Ok(("", Operator::BitOr)));
        assert_eq!(parse_operator("bxor"), Ok(("", Operator::BitXor)));
        assert_eq!(parse_operator("bnot"), Ok(("", Operator::BitNot)));

        assert_eq!(parse_operator("extend_ff"), Ok(("", Operator::Extend)));
        assert_eq!(parse_operator("wrap_i64"), Ok(("", Operator::Wrap)));

        assert_eq!(parse_operator("load", ), Ok(("", Operator::Load)));
        assert_eq!(parse_operator("store", ), Ok(("", Operator::Store)));

        assert_eq!(parse_operator("get_signal"), Ok(("", Operator::GetSignal)));
        assert_eq!(parse_operator("get_cmp_signal"), Ok(("", Operator::GetCmpSignal)));
        assert_eq!(parse_operator("set_signal"), Ok(("", Operator::SetSignal)));
        assert_eq!(parse_operator("set_cmp_input"), Ok(("", Operator::SetCmpIn)));
        assert_eq!(parse_operator("set_cmp_input_cnt"), Ok(("", Operator::SetCmpInCnt)));
        assert_eq!(parse_operator("set_cmp_input_run"), Ok(("", Operator::SetCmpInRun)));
        assert_eq!(parse_operator("set_cmp_input_cnt_check"), Ok(("", Operator::SetCmpInCntCheck)));

        assert_eq!(parse_operator("return"), Ok(("", Operator::Return)));
        assert_eq!(parse_operator("call"), Ok(("", Operator::Call)));
        assert_eq!(parse_operator("error"), Ok(("", Operator::Error)));

        assert!(parse_operator("unknown").is_err());
    }

    #[test]
    fn test_parse_atomic() {
        assert_eq!(parse_atomic("i64.42"), Ok(("", Atomic::Constant(ConstantType::I64(42)))));
        assert_eq!(parse_atomic("ff.3"), Ok(("", Atomic::Constant(ConstantType::FF(BigInt::from(3))))));
        assert_eq!(parse_atomic("ff.12345678901234567890"), Ok(("", Atomic::Constant(ConstantType::FF(BigInt::from(12345678901234567890u64))))));
        assert_eq!(parse_atomic("variable_name"), Ok(("", Atomic::Variable("variable_name".to_string()))));
    }

    #[test]
    fn test_parse_two_atomics() {
        assert_eq!(
            parse_two_atomics("(i64.42, i64.64)"),
            Ok(("", (Atomic::Constant(ConstantType::I64(42)), Atomic::Constant(ConstantType::I64(64)))))
        );
        assert_eq!(
            parse_two_atomics("(adfaf, jkjk)"),
            Ok(("", (Atomic::Variable("adfaf".to_string()), Atomic::Variable("jkjk".to_string()))))
        );
        assert_eq!(
            parse_two_atomics("(adfaf, i64.3)"),
            Ok(("", (Atomic::Variable("adfaf".to_string()), Atomic::Constant(ConstantType::I64(3)))))
        );
        assert!(parse_two_atomics("(i64.42, )").is_err());
    }

    #[test]
    fn test_parse_signal() {
        assert_eq!(
            parse_signal("signal(i64.42, i64.64)"),
            Ok(("", Parameter::Signal { index: Atomic::Constant(ConstantType::I64(42)), size: Atomic::Constant(ConstantType::I64(64)) }))
        );
    }

    #[test]
    fn test_parse_subcmp_signal() {
        assert_eq!(
            parse_subcmp_signal("subcmpsignal(component, i64.42, i64.64)"),
            Ok((
                "",
                Parameter::SubcmpSignal {
                    component: Atomic::Variable("component".to_string()),
                    index: Atomic::Constant(ConstantType::I64(42)),
                    size: Atomic::Constant(ConstantType::I64(64)),
                }
            ))
        );
    }

    #[test]
    fn test_parse_i64_memory() {
        assert_eq!(
            parse_i64_memory("i64.memory(i64.42, i64.64)"),
            Ok(("", Parameter::I64Memory { index: Atomic::Constant(ConstantType::I64(42)), size: Atomic::Constant(ConstantType::I64(64)) }))
        );
    }

    #[test]
    fn test_parse_ff_memory() {
        assert_eq!(
            parse_ff_memory("ff.memory(i64.42, i64.64)"),
            Ok(("", Parameter::FfMemory { index: Atomic::Constant(ConstantType::I64(42)), size: Atomic::Constant(ConstantType::I64(64)) }))
        );
    }

    #[test]
    fn test_parse_parameter() {
        assert_eq!(
            parse_parameter("signal(i64.42, i64.64)"),
            Ok(("", Parameter::Signal { index: Atomic::Constant(ConstantType::I64(42)), size: Atomic::Constant(ConstantType::I64(64))}))
        );
        assert_eq!(
            parse_parameter("i64.memory(i64.42, i64.64)"),
            Ok(("", Parameter::I64Memory { index: Atomic::Constant(ConstantType::I64(42)), size: Atomic::Constant(ConstantType::I64(64))}))
        );
        assert_eq!(
            parse_parameter("subcmpsignal(component, i64.42, i64.64)"),
            Ok((
                "",
                Parameter::SubcmpSignal {
                    component: Atomic::Variable("component".to_string()),
                    index: Atomic::Constant(ConstantType::I64(42)),
                    size: Atomic::Constant(ConstantType::I64(64))
                }
            ))
        );
        assert_eq!(
            parse_ff_memory("ff.memory(i64.42, i64.64)"),
            Ok(("", Parameter::FfMemory { index: Atomic::Constant(ConstantType::I64(42)), size: Atomic::Constant(ConstantType::I64(64))}))
        );
    }

    #[test]
    fn test_parse_expression() {
        assert_eq!(
            parse_expression("signal(i64.42, i64.64)"),
            Ok(("", Expression::Parameter(Parameter::Signal { index: Atomic::Constant(ConstantType::I64(42)), size: Atomic::Constant(ConstantType::I64(64))})))
        );
        assert_eq!(parse_expression("i64.42"), Ok(("", Expression::Atomic(Atomic::Constant(ConstantType::I64(42))))));
        assert_eq!(parse_expression("adfaf"), Ok(("", Expression::Atomic(Atomic::Variable("adfaf".to_string())))));
    }

    #[test]
    fn test_parse_operation_no_output() {
        assert_eq!(
            parse_operation_no_output("i64.add i64.42 i64.64"),
            Ok((
                "",
                ASTNode::Operation {
                    num_type: Some(NumericType::Integer),
                    operator: Some(Operator::Add),
                    output: None,
                    operands: vec![
                        Expression::Atomic(Atomic::Constant(ConstantType::I64(42))),
                        Expression::Atomic(Atomic::Constant(ConstantType::I64(64))),
                    ]
                }
            ))
        );
        assert_eq!(
            parse_operation_no_output("get_signal i64.42 test"),
            Ok((
                "",
                ASTNode::Operation {
                    num_type: None,
                    operator: Some(Operator::GetSignal),
                    output: None,
                    operands: vec![
                        Expression::Atomic(Atomic::Constant(ConstantType::I64(42))),
                        Expression::Atomic(Atomic::Variable("test".to_string())),
                    ]
                }
            ))
        );
        assert_eq!(
            parse_operation_no_output("ff.call $foo y signal(s,i64.3) ff.memory(i64.0,i64.1)"),
            Ok((
                "",
                ASTNode::Operation {
                    num_type: Some(NumericType::FiniteField),
                    operator: Some(Operator::Call),
                    output: None,
                    operands: vec![
                        Expression::Atomic(Atomic::Variable("$foo".to_string())),
                        Expression::Atomic(Atomic::Variable("y".to_string())),
                        Expression::Parameter(Parameter::Signal {
                            index: Atomic::Variable("s".to_string()),
                            size: Atomic::Constant(ConstantType::I64(3))
                        }),
                        Expression::Parameter(Parameter::FfMemory {
                            index: Atomic::Constant(ConstantType::I64(0)),
                            size: Atomic::Constant(ConstantType::I64(1))
                        })
                    ]
                }
            ))
        );
    }

    #[test]
    fn test_parse_operation_with_output() {
        assert_eq!(
            parse_operation_with_output("result = ff.add ff.42 ff.64"),
            Ok((
                "",
                ASTNode::Operation {
                    num_type: Some(NumericType::FiniteField),
                    operator: Some(Operator::Add),
                    output: Some("result".to_string()),
                    operands: vec![
                        Expression::Atomic(Atomic::Constant(ConstantType::FF(BigInt::from(42)))),
                        Expression::Atomic(Atomic::Constant(ConstantType::FF(BigInt::from(64))))
                    ]
                }
            ))
        );
        assert_eq!(
            parse_operation_with_output("x = ff.call $foo y signal(s,i64.3) ff.memory(i64.0,i64.1)"),
            Ok((
            "",
            ASTNode::Operation {
                num_type: Some(NumericType::FiniteField),
                operator: Some(Operator::Call),
                output: Some("x".to_string()),
                operands: vec![
                Expression::Atomic(Atomic::Variable("$foo".to_string())),
                Expression::Atomic(Atomic::Variable("y".to_string())),
                Expression::Parameter(Parameter::Signal {
                    index: Atomic::Variable("s".to_string()),
                    size: Atomic::Constant(ConstantType::I64(3))
                }),
                Expression::Parameter(Parameter::FfMemory {
                    index: Atomic::Constant(ConstantType::I64(0)),
                    size: Atomic::Constant(ConstantType::I64(1))
                })
                ]
            }
            ))
        );
    }

    #[test]
    fn test_parse_operation_two_variables() {
        assert_eq!(
            parse_operation_two_variables("result = i64.42"),
            Ok((
                "",
                ASTNode::Operation {
                    num_type: None,
                    operator: None,
                    output: Some("result".to_string()),
                    operands: vec![Expression::Atomic(Atomic::Constant(ConstantType::I64(42)))]
                }
            ))
        );
    }

    #[test]
    fn test_parse_operation() {
        assert_eq!(
            parse_operation("result = i64.add i64.42 i64.64"),
            Ok((
                "",
                ASTNode::Operation {
                    num_type: Some(NumericType::Integer),
                    operator: Some(Operator::Add),
                    output: Some("result".to_string()),
                    operands: vec![
                        Expression::Atomic(Atomic::Constant(ConstantType::I64(42))),
                        Expression::Atomic(Atomic::Constant(ConstantType::I64(64)))
                    ]
                }
            ))
        );
        assert_eq!(
            parse_operation("ff.add ff.42 ff.64"),
            Ok((
                "",
                ASTNode::Operation {
                    num_type: Some(NumericType::FiniteField),
                    operator: Some(Operator::Add),
                    output: None,
                    operands: vec![
                        Expression::Atomic(Atomic::Constant(ConstantType::FF(BigInt::from(42)))),
                        Expression::Atomic(Atomic::Constant(ConstantType::FF(BigInt::from(64))))
                    ]
                }
            ))
        );
        assert_eq!(
            parse_operation("res = x"),
            Ok((
                "",
                ASTNode::Operation {
                    num_type: None,
                    operator: None,
                    output: Some("res".to_string()),
                    operands: vec![Expression::Atomic(Atomic::Variable("x".to_string()))]
                }
            ))
        );
    }
}
