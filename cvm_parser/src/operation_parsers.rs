use cfg_ssa::{
    ast::ASTNode,
    types::{Expression, NumericType, Operator, Parameter},
};
use crate::parse_variable_name;

use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::{i64, space0},
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
        value(Operator::Add, tag("add")),
        value(Operator::Sub, tag("sub")),
        value(Operator::Mul, tag("mul")),
        value(Operator::Div, tag("div")),
        value(Operator::Rem, tag("rem")),
        value(Operator::IDiv, tag("idiv")),
        value(Operator::Pow, tag("pow")),
        value(Operator::Greater, tag("gt")),
        value(Operator::GreaterEqual, tag("ge")),
        value(Operator::Less, tag("lt")),
        value(Operator::LessEqual, tag("le")),
        value(Operator::Equal, tag("eq")),
        value(Operator::NotEqual, tag("neq")),
        value(Operator::EqualZero, tag("eqz")),
        value(Operator::And, tag("and")),
        value(Operator::Or, tag("or")),
        value(Operator::ShiftRight, tag("shr")),
        value(Operator::ShiftLeft, tag("shl")),
        value(Operator::BitAnd, tag("band")),
        value(Operator::BitOr, tag("bor")),
        value(Operator::BitXor, tag("bxor")),
        value(Operator::BitNot, tag("bnot")),
        value(Operator::Extend, tag("extend_ff")),
        value(Operator::Wrap, tag("wrap_i64")),
        value(Operator::Load, tag("load")),
        value(Operator::Store, tag("store")),
        value(Operator::GetSignal, tag("get_signal")),
        value(Operator::GetCmpSignal, tag("get_cmp_signal")),
        value(Operator::SetSignal, tag("set_signal")),
        value(Operator::SetCmpSignal, tag("set_cmp_signal")),
        value(Operator::SetCmpIn, tag("set_cmp_input")),
        value(Operator::SetCmpInCnt, tag("set_cmp_input_cnt")),
        value(Operator::SetCmpInRun, tag("set_cmp_input_run")),
        value(Operator::SetCmpInCntCheck, tag("set_cmp_input_cnt_check")),
        value(Operator::Return, tag("return")),
        value(Operator::Call, tag("call")),
        value(Operator::Error, tag("error")),
    ])
    .parse(input)
}

//TODO: This is useless, we should parse expressions
fn parse_two_i64(input: &str) -> IResult<&str, (i64, i64)> {
    delimited(tag("("), separated_pair(i64, delimited(space0, tag(","), space0), i64), tag(")"))
        .parse(input)
}

fn parse_signal(input: &str) -> IResult<&str, Parameter> {
    map((tag("signal"), parse_two_i64), |(_, (index, size))| Parameter::Signal { index, size })
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
                    parse_variable_name,
                    delimited(space0, tag(","), space0),
                    i64,
                    delimited(space0, tag(","), space0),
                    i64,
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
    map((tag("i64.memory"), parse_two_i64), |(_, (index, size))| Parameter::I64Memory {
        index,
        size,
    })
    .parse(input)
}

// Parses: ff.memory(index, size)
fn parse_ff_memory(input: &str) -> IResult<&str, Parameter> {
    map((tag("ff.memory"), parse_two_i64), |(_, (index, size))| Parameter::FfMemory { index, size })
        .parse(input)
}

fn parse_parameter(input: &str) -> IResult<&str, Parameter> {
    alt((parse_signal, parse_subcmp_signal, parse_i64_memory, parse_ff_memory)).parse(input)
}

pub fn parse_expression(input: &str) -> IResult<&str, Expression> {
    alt((
        map(i64, Expression::Constant),
        map(parse_parameter, Expression::Parameter),
        map(parse_variable_name, Expression::Variable),
    ))
    .parse(input)
}

fn parse_operation_no_output(input: &str) -> IResult<&str, Box<ASTNode>> {
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

    Ok((input, Box::new(ASTNode::Operation { num_type, operator, output: None, operands })))
}

fn parse_operation_with_output(input: &str) -> IResult<&str, Box<ASTNode>> {
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

    Ok((input, Box::new(ASTNode::Operation { num_type, operator, output: Some(output), operands })))
}

pub fn parse_operation(input: &str) -> IResult<&str, Box<ASTNode>> {
    alt((parse_operation_no_output, parse_operation_with_output)).parse(input)
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
        assert!(parse_operator("unknown").is_err());
    }

    #[test]
    fn test_parse_two_i64() {
        assert_eq!(parse_two_i64("(1,2)"), Ok(("", (1, 2))));
        assert!(parse_two_i64("(1,)").is_err());
    }

    #[test]
    fn test_parse_signal() {
        assert_eq!(parse_signal("signal(1,2)"), Ok(("", Parameter::Signal { index: 1, size: 2 })));
        assert!(parse_signal("signal(1,)").is_err());
    }

    #[test]
    fn test_parse_subcmp_signal() {
        assert_eq!(
            parse_subcmp_signal("subcmpsignal(component,1,2)"),
            Ok((
                "",
                Parameter::SubcmpSignal { component: "component".to_string(), index: 1, size: 2 }
            ))
        );
        assert!(parse_subcmp_signal("subcmpsignal(component,1,)").is_err());
    }

    #[test]
    fn test_parse_i64_memory() {
        assert_eq!(
            parse_i64_memory("i64.memory(1,2)"),
            Ok(("", Parameter::I64Memory { index: 1, size: 2 }))
        );
        assert!(parse_i64_memory("i64.memory(1,)").is_err());
    }

    #[test]
    fn test_parse_ff_memory() {
        assert_eq!(
            parse_ff_memory("ff.memory(1,2)"),
            Ok(("", Parameter::FfMemory { index: 1, size: 2 }))
        );
        assert!(parse_ff_memory("ff.memory(1,)").is_err());
    }

    #[test]
    fn test_parse_parameter() {
        assert_eq!(
            parse_parameter("signal(1,2)"),
            Ok(("", Parameter::Signal { index: 1, size: 2 }))
        );
        assert_eq!(
            parse_parameter("subcmpsignal(component,1,2)"),
            Ok((
                "",
                Parameter::SubcmpSignal { component: "component".to_string(), index: 1, size: 2 }
            ))
        );
        assert_eq!(
            parse_parameter("i64.memory(1,2)"),
            Ok(("", Parameter::I64Memory { index: 1, size: 2 }))
        );
        assert_eq!(
            parse_parameter("ff.memory(1,2)"),
            Ok(("", Parameter::FfMemory { index: 1, size: 2 }))
        );
    }

    #[test]
    fn test_parse_expression() {
        assert_eq!(parse_expression("42"), Ok(("", Expression::Constant(42))));
        assert_eq!(
            parse_expression("variable"),
            Ok(("", Expression::Variable("variable".to_string())))
        );
        assert_eq!(
            parse_expression("signal(1,2)"),
            Ok(("", Expression::Parameter(Parameter::Signal { index: 1, size: 2 })))
        );
    }

    #[test]
    fn test_parse_operation() {
        assert_eq!(
            parse_operation("x = i64.add 1 2"),
            Ok((
                "",
                Box::new(ASTNode::Operation {
                    num_type: Some(NumericType::Integer),
                    operator: Operator::Add,
                    output: Some("x".to_string()),
                    operands: vec![Expression::Constant(1), Expression::Constant(2)],
                })
            ))
        );
        assert_eq!(
            parse_operation("ff.mul 3 4"),
            Ok((
                "",
                Box::new(ASTNode::Operation {
                    num_type: Some(NumericType::FiniteField),
                    operator: Operator::Mul,
                    output: None,
                    operands: vec![Expression::Constant(3), Expression::Constant(4)],
                })
            ))
        );
    }

    #[test]
    fn test_parse_operation_signal() {
        assert_eq!(
            parse_operation("set_signal signal(1,2) 3"),
            Ok((
                "",
                Box::new(ASTNode::Operation {
                    num_type: None,
                    operator: Operator::SetSignal,
                    output: None,
                    operands: vec![
                        Expression::Parameter(Parameter::Signal { index: 1, size: 2 }),
                        Expression::Constant(3)
                    ],
                })
            ))
        );
    }

    #[test]
    fn test_parse_operation_function_call() {
        assert_eq!(
            parse_operation("call $my_function 1 2 3"),
            Ok((
                "",
                Box::new(ASTNode::Operation {
                    num_type: None,
                    operator: Operator::Call,
                    output: None,
                    operands: vec![
                        Expression::Variable("$my_function".to_string()),
                        Expression::Constant(1),
                        Expression::Constant(2),
                        Expression::Constant(3)
                    ],
                })
            ))
        );
    }

    #[test]
    fn test_parse_complex_operation() {
        assert_eq!(
            parse_operation("x = ff.call $foo y signal(0,3) ff.memory(0,1)"),
            Ok((
                "",
                Box::new(ASTNode::Operation {
                    num_type: Some(NumericType::FiniteField),
                    operator: Operator::Call,
                    output: Some("x".to_string()),
                    operands: vec![
                        Expression::Variable("$foo".to_string()),
                        Expression::Variable("y".to_string()),
                        Expression::Parameter(Parameter::Signal { index: 0, size: 3 }),
                        Expression::Parameter(Parameter::FfMemory { index: 0, size: 1 }),
                    ],
                })
            ))
        );
    }

    #[test]
    fn test_parse_operation_return() {
        assert_eq!(
            parse_operation("return 42"),
            Ok((
                "",
                Box::new(ASTNode::Operation {
                    num_type: None,
                    operator: Operator::Return,
                    output: None,
                    operands: vec![Expression::Constant(42)],
                })
            ))
        );
    }
}
