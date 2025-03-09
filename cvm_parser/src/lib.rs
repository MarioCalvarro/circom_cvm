use nom::{
    IResult,
    branch::alt,
    combinator::{value, recognize, map, opt},
    bytes::tag, Parser,
    character::complete::{alphanumeric1, i64, alpha1, char},
    sequence::preceded, multi::many0,
};
use cfg_ssa::*;

fn parse_numeric_type(input: &str) -> IResult<&str, NumericType> {
    alt((
        value(NumericType::Integer, tag("i64")),
        value(NumericType::FiniteField, tag("ff")),
    )).parse(input)
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
    ]).parse(input)
}

fn parse_variable_name(input: &str) -> IResult<&str, &str> {
    recognize(
        preceded(
            alt((tag("_"), alpha1)),
            many0(alt((alphanumeric1, tag("_"))))
        )
    ).parse(input)
}

fn parse_operand(input: &str) -> IResult<&str, Operand> {
    alt((
        map(i64, Operand::Constant),
        map(parse_variable_name, |name| Operand::Variable(name.to_string())) 
    )).parse(input)
}

fn parse_operation(input: &str) -> IResult<&str, (NumericType, Operator, Operand, Option<Operand>)> {
    let (input, numeric_type) = parse_numeric_type(input)?;

    let (input, operator) = preceded(char(' '), parse_operator).parse(input)?;

    let (input, operand1) = preceded(char(' '), parse_operand).parse(input)?;

    let (input, operand2) = opt(preceded(char(' '), parse_operand)).parse(input)?;

    Ok((input, (numeric_type, operator, operand1, operand2)))
}

fn parse_signal_operation(input: &str) -> IResult<&str, (Operator, Operand, Option<Operand>, Option<Operand>)> {
    let (input, operator) = preceded(char(' '), parse_operator).parse(input)?;

    let (input, operand1) = preceded(char(' '), parse_operand).parse(input)?;

    let (input, operand2) = opt(preceded(char(' '), parse_operand)).parse(input)?;

    let (input, operand3) = opt(preceded(char(' '), parse_operand)).parse(input)?;

    Ok((input, (operator, operand1, operand2, operand3)))
}

fn parse_expression(input: &str) -> IResult<&str, Expression> {
    alt((
        map(parse_operand, Expression::Atomic),
        map(parse_operation, |(nt, ope, op1, op2)| Expression::Operation(nt, ope, op1, op2)),
        map(parse_signal_operation, |(ope, op1, op2, op3)| Expression::SignalOperation(ope, op1, op2, op3)),
    )).parse(input)
}
