use nom::{
    branch::alt, bytes::{complete::take_until, is_not, tag}, character::complete::{alpha1, alphanumeric1, digit1, i64, line_ending, multispace0, space0, space1, usize}, combinator::{map, opt, recognize, value}, multi::{many0, separated_list1}, sequence::{delimited, preceded, terminated}, IResult, Parser
};
use cfg_ssa::*;

fn parse_numeric_type(input: &str) -> IResult<&str, NumericType> {
    alt((
        value(NumericType::Integer, tag("i64")),
        value(NumericType::FiniteField, tag("ff")),
    )).parse(input)
}

fn parse_component_mode(input: &str) -> IResult<&str, ComponentMode> {
    alt((
        value(ComponentMode::Implicit, tag("implicit")),
        value(ComponentMode::Implicit, tag("explicit")),
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

fn parse_variable_name(input: &str) -> IResult<&str, String> {
    recognize(
        preceded(
            alt((tag("_"), alpha1)),
            many0(alt((alphanumeric1, tag("_"))))
        )
    ).parse(input)
    .map(|(remain, var)| (remain, var.to_string()))
}

fn parse_operand(input: &str) -> IResult<&str, Operand> {
    alt((
        map(i64, Operand::Constant),
        map(parse_variable_name, Operand::Variable) 
    )).parse(input)
}

fn parse_operation(input: &str) -> IResult<&str, (NumericType, Operator, Operand, Option<Operand>)> {
    let (input, numeric_type) = parse_numeric_type(input)?;

    let (input, operator) = preceded(space1, parse_operator).parse(input)?;

    let (input, operand1) = preceded(space1, parse_operand).parse(input)?;

    let (input, operand2) = opt(preceded(space1, parse_operand)).parse(input)?;

    Ok((input, (numeric_type, operator, operand1, operand2)))
}

fn parse_signal_operation(input: &str) -> IResult<&str, (Operator, Operand, Option<Operand>, Option<Operand>)> {
    let (input, operator) = preceded(space1, parse_operator).parse(input)?;

    let (input, operand1) = preceded(space1, parse_operand).parse(input)?;

    let (input, operand2) = opt(preceded(space1, parse_operand)).parse(input)?;

    let (input, operand3) = opt(preceded(space1, parse_operand)).parse(input)?;

    Ok((input, (operator, operand1, operand2, operand3)))
}

fn parse_expression(input: &str) -> IResult<&str, Expression> {
    alt((
        map(parse_operand, Expression::Atomic),
        map(parse_operation, |(nt, ope, op1, op2)| Expression::Operation(nt, ope, op1, op2)),
        map(parse_signal_operation, |(ope, op1, op2, op3)| Expression::SignalOperation(ope, op1, op2, op3)),
    )).parse(input)
}

fn parse_comment(input: &str) -> IResult<&str, &str> {
    preceded(tag("//"), take_until("\n")).parse(input)
}

fn parse_empty_line(input: &str) -> IResult<&str, &str> {
    terminated(multispace0, line_ending).parse(input)
}

fn parse_empty_line_or_comments(input: &str) -> IResult<&str, ()> {
    let (input, _) = many0(alt((parse_comment, parse_empty_line))).parse(input)?;
    Ok((input, ()))
}

fn parse_assignment(input: &str) -> IResult<&str, (String, Expression)> {
    (parse_variable_name, preceded(delimited(space0, tag("="), space0), parse_expression)).parse(input)
}

fn parse_instruction(input: &str) -> IResult<&str, Instruction> {
    preceded(parse_empty_line_or_comments,
    alt((
        map(parse_assignment, |(var, expr)| Instruction::Assignment(var, expr)),
        map(parse_expression, Instruction::Expr),
        value(Instruction::Loop, tag("loop")),
        value(Instruction::Break, tag("break")),
        value(Instruction::Continue, tag("continue")),
        map(preceded(tag("if"), preceded(space1, parse_variable_name)),
            |var: String| Instruction::If(Expression::Atomic(Operand::Variable(var)))),
        value(Instruction::Else, tag("else")),
        value(Instruction::Return, tag("return")),
        map(i64, Instruction::Error),
        map(preceded(tag("%%template"), terminated(separated_list1(space1, is_not(" \n")), line_ending)),
             |words| Instruction::Template(words.into_iter().map(String::from).collect())),
    ))).parse(input)
}

fn parse_prime(input: &str) -> IResult<&str, Instruction> {
    preceded(parse_empty_line_or_comments, 
        map(preceded(tag("%%prime"), preceded(space1, digit1)),
            |prime: &str| Instruction::Prime(prime.to_string())))
    .parse(input)
}

fn parse_signals(input: &str) -> IResult<&str, Instruction> {
    preceded(parse_empty_line_or_comments, 
        map(preceded(tag("%%signals"), preceded(space1, usize)),
            Instruction::Signals))
    .parse(input)
}

fn parse_components_heap(input: &str) -> IResult<&str, Instruction> {
    preceded(parse_empty_line_or_comments, 
        map(preceded(tag("%%components_heap"), preceded(space1, usize)),
            Instruction::Heap))
    .parse(input)
}

fn parse_start(input: &str) -> IResult<&str, Instruction> {
    preceded(parse_empty_line_or_comments, 
        map(preceded(tag("%%start"), preceded(space1, parse_variable_name)),
            Instruction::Start))
    .parse(input)
}

fn parse_components(input: &str) -> IResult<&str, Instruction> {
    preceded(parse_empty_line_or_comments, 
        map(preceded(tag("%%components"), preceded(space1, parse_component_mode)),
            Instruction::Components))
    .parse(input)
}

fn parse_witness(input: &str) -> IResult<&str, Instruction> {
    preceded(parse_empty_line_or_comments, 
        map(separated_list1(space1, usize),
            Instruction::Witness))
    .parse(input)
}

fn parse_program(input: &str) -> IResult<&str, CFG> {
    //Entry point is 1 and 0 will be where the initial information will be stored
    let mut graph = CFG::new(1);
    //Block of initial information
    graph.add_block();

    //Parse prime
    let (input, prime) = parse_prime(input)?;
    graph.add_initial_instruction(prime);

    //Parse signals
    let (input, signals) = parse_signals(input)?;
    graph.add_initial_instruction(signals);

    //Parse components heap
    let (input, heap) = parse_components_heap(input)?;
    graph.add_initial_instruction(heap);
    
    //Parse start
    let (input, start) = parse_start(input)?;
    graph.add_initial_instruction(start);

    //Parse components mode
    let (input, components) = parse_components(input)?;
    graph.add_initial_instruction(components);

    //Parse witness
    let (input, witness) = parse_witness(input)?;
    graph.add_initial_instruction(witness);
    
    //Parse the rest of the program
    //First block of actual code
    graph.add_block();
    let mut input = input;
    while let Ok((new_input, ins)) = parse_instruction(input) {
        input = new_input;
        match ins {
            Instruction::Assignment(var, expr) => {
                //TODO: Crear cadenas def-use, use-def
                graph.add_instruction_to_current_block(Instruction::Assignment(var, expr));
            },
            Instruction::Expr(expr) => {
                graph.add_instruction_to_current_block(Instruction::Expr(expr));
            },
            Instruction::Loop => {
                graph.add_loop_blocks();
            },
            Instruction::Break => {
            },
            Instruction::Continue => {
                graph.add_edge_continue();
            },
            Instruction::If(expr) => {
                graph.add_instruction_to_current_block(Instruction::If(expr));
                graph.add_if_block();
            },
            Instruction::Else => {
            },
            Instruction::End => {
                //TODO: when exactly do we use this instruction?
            },
            Instruction::Return => {
                //TODO: when exactly do we use this instruction?
            },
            Instruction::Error(n) => {
                //TODO: what to do with this
            },
            Instruction::Template(words) => {
                //TODO: what to do with this
            },
            _ => {
                panic!("unexpected instruction");
            }
        }
    }



    Ok(("", graph))
}
