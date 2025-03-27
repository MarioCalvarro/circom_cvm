extern crate num_bigint_dig as num_bigint;

use nom::{
    branch::alt, bytes::tag, character::{complete::{alphanumeric1, multispace1, none_of, satisfy, space0, usize}, streaming::space1}, combinator::{complete, map, opt, recognize, value}, multi::{many0, separated_list0}, sequence::{delimited, pair, preceded}, IResult, Parser
};
use cfg_ssa::ast::*;

mod initial_parsers;
mod operation_parsers;
use initial_parsers::*;
use operation_parsers::{parse_expression, parse_operation};

fn parse_variable_name(input: &str) -> IResult<&str, String> {
    recognize(
        pair(
            satisfy(|c| c.is_ascii_alphabetic() || c == '_' || c == '$'),
            many0(satisfy(|c| c.is_ascii_alphanumeric() || c == '_'))
        )
    ).parse(input)
    .map(|(remain, var)| (remain, var.to_string()))
}

fn parse_comment(input: &str) -> IResult<&str, ()> {
    let (input, _) = preceded(tag(";;"), many0(none_of("\n"))).parse(input)?;
    Ok((input, ()))
}

fn parse_useless(input: &str) -> IResult<&str, ()> {
    let (input, _) = opt(many0(alt((
        map(multispace1, |_| ()),
        complete(parse_comment),
    ))
    )).parse(input)?;
    Ok((input, ()))
}

fn parse_loop(input: &str) -> IResult<&str, Box<ASTNode>> {
    let (input, _) = tag("loop").parse(input)?;
    let (input, _) = parse_useless(input)?;
    let (input, body) = many0(parse_ast_node).parse(input)?;
    let (input, _) = parse_useless(input)?;
    let (input, _) = tag("end").parse(input)?;
    Ok((input, Box::new(ASTNode::Loop {body})))
}

fn parse_if_then_else(input: &str) -> IResult<&str, Box<ASTNode>> {
    let (input, _) = tag("if").parse(input)?;
    let (input, _) = space1(input)?;
    let (input, condition) = parse_expression(input)?;
    let (input, _) = parse_useless(input)?;
    let (input, if_case) = many0(parse_ast_node).parse(input)?;
    let (input, _) = parse_useless(input)?;
    let (input, else_case) = opt(preceded(tag("else"), many0(parse_ast_node))).parse(input)?;
    let (input, _) = parse_useless(input)?;
    let (input, _) = tag("end").parse(input)?;
    Ok((input, Box::new(ASTNode::IfThenElse {condition, if_case, else_case})))
}

fn parse_break(input: &str) -> IResult<&str, Box<ASTNode>> {
    value(Box::new(ASTNode::Break), tag("break")).parse(input)
}

fn parse_continue(input: &str) -> IResult<&str, Box<ASTNode>> {
    value(Box::new(ASTNode::Continue), tag("continue")).parse(input)
}

fn parse_ast_node(input: &str) -> IResult<&str, Box<ASTNode>> {
    //Delimited by useless to avoid parsing comments and whitespaces
    complete(delimited(
     parse_useless,
     alt((parse_operation, parse_if_then_else, parse_loop, parse_break, parse_continue)),
     parse_useless
    )).parse(input)
}

fn parse_template(input: &str) -> IResult<&str, Template> {
    let (input, _) = tag("%%template").parse(input)?;
    let (input, _) = space1(input)?;

    let (input, name) = parse_variable_name(input)?;
    let (input, _) = space0(input)?;

    let (input, inputs) = delimited(tag("["),
    delimited(space0, separated_list0(space1, alphanumeric1), space0), 
    tag("]")).parse(input)?;
    let inputs = inputs.iter().map(|x| x.to_string()).collect();
    let (input, _) = space0(input)?;

    let (input, outputs) = delimited(tag("["),
    delimited(space0, separated_list0(space1, alphanumeric1), space0), 
    tag("]")).parse(input)?;
    let outputs = outputs.iter().map(|x| x.to_string()).collect();
    let (input, _) = space0(input)?;

    let (input, signals) = delimited(tag("["),
    delimited(space0, usize, space0),
    tag("]")).parse(input)?;
    let (input, _) = space0(input)?;

    let (input, components) = delimited(tag("["),
    delimited(space0, usize, space0),
    tag("]")).parse(input)?;

    let (input, body) = many0(parse_ast_node).parse(input)?;

    Ok((input, Template {
        name,
        inputs,
        outputs,
        signals,
        components,
        body,
    }))
}

fn parse_program(input: &str) -> IResult<&str, AST> {
    let (input, _) = parse_useless(input)?;
    let (input, field) = parse_prime(input)?;
    let (input, _) = parse_useless(input)?;

    let (input, signals_memory) = parse_signal_memory(input)?;
    let (input, _) = parse_useless(input)?;

    let (input, components_heap) = parse_components_heap(input)?;
    let (input, _) = parse_useless(input)?;

    let (input, main_template) = parse_start(input)?;
    let (input, _) = parse_useless(input)?;

    let (input, components_creation_mode) = parse_components(input)?;
    let (input, _) = parse_useless(input)?;

    let (input, witness) = parse_witness(input)?;
    let (input, _) = parse_useless(input)?;

    let (input, templates) = many0(complete(parse_template)).parse(input)?;
    Ok((input, AST {
        field,
        signals_memory,
        components_heap,
        main_template,
        components_creation_mode,
        witness,
        templates,
    }))
}

#[cfg(test)]
mod tests {
    use cfg_ssa::types::{Atomic, Expression, NumericType, Operator};

    use super::*;

    #[test]
    fn test_parse_variable_name() {
        assert_eq!(parse_variable_name("_var1"), Ok(("", "_var1".to_string())));
        assert_eq!(parse_variable_name("$var2"), Ok(("", "$var2".to_string())));
        assert_eq!(parse_variable_name("var3"), Ok(("", "var3".to_string())));
        assert_eq!(parse_variable_name("var"), Ok(("", "var".to_string())));
        assert!(parse_variable_name("1var").is_err());
    }

    #[test]
    fn test_parse_comment() {
        assert_eq!(parse_comment(";; this is a comment\n"), Ok(("\n", ())));
        assert!(parse_comment("this is not a comment").is_err());
    }

    #[test]
    fn test_parse_useless() {
        assert_eq!(parse_useless("   ;; comment\n  ;;lkasjfl\n "), Ok(("", ())));
        assert_eq!(parse_useless("   \n   "), Ok(("", ())));
        assert!(parse_useless("").is_ok());
    }

    #[test]
    fn test_parse_loop() {
        let input = "loop\n  ;; comment\n  x = ff.add x y\n y = i64.sub 3 z\n ;;comment\n end\n";
        assert_eq!(parse_loop(input), Ok(("\n", Box::new(ASTNode::Loop { body: vec![
            Box::new(ASTNode::Operation {
            num_type: Some(NumericType::FiniteField),
            operator: Some(Operator::Add),
            output: Some("x".to_string()),
            operands: vec![
                Expression::Atomic(Atomic::Variable("x".to_string())),
                Expression::Atomic(Atomic::Variable("y".to_string()))
            ],
            }),
            Box::new(ASTNode::Operation {
            num_type: Some(NumericType::Integer),
            operator: Some(Operator::Sub),
            output: Some("y".to_string()),
            operands: vec![
                Expression::Atomic(Atomic::Constant(3)),
                Expression::Atomic(Atomic::Variable("z".to_string()))
            ],
            })
        ]}))));
    }

    #[test]
    fn test_parse_if_then_else() {
        let input = "if condition\n  ;; comment\n x = ff.add x y\n else\n  ;; comment\n x = 1 end\n";
        let expected = Box::new(ASTNode::IfThenElse {
            condition: Expression::Atomic(Atomic::Variable("condition".to_string())),
            if_case: vec![Box::new(ASTNode::Operation {
                num_type: Some(NumericType::FiniteField),
                operator: Some(Operator::Add),
                output: Some("x".to_string()),
                operands: vec![Expression::Atomic(Atomic::Variable("x".to_string())), Expression::Atomic(Atomic::Variable("y".to_string()))],
            })],
            else_case: Some(vec![Box::new(ASTNode::Operation {
                num_type: None,
                operator: None,
                output: Some("x".to_string()),
                operands: vec![Expression::Atomic(Atomic::Constant(1))],
            })]),
        });
        assert_eq!(parse_if_then_else(input), Ok(("\n", expected)));
    }

    #[test]
    fn test_parse_template() {
        let input = "%%template template_name [input1 input2] [output1] [10] [5]\n  ;; body\n x = ff.add y z\n";
        let res = parse_template(input);
        println!("{:?}", res);
        assert!(res.is_ok());
    }

    #[test]
    fn test_parse_program() {
        let input = ";; Prime value
                %%prime 21888242871839275222246405745257275088548364400416034343698204186575808495617


                ;; Memory of signals
                %%signals 200


                ;; Heap of components
                %%components_heap 19


                ;; Types (for each field we store name type offset size nDims dims


                ;; Main template
                %%start A_3


                ;; Component creation mode (implicit/explicit)
                %%components explicit


                ;; Witness (signal list)
                %%witness 0 1 2 3 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50 51 52 53 54 55 56 57 58 59 60 61 62 63 64 65 66 67 68 70 71 72 73 74 75 76 77 78 79 80 81 82 83 84 85 86 87 88 89 90 91 92 93 94 95 96 97 98 99 100 101 135


                %%template Num2Bits_0 [ ff 0 ] [ ff 1 32] [33] [0]
                ;; store bucket. Line 0
                ;; getting src
                ;; getting dest
                ff.store 0 32
                ;; end of store bucket
                ;; store bucket. Line 28
                ;; getting src
                ;; getting dest
                ff.store 1 0
                ;; end of store bucket
                ;; store bucket. Line 30
                ;; getting src
                ;; getting dest
                ff.store 2 1
                ;; end of store bucket
                ;; store bucket. Line 31
                ;; getting src
                ;; getting dest
                ff.store 3 0
                ;; end of store bucket
                ;; loop bucket. Line 31
                loop
                ;; compute bucket
                ;; load bucket
                x_0 = ff.load 3
                ;; end of load bucket
                ;; OP(LESSER)
                x_1 = ff.lt x_0 32
                ;; end of compute bucket
                if x_1
                ;; store bucket. Line 32
                ;; getting src
                ;; compute bucket
                ;; compute bucket
                ;; load bucket
                x_2 = get_signal 32
                ;; end of load bucket
                ;; load bucket
                x_3 = ff.load 3
                ;; end of load bucket
                ;; OP(SHIFT_R)
                x_4 = ff.shr x_2 x_3
                ;; end of compute bucket
                ;; OP(BITAND)
                x_5 = ff.band x_4 1
                ;; end of compute bucket
                ;; getting dest
                ;; compute bucket
                ;; compute bucket
                ;; compute bucket
                ;; load bucket
                x_6 = ff.load 3
                ;; end of load bucket
                ;; OP(TO_ADDRESS)
                x_7 = ff.wrap_i64 x_6
                ;; end of compute bucket
                ;; OP(MUL_ADDRESS)
                x_8 = i64.mul 1 x_7
                ;; end of compute bucket
                ;; OP(ADD_ADDRESS)
                x_9 = i64.add x_8 0
                ;; end of compute bucket
                set_signal x_9 x_5
                ;; end of store bucket
                ;; assert bucket
                ;; compute bucket
                ;; compute bucket
                ;; load bucket
                ;; compute bucket
                ;; compute bucket
                ;; compute bucket
                ;; load bucket
                x_11 = ff.load 3
                ;; end of load bucket
                ;; OP(TO_ADDRESS)
                x_12 = ff.wrap_i64 x_11
                ;; end of compute bucket
                ;; OP(MUL_ADDRESS)
                x_13 = i64.mul 1 x_12
                ;; end of compute bucket
                ;; OP(ADD_ADDRESS)
                x_14 = i64.add x_13 0
                ;; end of compute bucket
                x_10 = get_signal x_14
                ;; end of load bucket
                ;; compute bucket
                ;; load bucket
                ;; compute bucket
                ;; compute bucket
                ;; compute bucket
                ;; load bucket
                x_16 = ff.load 3
                ;; end of load bucket
                ;; OP(TO_ADDRESS)
                x_17 = ff.wrap_i64 x_16
                ;; end of compute bucket
                ;; OP(MUL_ADDRESS)
                x_18 = i64.mul 1 x_17
                ;; end of compute bucket
                ;; OP(ADD_ADDRESS)
                x_19 = i64.add x_18 0
                ;; end of compute bucket
                x_15 = get_signal x_19
                ;; end of load bucket
                ;; OP(SUB)
                x_20 = ff.sub x_15 1
                ;; end of compute bucket
                ;; OP(MUL)
                x_21 = ff.mul x_10 x_20
                ;; end of compute bucket
                ;; OP(EQ(Single(1)))
                x_22 = ff.eq x_21 0
                ;; end of compute bucket
                x_23 = ff.eqz x_22
                if x_23
                error 0
                end
                ;; end of assert bucket
                ;; store bucket. Line 34
                ;; getting src
                ;; compute bucket
                ;; load bucket
                x_24 = ff.load 1
                ;; end of load bucket
                ;; compute bucket
                ;; load bucket
                ;; compute bucket
                ;; compute bucket
                ;; compute bucket
                ;; load bucket
                x_26 = ff.load 3
                ;; end of load bucket
                ;; OP(TO_ADDRESS)
                x_27 = ff.wrap_i64 x_26
                ;; end of compute bucket
                ;; OP(MUL_ADDRESS)
                x_28 = i64.mul 1 x_27
                ;; end of compute bucket
                ;; OP(ADD_ADDRESS)
                x_29 = i64.add x_28 0
                ;; end of compute bucket
                x_25 = get_signal x_29
                ;; end of load bucket
                ;; load bucket
                x_30 = ff.load 2
                ;; end of load bucket
                ;; OP(MUL)
                x_31 = ff.mul x_25 x_30
                ;; end of compute bucket
                ;; OP(ADD)
                x_32 = ff.add x_24 x_31
                ;; end of compute bucket
                ;; getting dest
                ff.store 1 x_32
                ;; end of store bucket
                ;; store bucket. Line 35
                ;; getting src
                ;; compute bucket
                ;; load bucket
                x_33 = ff.load 2
                ;; end of load bucket
                ;; load bucket
                x_34 = ff.load 2
                ;; end of load bucket
                ;; OP(ADD)
                x_35 = ff.add x_33 x_34
                ;; end of compute bucket
                ;; getting dest
                ff.store 2 x_35
                ;; end of store bucket
                ;; store bucket. Line 31
                ;; getting src
                ;; compute bucket
                ;; load bucket
                x_36 = ff.load 3
                ;; end of load bucket
                ;; OP(ADD)
                x_37 = ff.add x_36 1
                ;; end of compute bucket
                ;; getting dest
                ff.store 3 x_37
                ;; end of store bucket
                continue
                end
                end
                ;; end of loop bucket
                ;; assert bucket
                ;; compute bucket
                ;; load bucket
                x_38 = ff.load 1
                ;; end of load bucket
                ;; load bucket
                x_39 = get_signal 32
                ;; end of load bucket
                ;; OP(EQ(Single(1)))
                x_40 = ff.eq x_38 x_39
                ;; end of compute bucket
                x_41 = ff.eqz x_40
                if x_41
                error 0
                end
                ;; end of assert bucket

                %%template BinSum_1 [ ff 2 2 32] [ ff 1 33] [97] [0]
                ;; store bucket. Line 0
                ;; getting src
                ;; getting dest
                ff.store 0 32
                ;; end of store bucket
                ;; store bucket. Line 0
                ;; getting src
                ;; getting dest
                ff.store 1 2
                ;; end of store bucket
                ;; store bucket. Line 70
                ;; getting src
                ;; getting dest
                ff.store 3 0
                ;; end of store bucket
                ;; store bucket. Line 71
                ;; getting src
                ;; getting dest
                ff.store 4 0
                ;; end of store bucket
                ;; store bucket. Line 73
                ;; getting src
                ;; getting dest
                ff.store 5 0
                ;; end of store bucket
                ;; store bucket. Line 74
                ;; getting src
                ;; getting dest
                ff.store 6 0
                ;; end of store bucket
                ;; store bucket. Line 76
                ;; getting src
                ;; getting dest
                ff.store 7 0
                ;; end of store bucket
                ;; store bucket. Line 78
                ;; getting src
                ;; getting dest
                ff.store 7 1
                ;; end of store bucket
                ;; store bucket. Line 79
                ;; getting src
                ;; getting dest
                ff.store 5 0
                ;; end of store bucket
                ;; loop bucket. Line 79
                loop
                ;; compute bucket
                ;; load bucket
                x_42 = ff.load 5
                ;; end of load bucket
                ;; OP(LESSER)
                x_43 = ff.lt x_42 32
                ;; end of compute bucket
                if x_43
                ;; store bucket. Line 80
                ;; getting src
                ;; getting dest
                ff.store 6 0
                ;; end of store bucket
                ;; loop bucket. Line 80
                loop
                ;; compute bucket
                ;; load bucket
                x_44 = ff.load 6
                ;; end of load bucket
                ;; OP(LESSER)
                x_45 = ff.lt x_44 2
                ;; end of compute bucket
                if x_45
                ;; store bucket. Line 81
                ;; getting src
                ;; compute bucket
                ;; load bucket
                x_46 = ff.load 3
                ;; end of load bucket
                ;; compute bucket
                ;; load bucket
                ;; compute bucket
                ;; compute bucket
                ;; compute bucket
                ;; compute bucket
                ;; load bucket
                x_48 = ff.load 6
                ;; end of load bucket
                ;; OP(TO_ADDRESS)
                x_49 = ff.wrap_i64 x_48
                ;; end of compute bucket
                ;; OP(MUL_ADDRESS)
                x_50 = i64.mul 32 x_49
                ;; end of compute bucket
                ;; compute bucket
                ;; compute bucket
                ;; load bucket
                x_51 = ff.load 5
                ;; end of load bucket
                ;; OP(TO_ADDRESS)
                x_52 = ff.wrap_i64 x_51
                ;; end of compute bucket
                ;; OP(MUL_ADDRESS)
                x_53 = i64.mul 1 x_52
                ;; end of compute bucket
                ;; OP(ADD_ADDRESS)
                x_54 = i64.add x_50 x_53
                ;; end of compute bucket
                ;; OP(ADD_ADDRESS)
                x_55 = i64.add x_54 33
                ;; end of compute bucket
                x_47 = get_signal x_55
                ;; end of load bucket
                ;; load bucket
                x_56 = ff.load 7
                ;; end of load bucket
                ;; OP(MUL)
                x_57 = ff.mul x_47 x_56
                ;; end of compute bucket
                ;; OP(ADD)
                x_58 = ff.add x_46 x_57
                ;; end of compute bucket
                ;; getting dest
                ff.store 3 x_58
                ;; end of store bucket
                ;; store bucket. Line 80
                ;; getting src
                ;; compute bucket
                ;; load bucket
                x_59 = ff.load 6
                ;; end of load bucket
                ;; OP(ADD)
                x_60 = ff.add x_59 1
                ;; end of compute bucket
                ;; getting dest
                ff.store 6 x_60
                ;; end of store bucket
                continue
                end
                end
                ;; end of loop bucket
                ;; store bucket. Line 83
                ;; getting src
                ;; compute bucket
                ;; load bucket
                x_61 = ff.load 7
                ;; end of load bucket
                ;; load bucket
                x_62 = ff.load 7
                ;; end of load bucket
                ;; OP(ADD)
                x_63 = ff.add x_61 x_62
                ;; end of compute bucket
                ;; getting dest
                ff.store 7 x_63
                ;; end of store bucket
                ;; store bucket. Line 79
                ;; getting src
                ;; compute bucket
                ;; load bucket
                x_64 = ff.load 5
                ;; end of load bucket
                ;; OP(ADD)
                x_65 = ff.add x_64 1
                ;; end of compute bucket
                ;; getting dest
                ff.store 5 x_65
                ;; end of store bucket
                continue
                end
                end
                ;; end of loop bucket
                ;; store bucket. Line 86
                ;; getting src
                ;; getting dest
                ff.store 7 1
                ;; end of store bucket
                ;; store bucket. Line 87
                ;; getting src
                ;; getting dest
                ff.store 5 0
                ;; end of store bucket
                ;; loop bucket. Line 87
                loop
                ;; compute bucket
                ;; load bucket
                x_66 = ff.load 5
                ;; end of load bucket
                ;; OP(LESSER)
                x_67 = ff.lt x_66 33
                ;; end of compute bucket
                if x_67
                ;; store bucket. Line 88
                ;; getting src
                ;; compute bucket
                ;; compute bucket
                ;; load bucket
                x_68 = ff.load 3
                ;; end of load bucket
                ;; load bucket
                x_69 = ff.load 5
                ;; end of load bucket
                ;; OP(SHIFT_R)
                x_70 = ff.shr x_68 x_69
                ;; end of compute bucket
                ;; OP(BITAND)
                x_71 = ff.band x_70 1
                ;; end of compute bucket
                ;; getting dest
                ;; compute bucket
                ;; compute bucket
                ;; compute bucket
                ;; load bucket
                x_72 = ff.load 5
                ;; end of load bucket
                ;; OP(TO_ADDRESS)
                x_73 = ff.wrap_i64 x_72
                ;; end of compute bucket
                ;; OP(MUL_ADDRESS)
                x_74 = i64.mul 1 x_73
                ;; end of compute bucket
                ;; OP(ADD_ADDRESS)
                x_75 = i64.add x_74 0
                ;; end of compute bucket
                set_signal x_75 x_71
                ;; end of store bucket
                ;; assert bucket
                ;; compute bucket
                ;; compute bucket
                ;; load bucket
                ;; compute bucket
                ;; compute bucket
                ;; compute bucket
                ;; load bucket
                x_77 = ff.load 5
                ;; end of load bucket
                ;; OP(TO_ADDRESS)
                x_78 = ff.wrap_i64 x_77
                ;; end of compute bucket
                ;; OP(MUL_ADDRESS)
                x_79 = i64.mul 1 x_78
                ;; end of compute bucket
                ;; OP(ADD_ADDRESS)
                x_80 = i64.add x_79 0
                ;; end of compute bucket
                x_76 = get_signal x_80
                ;; end of load bucket
                ;; compute bucket
                ;; load bucket
                ;; compute bucket
                ;; compute bucket
                ;; compute bucket
                ;; load bucket
                x_82 = ff.load 5
                ;; end of load bucket
                ;; OP(TO_ADDRESS)
                x_83 = ff.wrap_i64 x_82
                ;; end of compute bucket
                ;; OP(MUL_ADDRESS)
                x_84 = i64.mul 1 x_83
                ;; end of compute bucket
                ;; OP(ADD_ADDRESS)
                x_85 = i64.add x_84 0
                ;; end of compute bucket
                x_81 = get_signal x_85
                ;; end of load bucket
                ;; OP(SUB)
                x_86 = ff.sub x_81 1
                ;; end of compute bucket
                ;; OP(MUL)
                x_87 = ff.mul x_76 x_86
                ;; end of compute bucket
                ;; OP(EQ(Single(1)))
                x_88 = ff.eq x_87 0
                ;; end of compute bucket
                x_89 = ff.eqz x_88
                if x_89
                error 0
                end
                ;; end of assert bucket
                ;; store bucket. Line 93
                ;; getting src
                ;; compute bucket
                ;; load bucket
                x_90 = ff.load 4
                ;; end of load bucket
                ;; compute bucket
                ;; load bucket
                ;; compute bucket
                ;; compute bucket
                ;; compute bucket
                ;; load bucket
                x_92 = ff.load 5
                ;; end of load bucket
                ;; OP(TO_ADDRESS)
                x_93 = ff.wrap_i64 x_92
                ;; end of compute bucket
                ;; OP(MUL_ADDRESS)
                x_94 = i64.mul 1 x_93
                ;; end of compute bucket
                ;; OP(ADD_ADDRESS)
                x_95 = i64.add x_94 0
                ;; end of compute bucket
                x_91 = get_signal x_95
                ;; end of load bucket
                ;; load bucket
                x_96 = ff.load 7
                ;; end of load bucket
                ;; OP(MUL)
                x_97 = ff.mul x_91 x_96
                ;; end of compute bucket
                ;; OP(ADD)
                x_98 = ff.add x_90 x_97
                ;; end of compute bucket
                ;; getting dest
                ff.store 4 x_98
                ;; end of store bucket
                ;; store bucket. Line 95
                ;; getting src
                ;; compute bucket
                ;; load bucket
                x_99 = ff.load 7
                ;; end of load bucket
                ;; load bucket
                x_100 = ff.load 7
                ;; end of load bucket
                ;; OP(ADD)
                x_101 = ff.add x_99 x_100
                ;; end of compute bucket
                ;; getting dest
                ff.store 7 x_101
                ;; end of store bucket
                ;; store bucket. Line 87
                ;; getting src
                ;; compute bucket
                ;; load bucket
                x_102 = ff.load 5
                ;; end of load bucket
                ;; OP(ADD)
                x_103 = ff.add x_102 1
                ;; end of compute bucket
                ;; getting dest
                ff.store 5 x_103
                ;; end of store bucket
                continue
                end
                end
                ;; end of loop bucket
                ;; assert bucket
                ;; compute bucket
                ;; load bucket
                x_104 = ff.load 3
                ;; end of load bucket
                ;; load bucket
                x_105 = ff.load 4
                ;; end of load bucket
                ;; OP(EQ(Single(1)))
                x_106 = ff.eq x_104 x_105
                ;; end of compute bucket
                x_107 = ff.eqz x_106
                if x_107
                error 0
                end
                ;; end of assert bucket

                %%template Bits2Num_2 [ ff 1 32] [ ff 0 ] [33] [0]
                ;; store bucket. Line 0
                ;; getting src
                ;; getting dest
                ff.store 0 32
                ;; end of store bucket
                ;; store bucket. Line 58
                ;; getting src
                ;; getting dest
                ff.store 1 0
                ;; end of store bucket
                ;; store bucket. Line 60
                ;; getting src
                ;; getting dest
                ff.store 2 1
                ;; end of store bucket
                ;; store bucket. Line 61
                ;; getting src
                ;; getting dest
                ff.store 3 0
                ;; end of store bucket
                ;; loop bucket. Line 61
                loop
                ;; compute bucket
                ;; load bucket
                x_108 = ff.load 3
                ;; end of load bucket
                ;; OP(LESSER)
                x_109 = ff.lt x_108 32
                ;; end of compute bucket
                if x_109
                ;; store bucket. Line 62
                ;; getting src
                ;; compute bucket
                ;; load bucket
                x_110 = ff.load 1
                ;; end of load bucket
                ;; compute bucket
                ;; load bucket
                ;; compute bucket
                ;; compute bucket
                ;; compute bucket
                ;; load bucket
                x_112 = ff.load 3
                ;; end of load bucket
                ;; OP(TO_ADDRESS)
                x_113 = ff.wrap_i64 x_112
                ;; end of compute bucket
                ;; OP(MUL_ADDRESS)
                x_114 = i64.mul 1 x_113
                ;; end of compute bucket
                ;; OP(ADD_ADDRESS)
                x_115 = i64.add x_114 1
                ;; end of compute bucket
                x_111 = get_signal x_115
                ;; end of load bucket
                ;; load bucket
                x_116 = ff.load 2
                ;; end of load bucket
                ;; OP(MUL)
                x_117 = ff.mul x_111 x_116
                ;; end of compute bucket
                ;; OP(ADD)
                x_118 = ff.add x_110 x_117
                ;; end of compute bucket
                ;; getting dest
                ff.store 1 x_118
                ;; end of store bucket
                ;; store bucket. Line 63
                ;; getting src
                ;; compute bucket
                ;; load bucket
                x_119 = ff.load 2
                ;; end of load bucket
                ;; load bucket
                x_120 = ff.load 2
                ;; end of load bucket
                ;; OP(ADD)
                x_121 = ff.add x_119 x_120
                ;; end of compute bucket
                ;; getting dest
                ff.store 2 x_121
                ;; end of store bucket
                ;; store bucket. Line 61
                ;; getting src
                ;; compute bucket
                ;; load bucket
                x_122 = ff.load 3
                ;; end of load bucket
                ;; OP(ADD)
                x_123 = ff.add x_122 1
                ;; end of compute bucket
                ;; getting dest
                ff.store 3 x_123
                ;; end of store bucket
                continue
                end
                end
                ;; end of loop bucket
                ;; store bucket. Line 66
                ;; getting src
                ;; load bucket
                x_124 = ff.load 1
                ;; end of load bucket
                ;; getting dest
                set_signal 0 x_124
                ;; end of store bucket

                %%template A_3 [ ff 0  ff 0 ] [ ff 0 ] [3] [4]
                ;; store bucket. Line 11
                ;; getting src
                ;; getting dest
                ff.store 0 0
                ;; end of store bucket
                ;; store bucket. Line 18
                ;; getting src
                ;; load bucket
                x_125 = get_signal 1
                ;; end of load bucket
                ;; getting dest
                set_cmp_input_run 0 32 x_125
                ;; end of store bucket
                ;; store bucket. Line 19
                ;; getting src
                ;; load bucket
                x_126 = get_signal 2
                ;; end of load bucket
                ;; getting dest
                set_cmp_input_run 1 32 x_126
                ;; end of store bucket
                ;; store bucket. Line 21
                ;; getting src
                ;; getting dest
                ff.store 0 0
                ;; end of store bucket
                ;; loop bucket. Line 21
                loop
                ;; compute bucket
                ;; load bucket
                x_127 = ff.load 0
                ;; end of load bucket
                ;; OP(LESSER)
                x_128 = ff.lt x_127 32
                ;; end of compute bucket
                if x_128
                ;; store bucket. Line 22
                ;; getting src
                ;; load bucket
                ;; compute bucket
                ;; compute bucket
                ;; compute bucket
                ;; load bucket
                x_130 = ff.load 0
                ;; end of load bucket
                ;; OP(TO_ADDRESS)
                x_131 = ff.wrap_i64 x_130
                ;; end of compute bucket
                ;; OP(MUL_ADDRESS)
                x_132 = i64.mul 1 x_131
                ;; end of compute bucket
                ;; OP(ADD_ADDRESS)
                x_133 = i64.add x_132 0
                ;; end of compute bucket
                ;; is subcomponent
                x_129 = get_cmp_signal 0 x_133
                ;; end of load bucket
                ;; getting dest
                ;; compute bucket
                ;; compute bucket
                ;; compute bucket
                ;; compute bucket
                ;; load bucket
                x_134 = ff.load 0
                ;; end of load bucket
                ;; OP(TO_ADDRESS)
                x_135 = ff.wrap_i64 x_134
                ;; end of compute bucket
                ;; OP(MUL_ADDRESS)
                x_136 = i64.mul 1 x_135
                ;; end of compute bucket
                ;; OP(ADD_ADDRESS)
                x_137 = i64.add 0 x_136
                ;; end of compute bucket
                ;; OP(ADD_ADDRESS)
                x_138 = i64.add x_137 33
                ;; end of compute bucket
                set_cmp_input_cnt 2 x_138 x_129
                ;; end of store bucket
                ;; store bucket. Line 23
                ;; getting src
                ;; load bucket
                ;; compute bucket
                ;; compute bucket
                ;; compute bucket
                ;; load bucket
                x_140 = ff.load 0
                ;; end of load bucket
                ;; OP(TO_ADDRESS)
                x_141 = ff.wrap_i64 x_140
                ;; end of compute bucket
                ;; OP(MUL_ADDRESS)
                x_142 = i64.mul 1 x_141
                ;; end of compute bucket
                ;; OP(ADD_ADDRESS)
                x_143 = i64.add x_142 0
                ;; end of compute bucket
                ;; is subcomponent
                x_139 = get_cmp_signal 1 x_143
                ;; end of load bucket
                ;; getting dest
                ;; compute bucket
                ;; compute bucket
                ;; compute bucket
                ;; compute bucket
                ;; load bucket
                x_144 = ff.load 0
                ;; end of load bucket
                ;; OP(TO_ADDRESS)
                x_145 = ff.wrap_i64 x_144
                ;; end of compute bucket
                ;; OP(MUL_ADDRESS)
                x_146 = i64.mul 1 x_145
                ;; end of compute bucket
                ;; OP(ADD_ADDRESS)
                x_147 = i64.add 32 x_146
                ;; end of compute bucket
                ;; OP(ADD_ADDRESS)
                x_148 = i64.add x_147 33
                ;; end of compute bucket
                set_cmp_input_cnt_check 2 x_148 x_139
                ;; end of store bucket
                ;; store bucket. Line 21
                ;; getting src
                ;; compute bucket
                ;; load bucket
                x_149 = ff.load 0
                ;; end of load bucket
                ;; OP(ADD)
                x_150 = ff.add x_149 1
                ;; end of compute bucket
                ;; getting dest
                ff.store 0 x_150
                ;; end of store bucket
                continue
                end
                end
                ;; end of loop bucket
                ;; store bucket. Line 26
                ;; getting src
                ;; getting dest
                ff.store 0 0
                ;; end of store bucket
                ;; loop bucket. Line 26
                loop
                ;; compute bucket
                ;; load bucket
                x_151 = ff.load 0
                ;; end of load bucket
                ;; OP(LESSER)
                x_152 = ff.lt x_151 32
                ;; end of compute bucket
                if x_152
                ;; store bucket. Line 27
                ;; getting src
                ;; load bucket
                ;; compute bucket
                ;; compute bucket
                ;; compute bucket
                ;; load bucket
                x_154 = ff.load 0
                ;; end of load bucket
                ;; OP(TO_ADDRESS)
                x_155 = ff.wrap_i64 x_154
                ;; end of compute bucket
                ;; OP(MUL_ADDRESS)
                x_156 = i64.mul 1 x_155
                ;; end of compute bucket
                ;; OP(ADD_ADDRESS)
                x_157 = i64.add x_156 0
                ;; end of compute bucket
                ;; is subcomponent
                x_153 = get_cmp_signal 2 x_157
                ;; end of load bucket
                ;; getting dest
                ;; compute bucket
                ;; compute bucket
                ;; compute bucket
                ;; load bucket
                x_158 = ff.load 0
                ;; end of load bucket
                ;; OP(TO_ADDRESS)
                x_159 = ff.wrap_i64 x_158
                ;; end of compute bucket
                ;; OP(MUL_ADDRESS)
                x_160 = i64.mul 1 x_159
                ;; end of compute bucket
                ;; OP(ADD_ADDRESS)
                x_161 = i64.add x_160 1
                ;; end of compute bucket
                set_cmp_input_cnt_check 3 x_161 x_153
                ;; end of store bucket
                ;; store bucket. Line 26
                ;; getting src
                ;; compute bucket
                ;; load bucket
                x_162 = ff.load 0
                ;; end of load bucket
                ;; OP(ADD)
                x_163 = ff.add x_162 1
                ;; end of compute bucket
                ;; getting dest
                ff.store 0 x_163
                ;; end of store bucket
                continue
                end
                end
                ;; end of loop bucket
                ;; store bucket. Line 30
                ;; getting src
                ;; load bucket
                ;; is subcomponent
                x_164 = get_cmp_signal 3 0
                ;; end of load bucket
                ;; getting dest
                set_signal 0 x_164
                ;; end of store bucket
                ";
        let res = parse_program(input);
        println!("{:?}", res);
        assert!(res.is_ok());
    }
}
