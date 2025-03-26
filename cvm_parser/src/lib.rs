extern crate num_bigint_dig as num_bigint;

use nom::{
    branch::alt, bytes::{complete::take_until, tag}, character::{complete::{alpha1, alphanumeric0, alphanumeric1, i64, line_ending, multispace1, none_of, satisfy, space0, usize}, streaming::space1}, combinator::{complete, map, opt, recognize, value}, multi::{many0, separated_list0}, sequence::{delimited, pair, preceded, separated_pair}, IResult, Parser
};
use cfg_ssa::types::*;
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
    delimited(
     parse_useless,
    alt((parse_operation, parse_if_then_else, parse_loop, parse_break, parse_continue)),
     parse_useless)
    .parse(input)
}

fn parse_template(input: &str) -> IResult<&str, Template> {
    let (input, _) = tag("%%template").parse(input)?;
    let (input, _) = space1(input)?;

    let (input, name) = parse_variable_name(input)?;
    let (input, _) = space1(input)?;

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

    let (input, templates) = many0(parse_template).parse(input)?;
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
        let input = "loop\n  ;; comment\n  end\n";
        assert!(parse_loop(input).is_ok());
    }

    #[test]
    fn test_parse_if_then_else() {
        let input = "if condition\n  ;; comment\n  else\n  ;; comment\n  end\n";
        assert!(parse_if_then_else(input).is_ok());
    }

    #[test]
    fn test_parse_template() {
        let input = "%%template template_name [input1 input2] [output1] [10] [5]\n  ;; body\n  end\n";
        let res = parse_template(input);
        assert!(res.is_ok());
    }

    #[test]
    fn test_parse_program() {
        let input = ";; comment\nfield\n;; comment\nsignals_memory\n;; comment\ncomponents_heap\n;; comment\nstart\n;; comment\ncomponents\n;; comment\nwitness\n;; comment\n%%template template_name [input1 input2] [output1] [10] [5]\n  ;; body\n  end\n";
        assert!(parse_program(input).is_ok());
    }
}
