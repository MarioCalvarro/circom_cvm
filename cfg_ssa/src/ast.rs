extern crate num_bigint_dig as num_bigint;

use num_bigint::BigInt;
use crate::types::*;

#[derive(Debug, Clone, PartialEq)]
pub enum ComponentCreationMode {
    Implicit,
    Explicit,
}

#[derive(Debug, Clone, PartialEq)]
pub struct AST {
    pub field: BigInt,
    pub signals_memory: usize,
    pub components_heap: usize,
    pub main_template: String,
    pub components_creation_mode: ComponentCreationMode,
    pub witness: Vec<usize>,
    pub templates: Vec<Template>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Template {
    pub name: String,
    //TODO: What information do we need about the templates?
    pub inputs: Vec<String>,
    pub outputs: Vec<String>,
    pub signals: usize,
    pub components: usize,
    pub body: Vec<Box<ASTNode>>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ASTNode {
    Operation {
        num_type: Option<NumericType>,
        operator: Operator,
        output: Option<String>,
        operands: Vec<Expression>,
    },
    IfThenElse {
        condition: Expression,
        if_case: Vec<Box<ASTNode>>,
        else_case: Option<Vec<Box<ASTNode>>>,
    },
    Loop {
        body: Vec<Box<ASTNode>>,
    },
    Break,
    Continue,
}