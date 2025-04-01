use std::collections::HashSet;

pub mod ast;
pub mod types;
mod type_checking;

use crate::types::*;

#[derive(Debug, Clone)]
pub struct Statement {
    num_type: Option<NumericType>,
    operator: Operator,
    output: Option<String>,
    operands: Vec<Expression>,
}

#[derive(Debug, Clone)]
pub enum Successor {
    Unconditional {
        to: usize,
    },
    Conditional {
        condition: Expression,
        to_then: usize,
        to_else: Option<usize>,
    },
}

pub struct BasicBlock {
    id: usize,
    instructions: Vec<Statement>,
    predecessors: HashSet<usize>,
    successors: Option<Successor>,
}

impl BasicBlock {
    pub fn new(id: usize) -> Self {
        Self {
            id,
            instructions: Vec::new(),
            predecessors: HashSet::new(),
            successors: None,
        }
    }

    pub fn add_instruction(&mut self, instruction: Statement) {
        self.instructions.push(instruction);
    }

    pub fn add_predecessor(&mut self, pred: usize) {
        self.predecessors.insert(pred);
    }
}

#[derive(Default)]
pub struct CFG {
    entry: usize,
    blocks: Vec<BasicBlock>,
}

impl CFG {
    pub fn new(entry: usize) -> Self {
        Self {
            entry,
            blocks: Vec::new(),
        }
    }
}
