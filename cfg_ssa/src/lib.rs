use std::collections::HashSet;

//TODO: Change string for another more explicit type?
type OperandID = String;

pub enum NumericType {
    Integer,
    FiniteField,
}

pub enum Operator {
    //Arithmetic
    Add,
    Sub,
    Mul,
    Div,
    Rem,
    IDiv,       //Only for ff
    Pow,

    //Relational
    Greater,
    GreaterEqual,
    Less,
    LessEqual,
    Equal,
    NotEqual,
    EqualZero,

    //Boolean
    And,
    Or,

    //Bit operations
    ShiftRight,
    ShiftLeft,
    BitAnd,
    BitOr,
    BitXor,
    BitNot,

    //Conversions
    Extend,     //i64 → ff
    Wrap,       //ff → i64
    
    //Memory
    Load,
    Store,
}

pub enum Expression {
    Constant(i64),
    Variable(String),
    //The last operand is optional because some operations do not have a second operand
    Operation(NumericType, Operator, OperandID, Option<OperandID>),
    SignalOperation(Operator, OperandID, Option<OperandID>, Option<OperandID>),
}

pub enum Instruction {
    //If the first String, the variable, is None, the return value of the expresion is not saved
    Assignment(Option<String>, Expression),
    Loop,
    Break,
    Continue,
    If(Expression),
    Else,
    End,
    Return,
    Error(i64),   //TODO: Possible error codes?
}

#[derive(Default)]
pub enum EdgeType {
    #[default]
    Unconditional,
    Conditional,
    LoopBack,
}

#[derive(Default)]
pub struct Edge {
    from: usize,
    to: usize,
    edge_type: EdgeType,
}

#[derive(Default)]
pub struct BasicBlock {
    id: usize,
    instructions: Vec<Instruction>,
    //TODO: Necessary?
    predecessors: HashSet<usize>,
    successors: HashSet<usize>,
    is_exit: bool,
}

#[derive(Default)]
pub struct CFG {
    entry: usize,
    blocks: Vec<BasicBlock>,
    adjacency: Vec<Vec<Edge>>,
}
