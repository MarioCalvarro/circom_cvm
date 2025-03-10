use std::collections::HashSet;

#[derive(Clone)]
pub enum NumericType {
    Integer,
    FiniteField,
}

#[derive(Clone)]
pub enum ComponentMode {
    Implicit,
    Explicit,
}

#[derive(Clone)]
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

    //Signal
    GetSignal,
    GetCmpSignal,
    SetSignal,
    SetCmpSignal,
    SetCmpIn,
    SetCmpInCnt,
    SetCmpInRun,
    SetCmpInCntCheck,
}

#[derive(Clone)]
pub enum Operand {
    Constant(i64),
    Variable(String),
}

#[derive(Clone)]
pub enum Expression {
    Atomic(Operand),
    //The last operand is optional because some operations do not have a second operand
    Operation(NumericType, Operator, Operand, Option<Operand>),
    SignalOperation(Operator, Operand, Option<Operand>, Option<Operand>),
}

#[derive(Clone)]
pub enum Instruction {
    //If the first String, the variable, is None, the return value of the expresion is not saved
    Assignment(String, Expression),
    Expr(Expression),
    Loop,
    Break,
    Continue,
    If(Expression),
    Else,
    End,
    Return,
    Error(i64),   //TODO: Possible error codes?
    //TODO: Cambiar Intrucciones iniciales?
    Prime(String),
    Signals(usize),
    Heap(usize),
    Start(String),
    Components(ComponentMode),
    Witness(Vec<usize>),
    Template(Vec<String>),
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

impl Edge {
    pub fn new(from: usize, to: usize, edge_type: EdgeType) -> Self {
        Self { from, to, edge_type }
    }
}

#[derive(Default)]
pub struct BasicBlock {
    id: usize,
    instructions: Vec<Instruction>,
    predecessors: HashSet<usize>,
    successors: HashSet<usize>,
    is_exit: bool,
}

impl BasicBlock {
    pub fn new(id: usize) -> Self {
        Self {
            id,
            instructions: Vec::new(),
            predecessors: HashSet::new(),
            successors: HashSet::new(),
            is_exit: false,
        }
    }

    pub fn add_instruction(&mut self, instruction: Instruction) {
        self.instructions.push(instruction);
    }

    pub fn add_predecessor(&mut self, pred: usize) {
        self.predecessors.insert(pred);
    }

    pub fn add_successor(&mut self, succ: usize) {
        self.successors.insert(succ);
    }

    pub fn set_exit(&mut self, is_exit: bool) {
        self.is_exit = is_exit;
    }
}

#[derive(Default)]
pub struct CFG {
    entry: usize,
    blocks: Vec<BasicBlock>,
    adjacency: Vec<Vec<Edge>>,
}

impl CFG {
    pub fn new(entry: usize) -> Self {
        Self {
            entry,
            blocks: Vec::new(),
            adjacency: Vec::new(),
        }
    }

    pub fn add_block(&mut self) {
        self.blocks.push(BasicBlock::new(self.adjacency.len()));
        self.adjacency.push(Vec::new());
    }

    pub fn add_edge(&mut self, from: usize, to: usize, edge_type: EdgeType) {
        let edge = Edge::new(from, to, edge_type);
        if let Some(edges) = self.adjacency.get_mut(from) {
            edges.push(edge);
        }
    }

    pub fn add_initial_instruction(&mut self, ins: Instruction) {
        self.blocks[0].add_instruction(ins);
    }
}
