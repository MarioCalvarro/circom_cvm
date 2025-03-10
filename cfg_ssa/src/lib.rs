use std::collections::HashSet;

type Stack<T> = Vec<T>;

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
    //TODO: implement the construction of these sets
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

enum EndType {
    Conditional,
    //We save the location of the loop instruction to be able to come back later
    Loop(usize),
}

#[derive(Default)]
pub struct CFG {
    entry: usize,
    blocks: Vec<BasicBlock>,
    adjacency: Vec<Vec<Edge>>,
    curr: usize,
    stack_ends: Stack<EndType>,
}

impl CFG {
    pub fn new(entry: usize) -> Self {
        Self {
            entry,
            blocks: Vec::new(),
            adjacency: Vec::new(),
            //Block 0 is reserved for initial instructions
            curr: 1,
            stack_ends: Stack::new(),
        }
    }

    pub fn add_block(&mut self) {
        self.blocks.push(BasicBlock::new(self.adjacency.len()));
        self.adjacency.push(Vec::new());
    }

    fn add_edge(&mut self, from: usize, to: usize, edge_type: EdgeType) {
        let edge = Edge::new(from, to, edge_type);
        if let Some(edges) = self.adjacency.get_mut(from) {
            edges.push(edge);
        }
    }

    pub fn add_loop_blocks(&mut self) {
        //The location of the loop is a new block
        self.stack_ends.push(EndType::Loop(self.adjacency.len()));

        //Block with only loop instruction
        self.blocks.push(BasicBlock::new(self.adjacency.len()));
        self.adjacency.push(Vec::new());
        self.add_edge(self.curr, self.adjacency.len() - 1, EdgeType::Unconditional);
        self.add_instruction_to_current_block(Instruction::Loop);

        //Block with the code inside the loop
        self.blocks.push(BasicBlock::new(self.adjacency.len()));
        self.adjacency.push(Vec::new());
        self.add_edge(self.adjacency.len() - 2, self.adjacency.len() - 1, EdgeType::Unconditional);
        self.curr = self.adjacency.len() - 1;
    }

    pub fn add_if_block(&mut self) {
        self.stack_ends.push(EndType::Conditional);
        
        //Block with the code of the conditional
        self.blocks.push(BasicBlock::new(self.adjacency.len()));
        self.adjacency.push(Vec::new());
        self.add_edge(self.curr, self.adjacency.len() - 1, EdgeType::Unconditional);
        self.curr = self.adjacency.len() - 1;
    }

    pub fn add_edge_continue(&mut self) {
        let mut aux_stack: Stack<EndType> = Stack::new();
        while let Some(EndType::Conditional) = self.stack_ends.pop() {
            aux_stack.push(EndType::Conditional);
        }
        if let Some(EndType::Loop(loop_block)) = self.stack_ends.pop() {
            self.add_edge(self.curr, loop_block, EdgeType::Unconditional)
        }
        else {
            //TODO: Improve error handling
            panic!("continue instruction without expected loop");
        }
        //Restore the stack
        while let Some(end) = aux_stack.pop() {
            self.stack_ends.push(end);
        }
    }

    pub fn add_initial_instruction(&mut self, ins: Instruction) {
        self.blocks[0].add_instruction(ins);
    }

    pub fn add_instruction_to_current_block(&mut self, ins: Instruction) {
        self.blocks[self.curr].add_instruction(ins);
    }
}
