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
pub enum Statement {
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
    instructions: Vec<Statement>,
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

    pub fn add_instruction(&mut self, instruction: Statement) {
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
    //We save the location of the block with the if instruction to create the edge to the else
    //block later and the location of the merge block to point the conditional branches to it at
    //the end.
    Conditional(usize, usize),
    //We save the location of the loop instruction to be able to come back at the end (or the
    //continues) and the location of the block after the loop (because of the breaks)
    Loop(usize, usize),
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

    //Basic functions
    pub fn add_block(&mut self) {
        self.blocks.push(BasicBlock::new(self.blocks.len()));
        self.adjacency.push(Vec::new());
    }

    fn add_edge(&mut self, from: usize, to: usize, edge_type: EdgeType) {
        let edge = Edge::new(from, to, edge_type);
        if let Some(edges) = self.adjacency.get_mut(from) {
            edges.push(edge);
        }
    }

    pub fn add_initial_instruction(&mut self, ins: Statement) {
        self.blocks[0].add_instruction(ins);
    }

    pub fn add_instruction_to_current_block(&mut self, ins: Statement) {
        self.blocks[self.curr].add_instruction(ins);
    }


    //Functions to construct the cfg properly
    pub fn add_loop_blocks_and_instruction(&mut self) {
        //Block with only loop instruction
        self.add_block();
        self.add_edge(self.curr, self.blocks.len() - 1, EdgeType::Unconditional);
        self.curr = self.blocks.len() - 1;
        self.add_instruction_to_current_block(Statement::Loop);

        //The location of block after the loop is new
        self.add_block();
        self.stack_ends.push(EndType::Loop(self.curr, self.blocks.len() - 1));

        //Block with the code inside the loop
        self.add_block();
        self.add_edge(self.curr, self.blocks.len() - 1, EdgeType::Unconditional);
        self.curr = self.blocks.len() - 1;
    }

    pub fn add_if_block(&mut self) {
        //Save the block of the if and the merge block
        self.stack_ends.push(EndType::Conditional(self.curr, self.blocks.len()));

        //Block that merges the branches of the conditional
        self.add_block();

        //Block with the code of the conditional
        self.add_block();
        self.add_edge(self.curr, self.blocks.len() - 1, EdgeType::Conditional);
        self.curr = self.blocks.len() - 1;
    }

    pub fn add_else_block(&mut self) {
        //New block with the else branch
        self.add_block();

        //Get the information of the if block
        if let Some(EndType::Conditional(if_block, merge_block)) = self.stack_ends.last() {
            let merge_block = *merge_block;
            let if_block = *if_block;

            //Link the last block of the if branch to the merge block
            self.add_edge(self.curr, merge_block, EdgeType::Unconditional);

            //Link the if block to the else block
            self.add_edge(if_block, self.blocks.len() - 1, EdgeType::Unconditional);

            //Restore the stack
            self.stack_ends.push(EndType::Conditional(if_block, merge_block));
        }
        else {
            //TODO: Improve error handling
            panic!("an else instruction needs to be between an if and an end instructions");
        }
        self.curr = self.blocks.len() - 1;
    }

    pub fn add_edge_break(&mut self) {
        let mut aux_stack: Stack<EndType> = Stack::new();
        while matches!(self.stack_ends.last(), Some(EndType::Conditional(_, _))) {
            if let Some(EndType::Conditional(a, b)) = self.stack_ends.pop() {
                aux_stack.push(EndType::Conditional(a, b));
            }
        }
        if let Some(EndType::Loop(_, end_block)) = self.stack_ends.pop() {
            self.add_edge(self.curr, end_block, EdgeType::Unconditional)
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

    pub fn add_edge_continue(&mut self) {
        let mut aux_stack: Stack<EndType> = Stack::new();
        while matches!(self.stack_ends.last(), Some(EndType::Conditional(_, _))) {
            if let Some(EndType::Conditional(a, b)) = self.stack_ends.pop() {
                aux_stack.push(EndType::Conditional(a, b));
            }
        }
        if let Some(EndType::Loop(loop_block, _)) = self.stack_ends.pop() {
            self.add_edge(self.curr, loop_block, EdgeType::LoopBack)
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

    pub fn add_edge_end(&mut self) {
        match self.stack_ends.pop() {
            Some(EndType::Conditional(_, merge_block)) => {
                self.add_edge(self.curr, merge_block, EdgeType::Unconditional);
            },
            //TODO: change if the behavior is to go to next block instead of doing another
            //iteration
            Some(EndType::Loop(loop_block, _)) => {
                self.add_edge(self.curr, loop_block, EdgeType::LoopBack);
            },
            _ => {
                panic!("each end must go after an if or a loop")
            },
        }
    }
}
