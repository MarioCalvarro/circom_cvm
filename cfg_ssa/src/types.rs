
#[derive(Debug, Clone, PartialEq)]
pub enum NumericType {
    Integer,
    FiniteField,
}

#[derive(Debug, Clone, PartialEq)]
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

    //Functions
    Return,
    Call,

    //Misc
    Error,
    //TODO: What is outs ??
}

//TODO: change to expression
#[derive(Debug, Clone, PartialEq)]
pub enum Parameter {
    Signal {
        index: i64,
        size: i64,
    },
    SubcmpSignal {
        component: String,
        index: i64,
        size: i64,
    },
    I64Memory {
        index: i64,
        size: i64,
    },
    FfMemory {
        index: i64,
        size: i64,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expression {
    Constant(i64),
    Variable(String),
    Parameter(Parameter),
}