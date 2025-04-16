extern crate num_bigint_dig as num_bigint;
use num_bigint::BigInt;

#[derive(Debug, Clone, PartialEq)]
pub enum NumericType {
    Integer,
    FiniteField,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    //TODO: Change in the future, identifiers are not i64 (cannot be used to sum)
    Variable(NumericType),
    Function(Vec<NumericType>, Vec<NumericType>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Operator {
    //TODO: There are more operations
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
     //Stores for ff only
    MStore,
    MStoreFromSignal,
    MStoreFromCmpSignal,

    //Signal
    GetSignal,
    GetCmpSignal,
    SetSignal,
    MSetSignal,
    MSetSignalFromMemory,

    SetCmpIn,
    SetCmpInCnt,
    SetCmpInRun,
    SetCmpInCntCheck,

    MSetCmpIn,
    MSetCmpInCnt,
    MSetCmpInRun,
    MSetCmpInCntCheck,

    MSetCmpInFromCmp,
    MSetCmpInFromCmpCnt,
    MSetCmpInFromCmpRun,
    MSetCmpInFromCmpCntCheck,

    MSetCmpInFromMemory,
    MSetCmpInFromMemoryCnt,
    MSetCmpInFromMemoryRun,
    MSetCmpInFromMemoryCntCheck,

    //Functions
    Return,
    Call,

    //Templates
    GetTemplateId,
    GetTemplateSignalPosition,
    GetTemplateSignalSize,
    GetTemplateSignalDim,
    GetTemplateSignalType,

    //Buses
    GetBusSignalPos,
    GetBusSignalSize,
    GetBusSignalDim,
    GetBusSignalType,

    //Misc
    Error,
    //TODO: outs
}

#[derive(Debug, Clone, PartialEq)]
pub enum ConstantType {
    FF(BigInt),
    I64(i64),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Atomic {
    Constant(ConstantType),
    Variable(String),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Parameter {
    //TODO: Not index is number of dimensions and length of each
    //Change second to vector of size first
    Signal {
        index: Atomic,
        size: Atomic,
    },
    SubcmpSignal {
        component: Atomic,
        index: Atomic,
        size: Atomic,
    },
    I64Memory {
        index: Atomic,
        size: Atomic,
    },
    FfMemory {
        index: Atomic,
        size: Atomic,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expression {
    Atomic(Atomic),
    Parameter(Parameter),
}
