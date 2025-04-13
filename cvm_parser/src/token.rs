use cfg_ssa::ast::ComponentCreationMode;
use num_bigint::BigInt;
use nom::Needed;
use nom::Input;
use std::{ops::{Range, RangeFrom}, slice::Iter};

#[derive(PartialEq, Debug, Clone)]
pub enum Token<'a> {
    //Initial words
    Prime(BigInt),
    Signals(usize),
    CompHeap(usize),
    Start(&'a str),
    Witness(Vec<usize>),
    Function,
    Template,

    //Component creation mode
    CompMode(ComponentCreationMode),

    //Special Characters
    LBracket,
    RBracket,
    Dot,

    //Instruction Keywords
    If,
    Else,
    Loop,
    End,
    Break,
    Continue,

    //Identifier
    Ident(&'a str),

    // Number container (the string is later converted to a number type (i64 or BigInt))
    Number(&'a str),

    //Numeric Types
    Integer,
    FiniteField,

    //Equal Symbol
    Eq,

    //Operators
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
    SetCmpIn,
    SetCmpInCnt,
    SetCmpInRun,
    SetCmpInCntCheck,

    //Functions
    Return,
    Call,

    //Templates
    GetTemplateId,
    GetTemplateSignalPosition,
    GetTemplateSignalSize,

    //Misc
    Error,
    //TODO: outs
    
    //Parameters
    Signal,
    SubcmpSignal,
    Memory,
}

#[derive(Debug, Clone)]
pub struct Tokens<'a> {
    list_tokens: &'a [Token<'a>],
    start: usize,
    end: usize,
}

impl<'a> Tokens<'a> {
    // Returns the current view of the token slice based on start and end.
    fn as_slice(&self) -> &'a [Token<'a>] {
        &self.list_tokens[self.start..self.end]
    }
}

impl<'a> Input for Tokens<'a> {
    type Item = &'a Token<'a>;
    type Iter = std::slice::Iter<'a, Token<'a>>;
    type IterIndices = std::iter::Enumerate<std::slice::Iter<'a, Token<'a>>>;

    fn input_len(&self) -> usize {
        self.end - self.start
    }

    // Returns a new `Tokens` view with the first `count` tokens
    fn take(&self, index: usize) -> Self {
        if index > self.input_len() {
            panic!("Index out of bounds in take()");
        }
        let new_end = self.start + index;
        Tokens {
            list_tokens: self.list_tokens,
            start: self.start,
            end: new_end,
        }
    }

    // Returns a new `Tokens` view skipping the first `count` tokens
    fn take_from(&self, index: usize) -> Self {
        if index > self.input_len() {
            panic!("Index out of bounds in take_from()");
        }
        let new_start = self.start + index;
        Tokens {
            list_tokens: self.list_tokens,
            start: new_start,
            end: self.end,
        }
    }

    // Splits the current view at `count`, returning (rest, prefix)
    fn take_split(&self, index: usize) -> (Self, Self) {
        if index > self.input_len() {
            panic!("Index out of bounds in take_split()");
        }
        let mid = self.start + index;
        let first = Tokens {
            list_tokens: self.list_tokens,
            start: self.start,
            end: mid,
        };
        let second = Tokens {
            list_tokens: self.list_tokens,
            start: mid,
            end: self.end,
        };
        (first, second)
    }

    // Returns the byte position of the first element that satisfies the predicate
    fn position<P>(&self, pred: P) -> Option<usize>
    where
        P: Fn(Self::Item) -> bool,
    {
        self.as_slice().iter().position(pred).map(|idx| self.start + idx)
    }

    // Returns an iterator over the tokens in the current slice
    fn iter_elements(&self) -> Self::Iter {
        self.as_slice().iter()
    }

    // Returns an iterator that yields (index, token) pairs for the current slice
    fn iter_indices(&self) -> Self::IterIndices {
        self.as_slice().iter().enumerate()
    }

    // Returns the byte offset from the element’s position in the stream
    fn slice_index(&self, count: usize) -> Result<usize, Needed> {
        if self.input_len() < count {
            Err(Needed::new(count - self.input_len()))
        } else {
            Ok(count)
        }
    }
}
