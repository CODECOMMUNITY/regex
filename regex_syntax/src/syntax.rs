#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Expr {
    Empty,
    Literal { c: char, casei: bool },
    LiteralString { s: Vec<char>, casei: bool },
    AnyChar,
    AnyCharNoNL,
    Class { ranges: CharClass, casei: bool },
    StartLine,
    EndLine,
    StartText,
    EndText,
    WordBoundary,
    NotWordBoundary,
    Capture { e: Box<Expr>, i: CaptureIndex, name: CaptureName },
    Repeat { e: Box<Expr>, r: Repeat, greedy: bool },
    Concat(Vec<Expr>),
    Alternate(Vec<Expr>),
}

pub type CaptureIndex = u32;

pub type CaptureName = Option<String>;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Repeat {
    ZeroOrOne,  // ?
    ZeroOrMore, // *
    OneOrMore,  // +
    // invariant: at least one of `start` or `end` must be `Some(_)`.
    Range { start: Option<u32>, end: Option<u32> },
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct CharClass(Vec<ClassRange>);

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ClassRange {
    // invariant: start < end
    pub start: char,
    pub end: char,
}
