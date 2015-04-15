pub enum Expr {
    Empty,
    Literal { c: char, casei: bool },
    LiteralString { s: String, casei: bool },
    AnyChar,
    AnyCharNoNL,
    Class { ranges: Vec<ClassRange>, casei: bool },
    StartLine,
    EndLine,
    StartText,
    EndText,
    WordBoundary,
    NotWordBoundary,
    Capture { e: Box<Expr>, i: u32, name: Option<String> },
    Repeat { e: Box<Expr>, r: ExprRepeat, greedy: bool },
    Concat(Vec<Expr>),
    Alternate(Vec<Expr>),
}

pub enum ExprRepeat {
    ZeroOrOne,  // ?
    ZeroOrMore, // *
    OneOrMore,  // +
    // invariant: at least one of `start` or `end` must be `Some(_)`.
    Range { start: Option<u32>, end: Option<u32> },
}

pub struct ClassRange {
    // invariant: start < end
    start: char,
    end: char,
}
