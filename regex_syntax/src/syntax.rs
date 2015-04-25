use std::fmt;
use std::ops::{Deref, DerefMut};

use self::Expr::*;
use self::Repeater::*;

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
    Group { e: Box<Expr>, i: CaptureIndex, name: CaptureName },
    Repeat { e: Box<Expr>, r: Repeater, greedy: bool },
    Concat(Vec<Expr>),
    Alternate(Vec<Expr>),
}

pub type CaptureIndex = Option<u32>;

pub type CaptureName = Option<String>;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Repeater {
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

impl Expr {
    pub fn can_repeat(&self) -> bool {
        match *self {
            Literal { .. }
            | LiteralString { .. }
            | AnyChar
            | AnyCharNoNL
            | Class { .. }
            | Group { .. }
            => true,
            _ => false,
        }
    }

    pub fn concat(self, new: Expr) -> Expr {
        fn concat(e1: Expr, e2: Expr) -> Expr {
            match (e1, e2) {
                (Concat(mut es1), Concat(es2)) => {
                    es1.extend(es2);
                    Concat(es1)
                }
                (Concat(mut es1), e2) => { es1.push(e2); Concat(es1) }
                (e1, Concat(mut es2)) => { es2.insert(0, e1); Concat(es2) }
                (e1, e2) => Concat(vec![e1, e2]),
            }
        }
        match (self, new) {
            (Empty, e) | (e, Empty) => e,
            (old @ Literal{..}, e) => concat(old, e),
            (old @ LiteralString{..}, e) => concat(old, e),
            (AnyChar, e) => concat(AnyChar, e),
            (AnyCharNoNL, e) => concat(AnyCharNoNL, e),
            (old @ Class{..}, e) => concat(old, e),
            (StartLine, e) => concat(StartLine, e),
            (EndLine, e) => concat(EndLine, e),
            (StartText, e) => concat(StartText, e),
            (EndText, e) => concat(EndText, e),
            (WordBoundary, e) => concat(WordBoundary, e),
            (NotWordBoundary, e) => concat(NotWordBoundary, e),
            (old @ Group{..}, e) => concat(old, e),
            (old @ Repeat{..}, e) => concat(old, e),
            (old @ Concat(_), e) => concat(old, e),
            (Alternate(mut es), e) => {
                let last = es.pop().expect("at least one alternate");
                es.push(last.concat(e));
                Alternate(es)
            }
        }
    }

    // Literal simplification pieces. ---AG
    /*(Literal { c: oc, casei: ocasei },
     Literal { c: nc, casei: ncasei }) => {
        if ocasei == ncasei {
            LiteralString { s: vec![oc, nc], casei: ocasei }
        } else {
            Concat(vec![Literal { c: oc, casei: ocasei },
                        Literal { c: nc, casei: ncasei }])
        }
    }
    (LiteralString { s: mut os, casei: ocasei },
     LiteralString { s: ns, casei: ncasei }) => {
        if ocasei == ncasei {
            os.extend(ns);
            LiteralString { s: os, casei: ocasei }
        } else {
            concat(LiteralString { s: os, casei: ocasei },
                   LiteralString { s: ns, casei: ncasei })
        }
    }
    (LiteralString { s: mut os, casei: ocasei },
     Literal { c: nc, casei: ncasei }) => {
        if ocasei == ncasei {
            os.push(nc);
            LiteralString { s: os, casei: ocasei }
        } else {
            concat(LiteralString { s: os, casei: ocasei },
                   Literal { c: nc, casei: ncasei })
        }
    }
    (Literal { c: oc, casei: ocasei },
     LiteralString { s: mut ns, casei: ncasei }) => {
        if ocasei == ncasei {
            ns.insert(0, oc);
            LiteralString { s: ns, casei: ocasei }
        } else {
            concat(Literal { c: oc, casei: ocasei },
                   LiteralString { s: ns, casei: ncasei })
        }
    }*/
}

impl Deref for CharClass {
    type Target = Vec<ClassRange>;
    fn deref(&self) -> &Vec<ClassRange> { &self.0 }
}

impl DerefMut for CharClass {
    fn deref_mut(&mut self) -> &mut Vec<ClassRange> { &mut self.0 }
}

// TODO(burntsushi): Display logic doesn't include escaping.
//
// N.B. Since this is printing in a "canonical" format, the `U` flag is never
// used. Additionally, other flags may be overused as no effort is taken to
// globally minimize the number of flags used.

// TODO(burntsushi): Write tests.

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Empty => write!(f, ""),
            Literal { c, casei: false } => write!(f, "{}", c),
            Literal { c, casei: true } => write!(f, "(?i:{})", c),
            LiteralString { ref s, casei } => {
                if casei { try!(write!(f, "(?i:")); }
                for c in s {
                    try!(write!(f, "{}", c));
                }
                if casei { try!(write!(f, ")")); }
                Ok(())
            }
            AnyChar => write!(f, "(?s:.)"),
            AnyCharNoNL => write!(f, "."),
            Class { ref ranges, .. } => write!(f, "{}", ranges),
            StartLine => write!(f, "(?m:^)"),
            EndLine => write!(f, "(?m:$)"),
            StartText => write!(f, r"^"),
            EndText => write!(f, r"$"),
            WordBoundary => write!(f, r"\b"),
            NotWordBoundary => write!(f, r"\B"),
            Group { ref e, i: None, name: None } => write!(f, "(?:{})", e),
            Group { ref e, name: None, .. } => write!(f, "({})", e),
            Group { ref e, name: Some(ref n), .. } => {
                write!(f, "(?P<{}>{})", n, e)
            }
            Repeat { ref e, r, greedy: true } => write!(f, "{}{}", e, r),
            Repeat { ref e, r, greedy: false } => write!(f, "{}{}?", e, r),
            Concat(ref es) => {
                for e in es {
                    try!(write!(f, "{}", e));
                }
                Ok(())
            }
            Alternate(ref es) => {
                for (i, e) in es.iter().enumerate() {
                    if i > 0 { try!(write!(f, "|")); }
                    try!(write!(f, "{}", e));
                }
                Ok(())
            }
        }
    }
}

impl fmt::Display for Repeater {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            ZeroOrOne => write!(f, "?"),
            ZeroOrMore => write!(f, "*"),
            OneOrMore => write!(f, "+"),
            Range { start: Some(s), end: None } => write!(f, "{{{},}}", s),
            Range { start: None, end: Some(e) } => write!(f, "{{,{}}}", e),
            Range { start: Some(s), end: Some(e) } => {
                write!(f, "{{{}, {}}}", s, e)
            }
            Range { start: None, end: None } => unreachable!(),
        }
    }
}

impl fmt::Display for CharClass {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        try!(write!(f, "["));
        for range in self.iter() {
            try!(write!(f, "{}", range));
        }
        write!(f, "]")
    }
}

impl fmt::Display for ClassRange {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}-{}", self.start, self.end)
    }
}
