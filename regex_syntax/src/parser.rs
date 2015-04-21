use std::cmp::{max, min};
use std::ops::{Deref, DerefMut};
use {Expr, Repeater, CharClass, ClassRange, CaptureIndex, CaptureName};

macro_rules! pushe {
    ($_self:expr, $e:expr) => ({ let e = $e; $_self.stack.push_expr(e); });
}

pub type Result<T> = ::std::result::Result<T, Error>;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Error {
    pub pos: i32,
    pub surround: String,
    pub kind: ErrorKind,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ErrorKind {
    RepeaterExpectsExpr,
    RepeaterUnexpectedExpr(Expr),
}

struct Builder(Vec<Build>);

enum Build {
    Expr(Expr),
    LeftParen { i: CaptureIndex, name: CaptureName },
    Alt,
}

struct Parser {
    chars: Vec<char>,
    chari: i32,
    stack: Builder,
    caps: usize,
}

impl Parser {
    fn parse(s: &str) -> Result<Expr> {
        Parser {
            chars: s.chars().collect(),
            chari: 0,
            stack: Builder::new(),
            caps: 0,
        }.parse_expr()
    }

    fn parse_expr(mut self) -> Result<Expr> {
        if self.chars.is_empty() {
            return Ok(Expr::Empty);
        }
        while !self.eof() {
            let c = self.cur();
            let build_expr = match c {
                '?' => try!(self.parse_simple_repeat(Repeater::ZeroOrOne)),
                '*' => try!(self.parse_simple_repeat(Repeater::ZeroOrMore)),
                '+' => try!(self.parse_simple_repeat(Repeater::OneOrMore)),
                '^' => self.parse_one(Expr::StartText),
                '$' => self.parse_one(Expr::EndText),
                '.' => self.parse_one(Expr::AnyCharNoNL),
                c => Build::Expr(Expr::Literal {
                    c: self.bump(),
                    casei: false,
                }),
            };
            self.stack.push(build_expr);
        }
        Ok(self.stack.collapse())
    }

    fn parse_simple_repeat(&mut self, rep: Repeater) -> Result<Build> {
        let e = try!(self.pop(ErrorKind::RepeaterExpectsExpr));
        if !e.can_repeat() {
            return Err(self.err(ErrorKind::RepeaterUnexpectedExpr(e)));
        }
        self.bump();
        Ok(Build::Expr(Expr::Repeat {
            e: Box::new(e),
            r: rep,
            greedy: !self.bump_if_eq('?'),
        }))
    }

    fn parse_one(&mut self, e: Expr) -> Build {
        self.bump();
        Build::Expr(e)
    }
}

// Auxiliary helper methods.
impl Parser {
    fn bump(&mut self) -> char { let c = self.cur(); self.chari += 1; c }
    fn cur(&self) -> char { self.chars[self.chari as usize] }
    fn eof(&self) -> bool { self.chari as usize >= self.chars.len() }

    fn bump_if_eq(&mut self, c: char) -> bool {
        if self.peek_is(c) {
            self.bump();
            true
        } else {
            false
        }
    }

    fn peek_is(&self, c: char) -> bool {
        if self.eof() {
            false
        } else {
            self.cur() == c
        }
    }

    fn err(&self, kind: ErrorKind) -> Error {
        self.errat(self.chari, kind)
    }

    fn errat(&self, pos: i32, kind: ErrorKind) -> Error {
        Error { pos: pos, surround: self.windowat(pos), kind: kind }
    }

    fn windowat(&self, pos: i32) -> String {
        let (s, e) = (max(0, pos - 5), min(self.chars.len() as i32, pos + 5));
        self.chars[s as usize..e as usize].iter().cloned().collect()
    }

    fn pop(&mut self, expected: ErrorKind) -> Result<Expr> {
        match self.stack.pop() {
            None => Err(self.err(expected)),
            Some(Build::Expr(e)) => Ok(e),
            _ => unimplemented!(),
        }
    }
}

impl Builder {
    fn new() -> Builder { Builder(vec![]) }

    fn collapse(self) -> Expr {
        let mut concat = vec![];
        for build in self.0 {
            match build {
                Build::Expr(e) => { concat.push(e); }
                _ => unimplemented!(),
            }
        }
        if concat.len() == 0 {
            // TODO: Should be an error.
            Expr::Empty
        } else if concat.len() == 1 {
            concat.pop().unwrap()
        } else {
            Expr::Concat(concat)
        }
    }
}

impl Deref for Builder {
    type Target = Vec<Build>;
    fn deref(&self) -> &Vec<Build> { &self.0 }
}

impl DerefMut for Builder {
    fn deref_mut(&mut self) -> &mut Vec<Build> { &mut self.0 }
}

#[cfg(test)]
mod tests {
    use { Expr, Repeater, Error, ErrorKind };
    use super::Parser;

    fn p(s: &str) -> Expr { Parser::parse(s).unwrap() }
    fn perr(s: &str) -> Error { Parser::parse(s).unwrap_err() }

    #[test]
    fn empty() {
        assert_eq!(p(""), Expr::Empty);
    }

    #[test]
    fn literal() {
        assert_eq!(p("a"), Expr::Literal { c: 'a', casei: false });
    }

    #[test]
    fn start_literal() {
        assert_eq!(p("^a"), Expr::Concat(vec![
            Expr::StartText,
            Expr::Literal { c: 'a', casei: false },
        ]));
    }

    #[test]
    fn repeat_zero_or_one_greedy() {
        assert_eq!(p("a?"), Expr::Repeat {
            e: Box::new(Expr::Literal { c: 'a', casei: false }),
            r: Repeater::ZeroOrOne,
            greedy: true,
        });
    }

    #[test]
    fn repeat_zero_or_one_nongreedy() {
        assert_eq!(p("a??"), Expr::Repeat {
            e: Box::new(Expr::Literal { c: 'a', casei: false }),
            r: Repeater::ZeroOrOne,
            greedy: false,
        });
    }

    #[test]
    fn repeat_one_or_more_greedy() {
        assert_eq!(p("a+"), Expr::Repeat {
            e: Box::new(Expr::Literal { c: 'a', casei: false }),
            r: Repeater::OneOrMore,
            greedy: true,
        });
    }

    #[test]
    fn repeat_one_or_more_nongreedy() {
        assert_eq!(p("a+?"), Expr::Repeat {
            e: Box::new(Expr::Literal { c: 'a', casei: false }),
            r: Repeater::OneOrMore,
            greedy: false,
        });
    }

    #[test]
    fn repeat_zero_or_more_greedy() {
        assert_eq!(p("a*"), Expr::Repeat {
            e: Box::new(Expr::Literal { c: 'a', casei: false }),
            r: Repeater::ZeroOrMore,
            greedy: true,
        });
    }

    #[test]
    fn repeat_zero_or_more_nongreedy() {
        assert_eq!(p("a*?"), Expr::Repeat {
            e: Box::new(Expr::Literal { c: 'a', casei: false }),
            r: Repeater::ZeroOrMore,
            greedy: false,
        });
    }

    #[test]
    fn repeat_illegal_exprs() {
        assert_eq!(perr("a**"), Error {
            pos: 2,
            surround: "a**".into(),
            kind: ErrorKind::RepeaterUnexpectedExpr(Expr::Repeat {
                e: Box::new(Expr::Literal { c: 'a', casei: false }),
                r: Repeater::ZeroOrMore,
                greedy: true,
            }),
        });
    }
}
