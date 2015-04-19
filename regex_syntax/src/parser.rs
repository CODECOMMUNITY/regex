use {Expr, Repeat, CharClass, ClassRange, CaptureIndex, CaptureName};

macro_rules! pushe {
    ($_self:expr, $e:expr) => ({ let e = $e; $_self.stack.push_expr(e); });
}

pub type Result<T> = ::std::result::Result<T, Error>;

#[derive(Clone, Debug)]
pub struct Error {
    pub pos: usize,
    pub surround: String,
    pub kind: ErrorKind,
}

#[derive(Clone, Debug)]
pub enum ErrorKind {
    RepeatExpectsExpr,
}

struct ExprBuilder(Vec<ExprBuild>);

enum ExprBuild {
    Expr(Expr),
    LeftParen { i: CaptureIndex, name: CaptureName },
    Alt,
}

struct Parser {
    chars: Vec<char>,
    chari: usize,
    stack: ExprBuilder,
    caps: usize,
}

impl Parser {
    fn parse(s: &str) -> Result<Expr> {
        Parser {
            chars: s.chars().collect(),
            chari: 0,
            stack: ExprBuilder::new(),
            caps: 0,
        }.parse_expr()
    }

    fn parse_expr(mut self) -> Result<Expr> {
        if self.chars.is_empty() {
            return Ok(Expr::Empty);
        }
        while !self.eof() {
            let c = self.cur();
            match c {
                '?' => {
                    self.bump();
                    try!(self.parse_simple_repeat(Repeat::ZeroOrOne));
                }
                '*' => {
                    self.bump();
                    try!(self.parse_simple_repeat(Repeat::ZeroOrMore));
                }
                '+' => {
                    self.bump();
                    try!(self.parse_simple_repeat(Repeat::OneOrMore));
                }
                '^' => { self.bump(); pushe!(self, Expr::StartText) }
                '$' => { self.bump(); pushe!(self, Expr::EndText) }
                '.' => { self.bump(); pushe!(self, Expr::AnyCharNoNL) }
                c => {
                    let lit = Expr::Literal { c: self.bump(), casei: false };
                    pushe!(self, lit);
                }
            }
        }
        Ok(self.stack.collapse())
    }

    fn parse_simple_repeat(&mut self, rep: Repeat) -> Result<()> {
        let e = try!(self.pop(ErrorKind::RepeatExpectsExpr));
        Ok(pushe!(self, Expr::Repeat { e: Box::new(e), r: rep, greedy: true }))
    }
}

// Auxiliary helper methods.
impl Parser {
    fn bump(&mut self) -> char { let c = self.cur(); self.chari += 1; c }
    fn cur(&self) -> char { self.chars[self.chari] }
    fn eof(&self) -> bool { self.chari >= self.chars.len() }

    fn err(&self, kind: ErrorKind) -> Error {
        Error { pos: 0, surround: "".into(), kind: kind }
    }

    fn pop(&mut self, expected: ErrorKind) -> Result<Expr> {
        match self.stack.0.pop() {
            None => Err(self.err(expected)),
            Some(ExprBuild::Expr(e)) => Ok(e),
            _ => unimplemented!(),
        }
    }
}

impl ExprBuilder {
    fn new() -> ExprBuilder { ExprBuilder(vec![]) }
    fn push_expr(&mut self, e: Expr) { self.0.push(ExprBuild::Expr(e)) }

    fn collapse(self) -> Expr {
        let mut concat = vec![];
        for build in self.0 {
            match build {
                ExprBuild::Expr(e) => { concat.push(e); }
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

#[cfg(test)]
mod tests {
    use Expr;
    use super::Parser;

    fn p(s: &str) -> Expr { Parser::parse(s).unwrap() }

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
}
