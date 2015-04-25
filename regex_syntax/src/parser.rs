use std::cmp::{max, min};
use std::ops::{Deref, DerefMut};
use std::result;
use {Expr, Repeater, CharClass, ClassRange, CaptureIndex, CaptureName};

pub type Result<T> = ::std::result::Result<T, Error>;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Error {
    pub pos: i32,
    pub surround: String,
    pub kind: ErrorKind,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ErrorKind {
    DoubleFlagNegation,
    EmptyAlternate,
    EmptyCaptureName,
    EmptyFlagNegation,
    EmptyGroup,
    InvalidCaptureName(String),
    RepeaterExpectsExpr,
    RepeaterUnexpectedExpr(Expr),
    UnclosedCaptureName(String),
    UnclosedParen,
    UnexpectedFlagEof,
    UnopenedParen,
    UnrecognizedFlag(char),
}

#[derive(Debug)]
enum Build {
    Expr(Expr),
    LeftParen {
        i: CaptureIndex,
        name: CaptureName,
        chari: i32,
        old_flags: Flags,
    },
}

#[derive(Debug)]
struct Parser {
    chars: Vec<char>,
    chari: i32,
    stack: Vec<Build>,
    caps: u32,
    flags: Flags,
}

#[derive(Clone, Copy, Debug)]
struct Flags {
    casei: bool,
    multi: bool,
    dotnl: bool,
    swap_greed: bool,
}

impl Parser {
    fn parse(s: &str) -> Result<Expr> {
        Parser {
            chars: s.chars().collect(),
            chari: 0,
            stack: vec![],
            caps: 0,
            flags: Flags {
                casei: false,
                multi: false,
                dotnl: false,
                swap_greed: false,
            },
        }.parse_expr()
    }

    fn parse_expr(mut self) -> Result<Expr> {
        while !self.eof() {
            let c = self.cur();
            let build_expr = match c {
                '|' => { let e = try!(self.alternate()); self.bump(); e }
                '?' => try!(self.parse_simple_repeat(Repeater::ZeroOrOne)),
                '*' => try!(self.parse_simple_repeat(Repeater::ZeroOrMore)),
                '+' => try!(self.parse_simple_repeat(Repeater::OneOrMore)),
                '^' => {
                    if self.flags.multi {
                        self.parse_one(Expr::StartLine)
                    } else {
                        self.parse_one(Expr::StartText)
                    }
                }
                '$' => {
                    if self.flags.multi {
                        self.parse_one(Expr::EndLine)
                    } else {
                        self.parse_one(Expr::EndText)
                    }
                }
                '.' => {
                    if self.flags.dotnl {
                        self.parse_one(Expr::AnyChar)
                    } else {
                        self.parse_one(Expr::AnyCharNoNL)
                    }
                }
                '(' => try!(self.parse_group()),
                ')' => {
                    let (old_flags, e) = try!(self.close_paren());
                    self.bump();
                    self.flags = old_flags;
                    e
                }
                c => Build::Expr(Expr::Literal {
                    c: self.bump(),
                    casei: self.flags.casei,
                }),
            };
            if !build_expr.is_empty() {
                self.stack.push(build_expr);
            }
        }
        self.finish_concat()
    }

    // Parses a group, e.g., `(abc)`.
    //
    // Start: `(`
    // End:   `a`
    //
    // A more interesting example, `(?P<foo>abc)`.
    //
    // Start: `(`
    // End:   `a`
    fn parse_group(&mut self) -> Result<Build> {
        let chari = self.chari;
        let mut name: CaptureName = None;
        self.bump();
        if self.bump_if("?P<") {
            name = Some(try!(self.parse_group_name()));
        } else if self.bump_if("?") {
            // This can never be capturing. It's either setting flags for
            // the current group, or it's opening a non-capturing group or
            // it's opening a group with a specific set of flags (which is
            // also non-capturing).
            // Anything else is an error.
            return self.parse_group_flags(chari);
        }
        self.caps += 1;
        Ok(Build::LeftParen {
            i: Some(self.caps),
            name: name,
            chari: chari,
            old_flags: self.flags, // no flags changed if we're here
        })
    }

    // Parses flags (inline or grouped), e.g., `(?s-i:abc)`.
    //
    // Start: `s`
    // End:   `a`
    //
    // Another example, `(?s-i)a`.
    //
    // Start: `s`
    // End:   `a`
    fn parse_group_flags(&mut self, opening_chari: i32) -> Result<Build> {
        let old_flags = self.flags;
        let mut sign = true;
        let mut saw_flag = false;
        loop {
            if self.eof() {
                // e.g., (?i
                return Err(self.err(ErrorKind::UnexpectedFlagEof));
            }
            match self.cur() {
                'i' => { self.flags.casei = sign; saw_flag = true }
                'm' => { self.flags.multi = sign; saw_flag = true }
                's' => { self.flags.dotnl = sign; saw_flag = true }
                'U' => { self.flags.swap_greed = sign; saw_flag = true }
                '-' => {
                    if !sign {
                        // e.g., (?-i-s)
                        return Err(self.err(ErrorKind::DoubleFlagNegation));
                    }
                    sign = false;
                    saw_flag = false;
                }
                ')' => {
                    if !saw_flag {
                        // e.g., (?)
                        return Err(self.err(ErrorKind::EmptyFlagNegation));
                    }
                    // At this point, we're just changing the flags inside
                    // the current group, which means the old flags have
                    // been saved elsewhere. Our modifications in place are
                    // okey dokey!
                    //
                    // This particular flag expression only has a stateful
                    // impact on a regex's AST, so nothing gets explicitly
                    // added.
                    self.bump();
                    return Ok(Build::Expr(Expr::Empty));
                }
                ':' => {
                    if !sign && !saw_flag {
                        // e.g., (?i-:a)
                        // Note that if there's no negation, it's OK not
                        // to see flag, because you end up with a regular
                        // non-capturing group: `(?:a)`.
                        return Err(self.err(ErrorKind::EmptyFlagNegation));
                    }
                    self.bump();
                    return Ok(Build::LeftParen {
                        i: None,
                        name: None,
                        chari: opening_chari,
                        old_flags: old_flags,
                    });
                }
                // e.g., (?z:a)
                c => return Err(self.err(ErrorKind::UnrecognizedFlag(c))),
            }
            self.bump();
        }
    }

    // Parses a group name, e.g., `foo` in `(?P<foo>abc)`.
    //
    // Start: `f`
    // End:   `a`
    fn parse_group_name(&mut self) -> Result<String> {
        let mut name = String::new();
        while !self.eof() && !self.peek_is('>') {
            name.push(self.bump());
        }
        if self.eof() {
            // e.g., (?P<a
            return Err(self.err(ErrorKind::UnclosedCaptureName(name)));
        }
        let all_valid = name.chars().all(is_valid_cap);
        match name.chars().next() {
            // e.g., (?P<>a)
            None => Err(self.err(ErrorKind::EmptyCaptureName)),
            Some(c) if (c >= '0' && c <= '9') || !all_valid => {
                // e.g., (?P<a#>x)
                // e.g., (?P<1a>x)
                Err(self.err(ErrorKind::InvalidCaptureName(name)))
            }
            _ => {
                self.bump(); // for `>`
                Ok(name)
            }
        }
    }

    // Parses a simple repetition operator, e.g., `a+?z`.
    //
    // Start: `+`
    // End:   `z`
    //
    // N.B. "simple" in this context means "not min/max repetition",
    // e.g., `a{1,2}`.
    fn parse_simple_repeat(&mut self, rep: Repeater) -> Result<Build> {
        let e = try!(self.pop(ErrorKind::RepeaterExpectsExpr)); // e.g., (*
        if !e.can_repeat() {
            // e.g., a**
            return Err(self.err(ErrorKind::RepeaterUnexpectedExpr(e)));
        }
        self.bump();
        Ok(Build::Expr(Expr::Repeat {
            e: Box::new(e),
            r: rep,
            greedy: !self.bump_if('?') ^ self.flags.swap_greed,
        }))
    }

    // Always bump to the next input and return the given expression as a
    // `Build`.
    //
    // This is mostly for convenience when the surrounding context implies
    // that the next character corresponds to the given expression.
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

    fn bump_if<B: Bumpable + Copy>(&mut self, s: B) -> bool {
        if self.peek_is(s) {
            self.chari += s.char_len() as i32;
            true
        } else {
            false
        }
    }

    fn peek_is<B: Bumpable + Copy>(&self, s: B) -> bool {
        s.peek_is(self)
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
            None | Some(Build::LeftParen{..}) => Err(self.err(expected)),
            Some(Build::Expr(e)) => Ok(e),
        }
    }
}

// Auxiliary methods for manipulating the expression stack.
impl Parser {
    // Called whenever an alternate (`|`) is found.
    //
    // This pops the expression stack until:
    //
    //  1. The stack is empty. Pushes an alternation with one arm.
    //  2. An opening parenthesis is found. Leave the parenthesis
    //     on the stack and push an alternation with one arm.
    //  3. An alternate (`|`) is found. Pop the existing alternation,
    //     add an arm and push the modified alternation.
    //
    // Each "arm" in the above corresponds to the concatenation of all
    // popped expressions.
    //
    // In the first two cases, the stack is left in an invalid state
    // because an alternation with one arm is not allowed. This
    // particular state will be detected by `finish_concat` and an
    // error will be reported.
    //
    // In none of the cases is an empty arm allowed. If an empty arm
    // is found, an error is reported.
    fn alternate(&mut self) -> Result<Build> {
        let mut concat = vec![];
        let alts = |es| Ok(Build::Expr(Expr::Alternate(es)));
        loop {
            match self.stack.pop() {
                None => {
                    if concat.is_empty() {
                        // e.g., |a
                        return Err(self.err(ErrorKind::EmptyAlternate));
                    }
                    return alts(vec![rev_concat(concat)]);
                }
                Some(e @ Build::LeftParen{..}) => {
                    if concat.is_empty() {
                        // e.g., (|a)
                        return Err(self.err(ErrorKind::EmptyAlternate));
                    }
                    self.stack.push(e);
                    return alts(vec![rev_concat(concat)]);
                }
                Some(Build::Expr(Expr::Alternate(mut es))) => {
                    if concat.is_empty() {
                        // e.g., a||
                        return Err(self.err(ErrorKind::EmptyAlternate));
                    }
                    es.push(rev_concat(concat));
                    return alts(es);
                }
                Some(Build::Expr(e)) => { concat.push(e); }
            }
        }
    }

    // Called whenever a closing parenthesis (`)`) is found.
    //
    // This pops the expression stack until:
    //
    //  1. The stack is empty. An error is reported because this
    //     indicates an unopened parenthesis.
    //  2. An opening parenthesis is found. Pop the opening parenthesis
    //     and push a `Group` expression.
    //  3. An alternate (`|`) is found. Pop the existing alternation
    //     and an arm to it in place. Pop one more item from the stack.
    //     If the stack was empty, then report an unopened parenthesis
    //     error, otherwise assume it is an opening parenthesis and
    //     push a `Group` expression with the popped alternation.
    //     (We can assume this is an opening parenthesis because an
    //     alternation either corresponds to the entire Regex or it
    //     corresponds to an entire group. This is guaranteed by the
    //     `alternate` method.)
    //
    // Each "arm" in the above corresponds to the concatenation of all
    // popped expressions.
    //
    // Empty arms nor empty groups are allowed.
    fn close_paren(&mut self) -> Result<(Flags, Build)> {
        let mut concat = vec![];
        loop {
            match self.stack.pop() {
                // e.g., )
                None => return Err(self.err(ErrorKind::UnopenedParen)),
                Some(Build::LeftParen { i, name, old_flags, .. }) => {
                    if concat.is_empty() {
                        // e.g., ()
                        return Err(self.err(ErrorKind::EmptyGroup));
                    }
                    return Ok((old_flags, Build::Expr(Expr::Group {
                        e: Box::new(rev_concat(concat)),
                        i: i,
                        name: name,
                    })));
                }
                Some(Build::Expr(Expr::Alternate(mut es))) => {
                    if concat.is_empty() {
                        // e.g., (a|)
                        return Err(self.err(ErrorKind::EmptyAlternate));
                    }
                    es.push(rev_concat(concat));
                    match self.stack.pop() {
                        // e.g., a|b)
                        None => return Err(self.err(ErrorKind::UnopenedParen)),
                        Some(Build::Expr(_)) => unreachable!(),
                        Some(Build::LeftParen { i, name, old_flags, .. }) => {
                            return Ok((old_flags, Build::Expr(Expr::Group {
                                e: Box::new(Expr::Alternate(es)),
                                i: i,
                                name: name,
                            })));
                        }
                    }
                }
                Some(Build::Expr(e)) => { concat.push(e); }
            }
        }
    }

    // Called only when the parser reaches the end of input.
    //
    // This pops the expression stack until:
    //
    //  1. The stack is empty. Return concatenation of popped
    //     expressions. This concatenation may be empty!
    //  2. An alternation is found. Pop the alternation and push
    //     a new arm. Return the alternation as the entire Regex.
    //
    // If an opening parenthesis is popped, then an error is
    // returned since it indicates an unclosed parenthesis.
    fn finish_concat(&mut self) -> Result<Expr> {
        let mut concat = vec![];
        loop {
            match self.stack.pop() {
                None => { return Ok(rev_concat(concat)); }
                Some(Build::LeftParen{ chari, ..}) => {
                    // e.g., a(b
                    return Err(self.errat(chari, ErrorKind::UnclosedParen));
                }
                Some(Build::Expr(Expr::Alternate(mut es))) => {
                    if concat.is_empty() {
                        // e.g., a|
                        return Err(self.err(ErrorKind::EmptyAlternate));
                    }
                    es.push(rev_concat(concat));
                    return Ok(Expr::Alternate(es));
                }
                Some(Build::Expr(e)) => { concat.push(e); }
            }
        }
    }
}

impl Build {
    fn is_empty(&self) -> bool {
        match *self {
            Build::Expr(Expr::Empty) => true,
            _ => false,
        }
    }
}

// Make it ergonomic to conditionally bump the parser.
// i.e., `bump_if('a')` or `bump_if("abc")`.
trait Bumpable {
    fn peek_is(&self, p: &Parser) -> bool;
    fn char_len(&self) -> usize;
}

impl<'a, T: ?Sized + Bumpable> Bumpable for &'a T {
    fn peek_is(&self, p: &Parser) -> bool { (**self).peek_is(p) }
    fn char_len(&self) -> usize { (**self).char_len() }
}

impl Bumpable for char {
    fn peek_is(&self, p: &Parser) -> bool {
        if p.eof() { false } else { p.cur() == *self }
    }
    fn char_len(&self) -> usize { 1 }
}

impl Bumpable for str {
    fn peek_is(&self, p: &Parser) -> bool {
        let rest = &p.chars[p.chari as usize..];
        if rest.len() < self.char_len() {
            false
        } else {
            self.chars().zip(rest).all(|(c1, &c2)| c1 == c2)
        }
    }
    fn char_len(&self) -> usize { self.chars().count() }
}

fn rev_concat(mut exprs: Vec<Expr>) -> Expr {
    if exprs.len() == 0 {
        Expr::Empty
    } else if exprs.len() == 1 {
        exprs.pop().unwrap()
    } else {
        exprs.reverse();
        Expr::Concat(exprs)
    }
}

fn is_valid_cap(c: char) -> bool {
    c == '_' || (c >= '0' && c <= '9')
    || (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')
}

#[cfg(test)]
mod tests {
    use { Expr, Repeater, Error, ErrorKind };
    use super::Parser;

    fn p(s: &str) -> Expr { Parser::parse(s).unwrap() }
    fn perr(s: &str) -> Error { Parser::parse(s).unwrap_err() }
    fn lit(c: char) -> Expr { Expr::Literal { c: c, casei: false } }
    fn liti(c: char) -> Expr { Expr::Literal { c: c, casei: true } }
    fn b<T>(v: T) -> Box<T> { Box::new(v) }
    fn c(es: &[Expr]) -> Expr { Expr::Concat(es.to_vec()) }

    #[test]
    fn empty() {
        assert_eq!(p(""), Expr::Empty);
    }

    #[test]
    fn literal() {
        assert_eq!(p("a"), lit('a'));
    }

    #[test]
    fn literal_string() {
        // TODO: This should be a literal string, but we need to implement
        // "simplify" or something first.
        assert_eq!(p("ab"), Expr::Concat(vec![lit('a'), lit('b')]));
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
            e: b(lit('a')),
            r: Repeater::ZeroOrOne,
            greedy: true,
        });
    }

    #[test]
    fn repeat_zero_or_one_greedy_concat() {
        assert_eq!(p("ab?"), Expr::Concat(vec![
            lit('a'),
            Expr::Repeat {
                e: b(lit('b')),
                r: Repeater::ZeroOrOne,
                greedy: true,
            },
        ]));
    }

    #[test]
    fn repeat_zero_or_one_nongreedy() {
        assert_eq!(p("a??"), Expr::Repeat {
            e: b(lit('a')),
            r: Repeater::ZeroOrOne,
            greedy: false,
        });
    }

    #[test]
    fn repeat_one_or_more_greedy() {
        assert_eq!(p("a+"), Expr::Repeat {
            e: b(lit('a')),
            r: Repeater::OneOrMore,
            greedy: true,
        });
    }

    #[test]
    fn repeat_one_or_more_nongreedy() {
        assert_eq!(p("a+?"), Expr::Repeat {
            e: b(lit('a')),
            r: Repeater::OneOrMore,
            greedy: false,
        });
    }

    #[test]
    fn repeat_zero_or_more_greedy() {
        assert_eq!(p("a*"), Expr::Repeat {
            e: b(lit('a')),
            r: Repeater::ZeroOrMore,
            greedy: true,
        });
    }

    #[test]
    fn repeat_zero_or_more_nongreedy() {
        assert_eq!(p("a*?"), Expr::Repeat {
            e: b(lit('a')),
            r: Repeater::ZeroOrMore,
            greedy: false,
        });
    }

    #[test]
    fn group_literal() {
        assert_eq!(p("(a)"), Expr::Group {
            e: b(lit('a')),
            i: Some(1),
            name: None,
        });
    }

    #[test]
    fn group_literal_concat() {
        assert_eq!(p("(ab)"), Expr::Group {
            e: b(c(&[lit('a'), lit('b')])),
            i: Some(1),
            name: None,
        });
    }

    #[test]
    fn alt_two() {
        assert_eq!(p("a|b"), Expr::Alternate(vec![lit('a'), lit('b')]));
    }

    #[test]
    fn alt_many() {
        assert_eq!(p("a|b|c"), Expr::Alternate(vec![
            lit('a'), lit('b'), lit('c'),
        ]));
    }

    #[test]
    fn alt_many_concat() {
        assert_eq!(p("ab|bc|cd"), Expr::Alternate(vec![
            c(&[lit('a'), lit('b')]),
            c(&[lit('b'), lit('c')]),
            c(&[lit('c'), lit('d')]),
        ]));
    }

    #[test]
    fn alt_group_two() {
        assert_eq!(p("(a|b)"), Expr::Group {
            e: b(Expr::Alternate(vec![lit('a'), lit('b')])),
            i: Some(1),
            name: None,
        });
    }

    #[test]
    fn alt_group_many() {
        assert_eq!(p("(a|b|c)"), Expr::Group {
            e: b(Expr::Alternate(vec![lit('a'), lit('b'), lit('c')])),
            i: Some(1),
            name: None,
        });
    }

    #[test]
    fn alt_group_many_concat() {
        assert_eq!(p("(ab|bc|cd)"), Expr::Group {
            e: b(Expr::Alternate(vec![
                c(&[lit('a'), lit('b')]),
                c(&[lit('b'), lit('c')]),
                c(&[lit('c'), lit('d')]),
            ])),
            i: Some(1),
            name: None,
        });
    }

    #[test]
    fn alt_group_nested() {
        assert_eq!(p("(ab|(bc|(cd)))"), Expr::Group {
            e: b(Expr::Alternate(vec![
                c(&[lit('a'), lit('b')]),
                Expr::Group {
                    e: b(Expr::Alternate(vec![
                        c(&[lit('b'), lit('c')]),
                        Expr::Group {
                            e: b(c(&[lit('c'), lit('d')])),
                            i: Some(3),
                            name: None,
                        }
                    ])),
                    i: Some(2),
                    name: None,
                },
            ])),
            i: Some(1),
            name: None,
        });
    }

    #[test]
    fn group_name() {
        assert_eq!(p("(?P<foo>a)"), Expr::Group {
            e: b(lit('a')),
            i: Some(1),
            name: Some("foo".into()),
        });
    }

    #[test]
    fn group_no_capture() {
        assert_eq!(p("(?:a)"), Expr::Group {
            e: b(lit('a')),
            i: None,
            name: None,
        });
    }

    #[test]
    fn group_flags() {
        assert_eq!(p("(?i:a)"), Expr::Group {
            e: b(liti('a')),
            i: None,
            name: None,
        });
    }

    #[test]
    fn group_flags_returned() {
        assert_eq!(p("(?i:a)a"), c(&[
            Expr::Group {
                e: b(liti('a')),
                i: None,
                name: None,
            },
            lit('a'),
        ]));
    }

    #[test]
    fn group_flags_retained() {
        assert_eq!(p("(?i)(?-i:a)a"), c(&[
            Expr::Group {
                e: b(lit('a')),
                i: None,
                name: None,
            },
            liti('a'),
        ]));
    }

    #[test]
    fn flags_inline() {
        assert_eq!(p("(?i)a"), liti('a'));
    }

    #[test]
    fn flags_inline_multiple() {
        assert_eq!(p("(?is)a."), c(&[liti('a'), Expr::AnyChar]));
    }

    #[test]
    fn flags_inline_multiline() {
        assert_eq!(p("(?m)^(?-m)$"), c(&[Expr::StartLine, Expr::EndText]));
    }

    #[test]
    fn flags_inline_swap_greed() {
        assert_eq!(p("(?U)a*a*?(?i-U)a*a*?"), c(&[
            Expr::Repeat {
                e: b(lit('a')),
                r: Repeater::ZeroOrMore,
                greedy: false,
            },
            Expr::Repeat {
                e: b(lit('a')),
                r: Repeater::ZeroOrMore,
                greedy: true,
            },
            Expr::Repeat {
                e: b(liti('a')),
                r: Repeater::ZeroOrMore,
                greedy: true,
            },
            Expr::Repeat {
                e: b(liti('a')),
                r: Repeater::ZeroOrMore,
                greedy: false,
            },
        ]));
    }

    #[test]
    fn flags_inline_multiple_negate_one() {
        assert_eq!(p("(?is)a.(?i-s)a."), c(&[
            liti('a'), Expr::AnyChar, liti('a'), Expr::AnyCharNoNL,
        ]));
    }

    #[test]
    fn flags_inline_negate() {
        assert_eq!(p("(?i)a(?-i)a"), c(&[liti('a'), lit('a')]));
    }

    #[test]
    fn flags_group_inline() {
        assert_eq!(p("(a(?i)a)a"), c(&[
            Expr::Group {
                e: b(c(&[lit('a'), liti('a')])),
                i: Some(1),
                name: None,
            },
            lit('a'),
        ]));
    }

    #[test]
    fn flags_group_inline_retain() {
        assert_eq!(p("(?i)((?-i)a)a"), c(&[
            Expr::Group {
                e: b(lit('a')),
                i: Some(1),
                name: None,
            },
            liti('a'),
        ]));
    }

    /******************************************************/
    // Test every single possible error case.
    /******************************************************/

    #[test]
    fn error_repeat_no_expr() {
        assert_eq!(perr("(*"), Error {
            pos: 1,
            surround: "(*".into(),
            kind: ErrorKind::RepeaterExpectsExpr,
        });
    }

    #[test]
    fn error_repeat_illegal_exprs() {
        assert_eq!(perr("a**"), Error {
            pos: 2,
            surround: "a**".into(),
            kind: ErrorKind::RepeaterUnexpectedExpr(Expr::Repeat {
                e: b(lit('a')),
                r: Repeater::ZeroOrMore,
                greedy: true,
            }),
        });
        assert_eq!(perr("a|*"), Error {
            pos: 2,
            surround: "a|*".into(),
            kind: ErrorKind::RepeaterUnexpectedExpr(Expr::Alternate(vec![
                lit('a'),
            ])),
        });
    }

    #[test]
    fn error_alternate_empty() {
        assert_eq!(perr("|a"), Error {
            pos: 0,
            surround: "|a".into(),
            kind: ErrorKind::EmptyAlternate,
        });
    }

    #[test]
    fn error_alternate_empty_with_group() {
        assert_eq!(perr("(|a)"), Error {
            pos: 1,
            surround: "(|a)".into(),
            kind: ErrorKind::EmptyAlternate,
        });
    }

    #[test]
    fn error_alternate_empty_with_alternate() {
        assert_eq!(perr("a||"), Error {
            pos: 2,
            surround: "a||".into(),
            kind: ErrorKind::EmptyAlternate,
        });
    }

    #[test]
    fn error_close_paren_unopened_empty() {
        assert_eq!(perr(")"), Error {
            pos: 0,
            surround: ")".into(),
            kind: ErrorKind::UnopenedParen,
        });
    }

    #[test]
    fn error_close_paren_unopened() {
        assert_eq!(perr("ab)"), Error {
            pos: 2,
            surround: "ab)".into(),
            kind: ErrorKind::UnopenedParen,
        });
    }

    #[test]
    fn error_close_paren_unopened_with_alt() {
        assert_eq!(perr("a|b)"), Error {
            pos: 3,
            surround: "a|b)".into(),
            kind: ErrorKind::UnopenedParen,
        });
    }

    #[test]
    fn error_close_paren_empty_alt() {
        assert_eq!(perr("(a|)"), Error {
            pos: 3,
            surround: "(a|)".into(),
            kind: ErrorKind::EmptyAlternate,
        });
    }

    #[test]
    fn error_close_paren_empty_group() {
        assert_eq!(perr("()"), Error {
            pos: 1,
            surround: "()".into(),
            kind: ErrorKind::EmptyGroup,
        });
    }

    #[test]
    fn error_close_paren_empty_group_with_name() {
        assert_eq!(perr("(?P<foo>)"), Error {
            pos: 8,
            surround: "<foo>)".into(),
            kind: ErrorKind::EmptyGroup,
        });
    }

    #[test]
    fn error_finish_concat_unclosed() {
        assert_eq!(perr("ab(xy"), Error {
            pos: 2,
            surround: "ab(xy".into(),
            kind: ErrorKind::UnclosedParen,
        });
    }

    #[test]
    fn error_finish_concat_empty_alt() {
        assert_eq!(perr("a|"), Error {
            pos: 2,
            surround: "a|".into(),
            kind: ErrorKind::EmptyAlternate,
        });
    }

    #[test]
    fn error_group_name_invalid() {
        assert_eq!(perr("(?P<a#>x)"), Error {
            pos: 6,
            surround: "?P<a#>x)".into(),
            kind: ErrorKind::InvalidCaptureName("a#".into()),
        });
    }

    #[test]
    fn error_group_name_invalid_leading() {
        assert_eq!(perr("(?P<1a>a)"), Error {
            pos: 6,
            surround: "?P<1a>a)".into(),
            kind: ErrorKind::InvalidCaptureName("1a".into()),
        });
    }

    #[test]
    fn error_group_name_unexpected_eof() {
        assert_eq!(perr("(?P<a"), Error {
            pos: 5,
            surround: "(?P<a".into(),
            kind: ErrorKind::UnclosedCaptureName("a".into()),
        });
    }

    #[test]
    fn error_group_name_empty() {
        assert_eq!(perr("(?P<>a)"), Error {
            pos: 4,
            surround: "(?P<>a)".into(),
            kind: ErrorKind::EmptyCaptureName,
        });
    }

    #[test]
    fn error_group_opts_unrecognized_flag() {
        assert_eq!(perr("(?z:a)"), Error {
            pos: 2,
            surround: "(?z:a)".into(),
            kind: ErrorKind::UnrecognizedFlag('z'),
        });
    }

    #[test]
    fn error_group_opts_unexpected_eof() {
        assert_eq!(perr("(?i"), Error {
            pos: 3,
            surround: "(?i".into(),
            kind: ErrorKind::UnexpectedFlagEof,
        });
    }

    #[test]
    fn error_group_opts_double_negation() {
        assert_eq!(perr("(?-i-s:a)"), Error {
            pos: 4,
            surround: "(?-i-s:a)".into(),
            kind: ErrorKind::DoubleFlagNegation,
        });
    }

    #[test]
    fn error_group_opts_empty_negation() {
        assert_eq!(perr("(?i-:a)"), Error {
            pos: 4,
            surround: "(?i-:a)".into(),
            kind: ErrorKind::EmptyFlagNegation,
        });
    }

    #[test]
    fn error_group_opts_empty() {
        assert_eq!(perr("(?)"), Error {
            pos: 2,
            surround: "(?)".into(),
            kind: ErrorKind::EmptyFlagNegation,
        });
    }
}
