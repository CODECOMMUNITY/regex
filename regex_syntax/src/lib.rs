// Copyright 2014 The Rust Project Developers. See the COPYRIGHT
// file at the top-level directory of this distribution and at
// http://rust-lang.org/COPYRIGHT.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

#![allow(dead_code, unused_imports, unused_variables)]

#[cfg(test)] extern crate quickcheck;

mod parser;
mod unicode;

use std::char;
use std::cmp::{Ordering, max, min};
use std::fmt;
use std::iter::IntoIterator;
use std::ops::{Deref, DerefMut};
use std::slice;
use std::vec;

use unicode::case_folding;

use self::Expr::*;
use self::Repeater::*;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Expr {
    Empty,
    Literal { c: char, casei: bool },
    LiteralString { s: Vec<char>, casei: bool },
    AnyChar,
    AnyCharNoNL,
    Class(CharClass),
    StartLine,
    EndLine,
    StartText,
    EndText,
    WordBoundary,
    NotWordBoundary,
    Group { e: Box<Expr>, i: CaptureIndex, name: CaptureName },
    Repeat { e: Box<Expr>, r: Repeater, greedy: bool },
    // The following either correspond to the entirety of a regex or appear
    // directly inside a `Group` expr. They are not valid in any other context.
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
    // `None` for `max` means "at least `min` repetitions."
    Range { min: u32, max: Option<u32> },
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct CharClass {
    pub ranges: Vec<ClassRange>,
    pub casei: bool,
}

#[derive(Clone, Copy, Debug, PartialEq, PartialOrd, Eq, Ord)]
pub struct ClassRange {
    // invariant: start <= end
    pub start: char,
    pub end: char,
}

impl Expr {
    // Returns true iff the expression can be repeated by a quantifier.
    fn can_repeat(&self) -> bool {
        match *self {
            Literal{..}
            | LiteralString{..}
            | AnyChar
            | AnyCharNoNL
            | Class(_)
            | Group{..}
            => true,
            _ => false,
        }
    }
}

impl Deref for CharClass {
    type Target = Vec<ClassRange>;
    fn deref(&self) -> &Vec<ClassRange> { &self.ranges }
}

impl DerefMut for CharClass {
    fn deref_mut(&mut self) -> &mut Vec<ClassRange> { &mut self.ranges }
}

impl IntoIterator for CharClass {
    type Item = ClassRange;
    type IntoIter = vec::IntoIter<ClassRange>;
    fn into_iter(self) -> vec::IntoIter<ClassRange> { self.ranges.into_iter() }
}

impl<'a> IntoIterator for &'a CharClass {
    type Item = &'a ClassRange;
    type IntoIter = slice::Iter<'a, ClassRange>;
    fn into_iter(self) -> slice::Iter<'a, ClassRange> { self.iter() }
}

impl<'a> IntoIterator for &'a mut CharClass {
    type Item = &'a mut ClassRange;
    type IntoIter = slice::IterMut<'a, ClassRange>;
    fn into_iter(self) -> slice::IterMut<'a, ClassRange> { self.iter_mut() }
}

impl CharClass {
    fn new(ranges: Vec<ClassRange>) -> CharClass {
        CharClass { ranges: ranges, casei: false }
    }

    fn new_casei(ranges: Vec<ClassRange>) -> CharClass {
        CharClass { ranges: ranges, casei: true }
    }

    fn empty(&self) -> CharClass {
        CharClass { ranges: Vec::with_capacity(self.len()), casei: self.casei }
    }

    fn merge(mut self, other: CharClass) -> CharClass {
        self.extend(other);
        self.canonicalize()
    }

    fn canonicalize(mut self) -> CharClass {
        // TODO: Save some cycles here by checking if already canonicalized.
        self.sort();
        let mut ordered = self.empty();
        for candidate in self {
            // If the candidate overlaps with an existing range, then it must
            // be the most recent range added because we process the candidates
            // in order.
            if let Some(or) = ordered.last_mut() {
                if or.overlapping(candidate) {
                    *or = or.merge(candidate);
                    continue;
                }
            }
            ordered.push(candidate);
        }
        ordered
    }

    fn negate(mut self) -> CharClass {
        fn range(s: char, e: char) -> ClassRange { ClassRange::new(s, e) }

        if self.is_empty() { return self; }
        self = self.canonicalize();
        let mut inv = self.empty();
        if self[0].start > '\x00' {
            inv.push(range('\x00', dec_char(self[0].start)));
        }
        for win in self.windows(2) {
            inv.push(range(inc_char(win[0].end), dec_char(win[1].start)));
        }
        if self[self.len() - 1].end < char::MAX {
            inv.push(range(inc_char(self[self.len() - 1].end), char::MAX));
        }
        inv
    }

    fn case_fold(self) -> CharClass {
        let mut folded = self.empty();
        for r in self {
            if r.needs_case_folding() {
                folded.extend(r.case_fold());
            } else {
                folded.push(r);
            }
        }
        folded.casei = true;
        folded.canonicalize()
    }
}

impl ClassRange {
    fn new(start: char, end: char) -> ClassRange {
        ClassRange { start: start, end: end }
    }

    fn canonical_ranges(mut chars: Vec<char>) -> Vec<ClassRange> {
        assert!(!chars.is_empty());
        chars.sort();
        chars.dedup();
        let mut ranges = Vec::with_capacity(chars.len());
        let mut chars = chars.into_iter();
        let mut start = chars.next().unwrap();
        let mut end = start;
        for c in chars {
            if c != inc_char(end) {
                ranges.push(ClassRange::new(start, end));
                start = c;
            }
            end = c;
        }
        ranges.push(ClassRange::new(start, end));
        ranges
    }

    fn overlapping(self, other: ClassRange) -> bool {
        max(self.start, other.start) <= inc_char(min(self.end, other.end))
    }

    fn merge(self, other: ClassRange) -> ClassRange {
        ClassRange {
            start: min(self.start, other.start),
            end: max(self.end, other.end),
        }
    }

    fn case_fold(self) -> Vec<ClassRange> {
        ClassRange::canonical_ranges(
            (self.start as u32..self.end as u32 + 1)
            .filter_map(char::from_u32)
            .map(simple_case_fold)
            .collect())
    }

    fn needs_case_folding(self) -> bool {
        case_folding::C_plus_S_table.binary_search_by(|&(c, _)| {
            if self.start <= c && c <= self.end {
                Ordering::Equal
            } else if c > self.end {
                Ordering::Greater
            } else {
                Ordering::Less
            }
        }).is_ok()
    }
}

// TODO(burntsushi): Write tests for the regex writer.
impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Empty => write!(f, ""),
            Literal { c, casei: false } => write!(f, "{}", quote_char(c)),
            Literal { c, casei: true } => write!(f, "(?i:{})", quote_char(c)),
            LiteralString { ref s, casei } => {
                if casei { try!(write!(f, "(?i:")); }
                for &c in s {
                    try!(write!(f, "{}", quote_char(c)));
                }
                if casei { try!(write!(f, ")")); }
                Ok(())
            }
            AnyChar => write!(f, "(?s:.)"),
            AnyCharNoNL => write!(f, "."),
            Class(ref cls) => write!(f, "{}", cls),
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
            Range { min: s, max: None } => write!(f, "{{{},}}", s),
            Range { min: s, max: Some(e) } if s == e => write!(f, "{{{}}}", s),
            Range { min: s, max: Some(e) } => write!(f, "{{{}, {}}}", s, e),
        }
    }
}

impl fmt::Display for CharClass {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if self.casei {
            try!(write!(f, "(?i:"));
        }
        try!(write!(f, "["));
        for range in self.iter() {
            try!(write!(f, "{}", range));
        }
        try!(write!(f, "]"));
        if self.casei {
            try!(write!(f, ")"));
        }
        Ok(())
    }
}

impl fmt::Display for ClassRange {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}-{}", quote_char(self.start), quote_char(self.end))
    }
}

pub type Result<T> = ::std::result::Result<T, Error>;

#[derive(Clone, Debug, PartialEq)]
pub struct Error {
    pub pos: i32,
    pub surround: String,
    pub kind: ErrorKind,
}

#[derive(Clone, Debug, PartialEq)]
pub enum ErrorKind {
    DoubleFlagNegation,
    EmptyAlternate,
    EmptyCaptureName,
    EmptyFlagNegation,
    EmptyGroup,
    InvalidBase10(String),
    InvalidBase16(String),
    InvalidCaptureName(String),
    InvalidRepeatRange { min: u32, max: u32 },
    InvalidScalarValue(u32),
    MissingBase10,
    RepeaterExpectsExpr,
    RepeaterUnexpectedExpr(Expr),
    UnclosedCaptureName(String),
    UnclosedHex,
    UnclosedParen,
    UnclosedRepeat,
    UnclosedUnicodeName,
    UnexpectedEscapeEof,
    UnexpectedFlagEof,
    UnexpectedTwoDigitHexEof,
    UnopenedParen,
    UnrecognizedEscape(char),
    UnrecognizedFlag(char),
    UnrecognizedUnicodeClass(String),
}

/// Returns the Unicode *simple* case folding of `c`.
///
/// N.B. This is hidden because it really isn't the responsibility of this
/// crate to do simple case folding. One hopes that either another crate or
/// the standard library will be able to do this for us. In any case, we still
/// expose it because it is used inside the various Regex engines.
#[doc(hidden)]
pub fn simple_case_fold(c: char) -> char {
    match case_folding::C_plus_S_table.binary_search_by(|&(x, _)| x.cmp(&c)) {
        Ok(i) => case_folding::C_plus_S_table[i].1,
        Err(_) => c,
    }
}

/// Escapes all regular expression meta characters in `text`.
///
/// The string returned may be safely used as a literal in a regular
/// expression.
pub fn quote(text: &str) -> String {
    let mut quoted = String::with_capacity(text.len());
    for c in text.chars() {
        if parser::is_punct(c) {
            quoted.push('\\');
        }
        quoted.push(c);
    }
    quoted
}

fn quote_char(c: char) -> String {
    let mut s = String::new();
    if parser::is_punct(c) {
        s.push('\\');
    }
    s.push(c);
    s
}

fn inc_char(c: char) -> char {
    match c {
        char::MAX => char::MAX,
        '\u{D7FF}' => '\u{E000}',
        c => char::from_u32(c as u32 + 1).unwrap(),
    }
}

fn dec_char(c: char) -> char {
    match c {
        '\x00' => '\x00',
        '\u{E000}' => '\u{D7FF}',
        c => char::from_u32(c as u32 - 1).unwrap(),
    }
}

#[cfg(test)]
mod tests {
    use quickcheck::{Arbitrary, Testable, quickcheck};
    use super::{CharClass, ClassRange};

    fn class(ranges: &[(char, char)]) -> CharClass {
        CharClass::new(ranges.iter().cloned().map(|(c1, c2)| {
            if c1 <= c2 {
                ClassRange::new(c1, c2)
            } else {
                ClassRange::new(c2, c1)
            }
        }).collect())
    }

    fn classi(ranges: &[(char, char)]) -> CharClass {
        CharClass::new_casei(ranges.iter().cloned().map(|(c1, c2)| {
            if c1 <= c2 {
                ClassRange::new(c1, c2)
            } else {
                ClassRange::new(c2, c1)
            }
        }).collect())
    }

    #[test]
    fn class_canon_no_change() {
        let cls = class(&[('a', 'c'), ('x', 'z')]);
        assert_eq!(cls.clone().canonicalize(), cls);
    }

    #[test]
    fn class_canon_unordered() {
        let cls = class(&[('x', 'z'), ('a', 'c')]);
        assert_eq!(cls.canonicalize(), class(&[
            ('a', 'c'), ('x', 'z'),
        ]));
    }

    #[test]
    fn class_canon_overlap() {
        let cls = class(&[('x', 'z'), ('w', 'y')]);
        assert_eq!(cls.canonicalize(), class(&[
            ('w', 'z'),
        ]));
    }

    #[test]
    fn class_canon_overlap_many() {
        let cls = class(&[
            ('c', 'f'), ('a', 'g'), ('d', 'j'), ('a', 'c'),
            ('m', 'p'), ('l', 's'),
        ]);
        assert_eq!(cls.clone().canonicalize(), class(&[
            ('a', 'j'), ('l', 's'),
        ]));
    }

    #[test]
    fn class_canon_overlap_many_case_fold() {
        let cls = class(&[
            ('C', 'F'), ('A', 'G'), ('D', 'J'), ('A', 'C'),
            ('M', 'P'), ('L', 'S'), ('c', 'f'),
        ]);
        assert_eq!(cls.case_fold(), classi(&[
            ('a', 'j'), ('l', 's'),
        ]));
    }

    #[test]
    fn class_canon_overlap_boundary() {
        let cls = class(&[('x', 'z'), ('u', 'w')]);
        assert_eq!(cls.canonicalize(), class(&[
            ('u', 'z'),
        ]));
    }

    #[test]
    fn class_canon_extreme_edge_case() {
        let cls = class(&[('\x00', '\u{10FFFF}'), ('\x00', '\u{10FFFF}')]);
        assert_eq!(cls.canonicalize(), class(&[
            ('\x00', '\u{10FFFF}'),
        ]));
    }

    #[test]
    fn class_canon_singles() {
        let cls = class(&[('a', 'a'), ('b', 'b')]);
        assert_eq!(cls.canonicalize(), class(&[('a', 'b')]));
    }

    #[test]
    fn class_negate_single() {
        let cls = class(&[('a', 'a')]);
        assert_eq!(cls.negate(), class(&[
            ('\x00', '\x60'), ('\x62', '\u{10FFFF}'),
        ]));
    }

    #[test]
    fn class_negate_singles() {
        let cls = class(&[('a', 'a'), ('b', 'b')]);
        assert_eq!(cls.negate(), class(&[
            ('\x00', '\x60'), ('\x63', '\u{10FFFF}'),
        ]));
    }

    #[test]
    fn class_negate_multiples() {
        let cls = class(&[('a', 'c'), ('x', 'z')]);
        assert_eq!(cls.negate(), class(&[
            ('\x00', '\x60'), ('\x64', '\x77'), ('\x7b', '\u{10FFFF}'),
        ]));
    }

    #[test]
    fn class_negate_min_scalar() {
        let cls = class(&[('\x00', 'a')]);
        assert_eq!(cls.negate(), class(&[
            ('\x62', '\u{10FFFF}'),
        ]));
    }

    #[test]
    fn class_negate_max_scalar() {
        let cls = class(&[('a', '\u{10FFFF}')]);
        assert_eq!(cls.negate(), class(&[
            ('\x00', '\x60'),
        ]));
    }

    #[test]
    fn class_negate_everything() {
        let cls = class(&[('\x00', '\u{10FFFF}')]);
        assert_eq!(cls.negate(), class(&[]));
    }

    #[test]
    fn class_negate_everything_sans_one() {
        let cls = class(&[
            ('\x00', '\u{10FFFD}'), ('\u{10FFFF}', '\u{10FFFF}')
        ]);
        assert_eq!(cls.negate(), class(&[
            ('\u{10FFFE}', '\u{10FFFE}'),
        ]));
    }

    #[test]
    fn class_negate_surrogates_min() {
        let cls = class(&[('\x00', '\u{D7FF}')]);
        assert_eq!(cls.negate(), class(&[
            ('\u{E000}', '\u{10FFFF}'),
        ]));
    }

    #[test]
    fn class_negate_surrogates_min_edge() {
        let cls = class(&[('\x00', '\u{D7FE}')]);
        assert_eq!(cls.negate(), class(&[
            ('\u{D7FF}', '\u{10FFFF}'),
        ]));
    }

    #[test]
    fn class_negate_surrogates_max() {
        let cls = class(&[('\u{E000}', '\u{10FFFF}')]);
        assert_eq!(cls.negate(), class(&[
            ('\x00', '\u{D7FF}'),
        ]));
    }

    #[test]
    fn class_negate_surrogates_max_edge() {
        let cls = class(&[('\u{E001}', '\u{10FFFF}')]);
        assert_eq!(cls.negate(), class(&[
            ('\x00', '\u{E000}'),
        ]));
    }

    #[test]
    fn class_fold_retain_only_needed() {
        let cls = class(&[('A', 'Z'), ('a', 'z')]);
        assert_eq!(cls.case_fold(), classi(&[
            ('a', 'z'),
        ]));
    }

    #[test]
    fn class_fold_az() {
        let cls = class(&[('A', 'Z')]);
        assert_eq!(cls.case_fold(), classi(&[
            ('a', 'z'),
        ]));
    }

    #[test]
    fn class_fold_a_underscore() {
        let cls = class(&[('A', 'A'), ('_', '_')]);
        assert_eq!(cls.clone().canonicalize(), class(&[
            ('A', 'A'), ('_', '_'),
        ]));
        assert_eq!(cls.case_fold(), classi(&[
            ('_', '_'), ('a', 'a'),
        ]));
    }

    #[test]
    fn class_fold_a_equals() {
        let cls = class(&[('A', 'A'), ('=', '=')]);
        assert_eq!(cls.clone().canonicalize(), class(&[
            ('=', '='), ('A', 'A'),
        ]));
        assert_eq!(cls.case_fold(), classi(&[
            ('=', '='), ('a', 'a'),
        ]));
    }

    #[test]
    fn class_fold_no_folding_needed() {
        let cls = class(&[('\x00', '\x10')]);
        assert_eq!(cls.case_fold(), classi(&[
            ('\x00', '\x10'),
        ]));
    }

    #[test]
    fn quickcheck_negate() {
        fn prop(ranges: Vec<(char, char)>) -> bool {
            class(&ranges).canonicalize() == class(&ranges).negate().negate()
        }
        quickcheck(prop as fn(Vec<(char, char)>) -> bool);
    }
}
