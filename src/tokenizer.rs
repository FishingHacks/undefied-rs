use std::{fmt::Display, str::Chars};

use crate::{error::err, utils::get_stdpath};

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct Loc {
    pub line: usize,
    pub char: usize,
    pub file: String,
}

impl Loc {
    pub fn new(file: &String, line: usize, char: usize) -> Self {
        Self {
            line,
            char,
            file: file.clone(),
        }
    }
}

impl Display for Loc {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut stdpath = get_stdpath().to_str().unwrap().to_string();
        stdpath += "/";
        let file = self.file.replace(&stdpath, "@");
        
        f.write_fmt(format_args!("{}:{}:{}", file, self.line, self.char))
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct IntegerToken {
    pub loc: Loc,
    pub value: u64,
}
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct StringToken {
    pub loc: Loc,
    pub value: String,
    pub is_cstr: bool,
}
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CharToken {
    pub loc: Loc,
    pub char: char,
}
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct WordToken {
    pub loc: Loc,
    pub value: String,
}
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CommentToken {
    pub loc: Loc,
    pub len: usize,
}
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct NoneToken {
    pub loc: Loc,
    pub len: usize,
}
#[derive(Clone, PartialEq, Eq)]
pub enum Token {
    IntegerToken(IntegerToken),
    StringToken(StringToken),
    CharToken(CharToken),
    WordToken(WordToken),
    CommentToken(CommentToken),
}

fn escape_str(str: &String) -> String {
    let mut new_str = String::with_capacity(str.len());

    for char in str.chars() {
        match char {
            '\n' => new_str.push_str("\\n"),
            '\r' => new_str.push_str("\\r"),
            '\t' => new_str.push_str("\\t"),
            '\\' => new_str.push_str("\\\\"),
            _ => new_str.push(char),
        }
    }

    new_str
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::CharToken(CharToken { char, .. }) => f.write_fmt(format_args!("'{}'", escape_str(&char.to_string()))),
            Self::CommentToken(..) => f.write_str(""),
            Self::IntegerToken(IntegerToken { value, .. }) => f.write_str(&value.to_string()),
            Self::StringToken(StringToken { value, is_cstr: true, .. }) => f.write_fmt(format_args!("\"{}\"c", value)),
            Self::StringToken(StringToken { value, is_cstr: false, .. }) => f.write_fmt(format_args!("\"{}\"", value)),
            Self::WordToken(WordToken { value, .. }) => f.write_str(&value),
        }
    }
}

impl Token {
    pub fn comment_token(loc: Loc, len: usize) -> Self {
        Self::CommentToken(CommentToken { loc, len })
    }

    pub fn int_token(loc: Loc, value: u64) -> Self {
        Self::IntegerToken(IntegerToken { loc, value })
    }

    pub fn word_token(loc: Loc, value: String) -> Self {
        Self::WordToken(WordToken { loc, value })
    }

    pub fn str_token(loc: Loc, value: String, is_cstr: bool) -> Self {
        Self::StringToken(StringToken {
            loc,
            value,
            is_cstr,
        })
    }

    pub fn char_token(loc: Loc, char: char) -> Self {
        Self::CharToken(CharToken { loc, char })
    }

    pub fn is_str_tok(&self) -> bool {
        matches!(self, Self::StringToken(..))
    }

    pub fn as_str_token(&self) -> Option<&StringToken> {
        match self {
            Self::StringToken(a) => Some(a),
            _ => None,
        }
    }

    pub fn as_word_token(&self) -> Option<&WordToken> {
        match self {
            Self::WordToken(a) => Some(a),
            _ => None,
        }
    }

    pub fn get_location(&self) -> &Loc {
        match self {
            Self::CharToken(CharToken { loc, .. })
            | Self::CommentToken(CommentToken { loc, .. })
            | Self::IntegerToken(IntegerToken { loc, .. })
            | Self::StringToken(StringToken { loc, .. })
            | Self::WordToken(WordToken { loc, .. }) => loc,
        }
    }

    pub fn get_location_mut(&mut self) -> &mut Loc {
        match self {
            Self::CharToken(CharToken { loc, .. })
            | Self::CommentToken(CommentToken { loc, .. })
            | Self::IntegerToken(IntegerToken { loc, .. })
            | Self::StringToken(StringToken { loc, .. })
            | Self::WordToken(WordToken { loc, .. }) => loc,
        }
    }
}

pub fn parse(tokens: String, file: &String) -> Vec<Token> {
    let lines = tokens.split('\n').map(|v| {
        if v.ends_with('\r') {
            &v[..v.len() - 2]
        } else {
            v
        }
    });

    let mut tokens: Vec<Token> = Vec::new();
    let mut value: String = String::new();
    let mut is_rec_char = false;
    
    for (l_idx, l) in lines.enumerate() {
        let l_idx = l_idx + 1;
        let mut is_rec_str = false;
        let mut is_escape: bool = false;
        let mut last_was_str_assign = false;
        value.clear();

        for (c_idx, c) in l.chars().enumerate() {
            let c_idx = c_idx + 1;
        
            if is_rec_str && is_escape {
                if is_rec_char && !value.is_empty() {
                    err(
                        &Loc::new(file, l_idx, c_idx),
                        "A character has to have exactly 1 character",
                    );
                }
                match c {
                    'n' => value.push('\n'),
                    'r' => value.push('\r'),
                    't' => value.push('\t'),
                    _ => value.push(c),
                }
                is_escape = false;
            } else if is_rec_str && c != '"' && !is_rec_char {
                if c == '\\' {
                    is_escape = true;
                } else {
                    value.push(c);
                }
                continue;
            } else if is_rec_str && c != '\'' && is_rec_char {
                if !value.is_empty() {
                    err(
                        &Loc::new(file, l_idx, c_idx),
                        "A character has to have exactly 1 character",
                    );
                }
                if c == '\\' {
                    is_escape = true;
                } else {
                    value.push(c);
                }
                continue;
            } else if c == '#' {
                tokens.push(Token::comment_token(
                    Loc::new(file, l_idx, c_idx),
                    l.len() - c_idx,
                ));
                break;
            } else if c == ' ' || c == '\t' {
                if value.is_empty() {
                    continue;
                }
                let loc = Loc::new(file, l_idx, c_idx - value.len());
                if let Some(num) = try_parse_num(&value) {
                    tokens.push(Token::int_token(loc, num));
                } else {
                    tokens.push(Token::word_token(loc, value.clone()));
                }
                value.clear();
            } else if c == '"' && (!is_rec_str || !is_rec_char) {
                if is_rec_str && !is_rec_char {
                    tokens.push(Token::str_token(
                        Loc::new(file, l_idx, c_idx - value.len()),
                        value.clone(),
                        false,
                    ));
                    value.clear();
                    is_rec_str = false;
                    is_escape = false;
                    last_was_str_assign = true;
                    continue;
                } else if !is_rec_str {
                    is_escape = false;
                    is_rec_str = true;
                    is_rec_char = false;
                    value.clear();
                }
            } else if c == 'c'
                && !is_rec_str
                && !tokens.is_empty()
                && tokens[tokens.len() - 1].is_str_tok()
                && !tokens[tokens.len() - 1].as_str_token().and_then(|f| Some(f.is_cstr)).unwrap_or_default()
                && last_was_str_assign
            {
                if let Some(tok) = tokens.pop() {
                    match tok {
                        Token::StringToken { 0: mut a } => {
                            a.is_cstr = true;
                            tokens.push(Token::StringToken(a));
                        }
                        _ => {
                            panic!("Unreachable: is_str_tok() returned true for a non-string token")
                        }
                    }
                }
            } else if c == '\'' && is_rec_str && is_rec_char {
                let first_char = value.chars().next();
                let char_amount = value.chars().count();
                if char_amount != 1 {
                    err(
                        &Loc::new(file, l_idx - if char_amount > 0 {char_amount - 1} else { 0 }, c_idx),
                        format!(
                            "A character has to have exactly 1 character, but found {} characters instead",
                            value.len()
                        ),
                    );
                }
                if let Some(char) = first_char {
                    tokens.push(Token::char_token(Loc::new(file, l_idx, c_idx - 2), char));
                }
                is_rec_str = false;
                is_rec_char = false;
                is_escape = false;
                value.clear();
            } else if c == '\'' && !is_rec_str {
                value.clear();
                is_rec_char = true;
                is_rec_str = true;
                is_escape = false;
            } else {
                value.push(c);
            }
            last_was_str_assign = false;
        }

        if l.is_empty() {
            continue;
        }
        let loc = Loc::new(file, l_idx, l.chars().count() - value.len());
        if is_rec_char {
            err(&loc, "Expected `'`, but found a new line")
        } else if is_rec_str {
            err(&loc, "Expected `\"`, but found a new line")
        }
        if value.is_empty() {
            continue;
        }
        if let Some(num) = try_parse_num(&value) {
            tokens.push(Token::int_token(loc, num));
        } else {
            tokens.push(Token::word_token(loc, value.clone()));
        }
        value.clear();
    }

    tokens
}

fn try_parse_num(value: &str) -> Option<u64> {
    if value.is_empty() {
        return None;
    }
    if value.starts_with("0x") {
        try_parse_num_hex(value)
    } else if value.starts_with("0b") {
        try_parse_num_bin(value)
    } else if value.starts_with("0o") {
        try_parse_num_oct(value)
    } else if value.starts_with("-") && value.len() > 1 {
        try_parse_int(&value[1..])
        .map(|value| if value == 0 { 0 } else { !(value - 1) })
    } else {
        try_parse_int(value)
    }
}

fn try_parse_int(value: &str) -> Option<u64> {
    let mut int: u64 = 0;
    for v in value.chars() {
        int *= 10;
        if !v.is_ascii_digit() {
            return None;
        }
        int += (v as u64) - ('0' as u64);
    }
    Some(int)
}
fn take_elements(mut iter: Chars) -> Chars<'_> {
    iter.next();
    iter.next();
    iter
}

fn try_parse_num_hex(value: &str) -> Option<u64> {
    let mut int: u64 = 0;
    for v in take_elements(value.chars()) {
        int *= 0x10;
        if !('0'..='f').contains(&v) {
            return None;
        }
        int += (v as u64) - ('0' as u64);
    }
    Some(int)
}
fn try_parse_num_bin(value: &str) -> Option<u64> {
    let mut int: u64 = 0;
    for v in take_elements(value.chars()) {
        int *= 0b10;
        if !('0'..='1').contains(&v) {
            return None;
        }
        int += (v as u64) - ('0' as u64);
    }
    Some(int)
}
fn try_parse_num_oct(value: &str) -> Option<u64> {
    let mut int: u64 = 0;
    for v in take_elements(value.chars()) {
        int *= 0o10;
        if !('0'..='7').contains(&v) {
            return None;
        }
        int += (v as u64) - ('0' as u64);
    }
    Some(int)
}
