use std::cmp::Ordering;
use std::collections::HashMap;
use std::fmt::{Debug, Display, Formatter};
use std::iter::Peekable;
use std::mem;
use std::str::CharIndices;

#[derive(Copy, Clone, Debug, Default)]
pub struct Index(usize, usize);

impl Display for Index {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:{}", self.0, self.1)
    }
}

#[derive(Clone, Debug)]
pub struct Token {
    pub position: Index,
    pub kind: TokenKind,
}

#[derive(Clone, Debug)]
pub enum TokenKind {
    BeginBlock,
    EndBlock,
    LineEnd,

    LParen,
    RParen,

    Plus,
    Minus,
    Star,
    Slash,

    Comma,

    TestEquals,

    Equals,

    Number(i64),
    Name(String),

    Print,
    Assert,
    Var,
    If,
    Else,
    Def,
}

impl Display for TokenKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match &self {
                TokenKind::BeginBlock => "<BLOCK>".to_string(),
                TokenKind::EndBlock => "<BLOCK_END>".to_string(),
                TokenKind::LineEnd => "<EOL>".to_string(),
                TokenKind::Plus => "(+)".to_string(),
                TokenKind::Minus => "(-)".to_string(),
                TokenKind::Star => "(*)".to_string(),
                TokenKind::Slash => "(/)".to_string(),
                TokenKind::Number(n) => format!("[NUMBER {}]", n),
                TokenKind::Name(s) => format!("[NAME {}]", s),
                TokenKind::Print => "(PRINT)".to_string(),
                TokenKind::LParen => "<(>".to_string(),
                TokenKind::RParen => "<)>".to_string(),
                TokenKind::Var => "(VAR)".to_string(),
                TokenKind::Equals => "(=)".to_string(),
                TokenKind::TestEquals => "(?=)".to_string(),
                TokenKind::If => "(if)".to_string(),
                TokenKind::Else => "(else)".to_string(),
                TokenKind::Assert => "(assert)".to_string(),
                TokenKind::Def => "(def)".to_string(),
                TokenKind::Comma => "<,>".to_string(),
            }
        )
    }
}

impl Token {
    pub fn get_string(&self) -> Option<&String> {
        match &self.kind {
            TokenKind::Name(n) => Some(n),
            _ => None,
        }
    }
}

pub fn tokenize(input: &str) -> Result<Vec<Token>, String> {
    let mut lexer = Lexer::new(input);
    lexer.tokenize()
}

struct Lexer<'input> {
    input_string: &'input str,
    input_iterator: Peekable<CharIndices<'input>>,
    line_number: usize,
    line_start: usize,
    indentation: Vec<usize>,
    keywords: HashMap<String, TokenKind>,
}

impl<'input> Lexer<'input> {
    fn new(input_string: &str) -> Lexer {
        use self::TokenKind::*;
        let keywords = vec![
            ("assert", Assert),
            ("print", Print),
            ("var", Var),
            ("if", If),
            ("else", Else),
            ("def", Def),
        ]
        .into_iter()
        .map(|(k, v)| (k.to_string(), v))
        .collect::<HashMap<String, TokenKind>>();

        Lexer {
            input_string,
            input_iterator: input_string.char_indices().peekable(),
            line_number: 0,
            line_start: 0,
            indentation: vec![],
            keywords,
        }
    }

    fn compute_index(&mut self) -> Index {
        let current_pos = self.compute_input_shift();
        Index(self.line_number + 1, current_pos - self.line_start + 1)
    }

    fn compute_input_shift(&mut self) -> usize {
        self.input_iterator
            .peek()
            .unwrap_or(&(self.input_string.len(), '\0'))
            .0
    }

    fn keyword_or_name(&self, s: &str) -> TokenKind {
        self.keywords
            .get(s)
            .cloned()
            .unwrap_or_else(|| TokenKind::Name(s.to_string()))
    }

    pub fn tokenize(&mut self) -> Result<Vec<Token>, String> {
        use TokenKind::*;
        let mut result = vec![Token {
            position: self.compute_index(),
            kind: BeginBlock,
        }];

        self.indentation.push(0);

        let mut is_reading_indentation = true;

        while let Some((_, character)) = self.input_iterator.peek() {
            let character = *character;
            if is_reading_indentation {
                let mut produced_tokens = self.read_identation()?;

                let last = result.pop().unwrap();
                if mem::discriminant(&last.kind) == mem::discriminant(&LineEnd) {
                    //never put block end on top of newline
                    result.append(&mut produced_tokens);
                    result.push(last);
                } else {
                    result.push(last);
                    result.append(&mut produced_tokens);
                }
                is_reading_indentation = false;
                continue;
            }

            macro_rules! token {
                ($kind:expr) => {{
                    Token {
                        position: self.compute_index(),
                        kind: $kind,
                    }
                }};
                ($position:expr, $kind:expr) => {
                    Token {
                        position: $position,
                        kind: $kind,
                    }
                };
            }

            match character {
                ' ' => {
                    //this is not indentation, skip space
                    self.input_iterator.next();
                }
                '\n' => {
                    match result.last() {
                        Some(x) if mem::discriminant(&x.kind) == mem::discriminant(&LineEnd) => {}
                        _ => {
                            result.push(token!(LineEnd));
                        }
                    }
                    self.input_iterator.next(); //skip newline
                    self.line_number += 1;
                    self.line_start = self
                        .input_iterator
                        .peek()
                        .unwrap_or(&(self.input_string.len(), '\0'))
                        .0;
                    is_reading_indentation = true;
                }

                '+' => {
                    result.push(token!(Plus));
                    self.input_iterator.next();
                }
                '-' => {
                    result.push(token!(Minus));
                    self.input_iterator.next();
                }
                '*' => {
                    result.push(token!(Star));
                    self.input_iterator.next();
                }
                '/' => {
                    result.push(token!(Slash));
                    self.input_iterator.next();
                }

                ',' => {
                    result.push(token!(Comma));
                    self.input_iterator.next();
                }

                x if x.is_numeric() => {
                    let start_idx = self.compute_input_shift();
                    let token_index = self.compute_index();

                    self.read_while(&|c| c.is_numeric());

                    let end_idx = self.compute_input_shift();
                    let number: i64 = self.input_string[start_idx..end_idx].parse().unwrap();
                    result.push(token!(token_index, Number(number)));
                }

                x if x.is_alphabetic() => {
                    let start_idx = self.compute_input_shift();
                    let token_index = self.compute_index();

                    self.read_while(&|c| c.is_alphanumeric());

                    let end_idx = self.compute_input_shift();
                    let name = &self.input_string[start_idx..end_idx];
                    //keywords
                    result.push(token!(token_index, self.keyword_or_name(name)))
                }

                '(' => {
                    result.push(token!(LParen));
                    self.input_iterator.next();
                }
                ')' => {
                    result.push(token!(RParen));
                    self.input_iterator.next();
                }

                '=' => {
                    result.push(token!(Equals));
                    self.input_iterator.next();
                }

                '?' => {
                    let possible_token_index = self.compute_index();
                    self.input_iterator.next(); //skip ?
                    let next_pair = self.input_iterator.peek().copied();
                    match next_pair {
                        Some((_, '=')) => {
                            result.push(token!(possible_token_index, TestEquals));
                            self.input_iterator.next();
                        }
                        Some((_, c)) => {
                            return Err(format!(
                                "unexpected character {} at {}",
                                c,
                                self.compute_index()
                            ));
                        }

                        None => {
                            return Err(format!("unexpected end at {}", self.compute_index()));
                        }
                    }
                }

                any_other => {
                    return Err(format!(
                        "unexpected character {} at {}",
                        any_other,
                        self.compute_index()
                    ))
                }
            }
        }

        while !self.indentation.is_empty() {
            result.push(Token {
                position: self.compute_index(),
                kind: EndBlock,
            });
            self.indentation.pop();
        }

        Ok(result)
    }

    fn read_while<F: (Fn(char) -> bool)>(&mut self, predicate: &F) {
        while let Some((_, character)) = self.input_iterator.peek() {
            let character = *character;
            if predicate(character) {
                self.input_iterator.next();
            } else {
                break;
            }
        }
    }

    fn read_identation(&mut self) -> Result<Vec<Token>, String> {
        use TokenKind::{BeginBlock, EndBlock};
        let mut result = vec![];
        let mut current_indentation = 0;

        while let Some((idx, symbol)) = self.input_iterator.peek() {
            match symbol {
                ' ' => {
                    current_indentation += 1;
                    self.input_iterator.next();
                }
                '\n' => {
                    self.line_start = idx + 1;
                    self.line_number += 1;
                    current_indentation = 0;
                    self.input_iterator.next();
                }
                _ => {
                    break;
                }
            }
        }

        let previous_indentation_level = *self.indentation.last().unwrap_or(&0);
        match previous_indentation_level.cmp(&current_indentation) {
            Ordering::Less => {
                self.indentation.push(current_indentation);
                result.push(Token {
                    position: self.compute_index(),
                    kind: BeginBlock,
                });
            }

            Ordering::Equal => {
                //do nothing, continue parsing current block
            }

            Ordering::Greater => {
                while let Some(indentation_level) = self.indentation.last() {
                    match indentation_level.cmp(&current_indentation) {
                        Ordering::Less => {
                            return Err(format!(
                                "unconsistent indentation level on line {}",
                                self.line_number + 1
                            ))
                        }

                        Ordering::Equal => {
                            break;
                        }

                        Ordering::Greater => {
                            self.indentation.pop();
                            result.push(Token {
                                position: self.compute_index(),
                                kind: EndBlock,
                            });
                        }
                    }
                }
            }
        }
        Ok(result)
    }
}
