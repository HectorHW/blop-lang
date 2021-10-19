use std::cmp::Ordering;
use std::fmt::{Debug, Display, Formatter};
use std::mem;

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

    Equals,

    Number(i64),
    Name(String),

    Print,
    Var,
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

pub fn tokenize(input: &str) -> Result<Vec<(Index, TokenKind, Index)>, String> {
    use TokenKind::*;
    let mut result = Vec::new();
    let mut identation = vec![0];

    result.push(Token {
        position: get_index(0, 0, 0),
        kind: BeginBlock,
    });

    let mut input_iterator = input.char_indices().peekable();

    let mut current_identation;
    let mut is_reading_indentation = true;

    let mut line_number = 0;
    let mut line_start = 0;

    while let Some((character_idx, character)) = input_iterator.peek() {
        let character = *character;
        let character_idx = *character_idx;
        if is_reading_indentation {
            current_identation = 0;

            while let Some((idx, symbol)) = input_iterator.peek() {
                match symbol {
                    ' ' => {
                        current_identation += 1;
                    }
                    '\n' => {
                        line_start = idx + 1;
                        line_number += 1;
                        current_identation = 0;
                    }
                    _ => break,
                }
            }

            let previous_identation_level = *identation.last().unwrap_or(&0);
            match previous_identation_level.cmp(&current_identation) {
                Ordering::Less => {
                    identation.push(current_identation);
                    result.push(Token {
                        position: get_index(character_idx, line_number, line_start),
                        kind: BeginBlock,
                    });
                }
                Ordering::Equal => {

                    //do nothing, continue parsing current block
                }
                Ordering::Greater => {
                    while let Some(identation_level) = identation.last() {
                        match identation_level.cmp(&current_identation) {
                            Ordering::Less => {
                                return Err(format!(
                                    "unconsistent identation level on line {}",
                                    line_number + 1
                                ))
                            }
                            Ordering::Equal => {
                                break;
                            }
                            Ordering::Greater => {
                                identation.pop();
                                result.push(Token {
                                    position: get_index(character_idx, line_number, line_start),
                                    kind: EndBlock,
                                });
                            }
                        }
                    }
                }
            }
            is_reading_indentation = false;
            continue;
        }

        macro_rules! token {
            ($kind:expr) => {{
                Token {
                    position: get_index(character_idx, line_number, line_start),
                    kind: $kind,
                }
            }};
        }

        match character {
            ' ' => {
                //this is not identation, skip space
                input_iterator.next();
            }
            '\n' => {
                match result.last() {
                    Some(x) if mem::discriminant(&x.kind) == mem::discriminant(&LineEnd) => {}
                    _ => {
                        result.push(token!(LineEnd));
                    }
                }
                input_iterator.next();
                line_number += 1;
                line_start = character_idx + 1;
                is_reading_indentation = true;
            }

            '+' => {
                result.push(token!(Plus));
                input_iterator.next();
            }
            '-' => {
                result.push(token!(Minus));
                input_iterator.next();
            }
            '*' => {
                result.push(token!(Star));
                input_iterator.next();
            }
            '/' => {
                result.push(token!(Slash));
                input_iterator.next();
            }

            x if x.is_numeric() => {
                let start_idx = character_idx;

                while let Some((_, character)) = input_iterator.peek() {
                    if character.is_numeric() {
                        input_iterator.next();
                    } else {
                        break;
                    }
                }
                let end_idx = input_iterator.peek().unwrap_or(&(input.len(), '\0')).0;
                let number: i64 = input[start_idx..end_idx].parse().unwrap();
                result.push(token!(Number(number)));
            }

            x if x.is_alphabetic() => {
                let start_idx = character_idx;

                while let Some((_, character)) = input_iterator.peek() {
                    if character.is_alphanumeric() || character == &'_' {
                        input_iterator.next();
                    } else {
                        break;
                    }
                }
                let end_idx = input_iterator.peek().unwrap_or(&(input.len(), '\0')).0;
                let name = input[start_idx..end_idx].to_string();

                result.push(match name.as_str() {
                    "print" => token!(Print),
                    "var" => token!(Var),
                    _ => token!(Name(name)),
                })
            }

            '(' => {
                result.push(token!(LParen));
                input_iterator.next();
            }
            ')' => {
                result.push(token!(RParen));
                input_iterator.next();
            }

            '=' => {
                result.push(token!(Equals));
                input_iterator.next();
            }

            any_other => {
                return Err(format!(
                    "unexpected character {} at {}",
                    any_other,
                    get_index(character_idx, line_number, line_start)
                ))
            }
        }
    }
    while identation.last().is_some() {
        result.push(Token {
            position: get_index(input.len(), line_number, line_start),
            kind: EndBlock,
        });
        identation.pop();
    }

    Ok(result
        .into_iter()
        .map(|x| {
            let position = x.position;
            let kind = x.kind;
            (position, kind, position)
        })
        .collect())
}

fn get_index(current_pos: usize, line_number: usize, line_start: usize) -> Index {
    Index(line_number + 1, current_pos - line_start + 1)
}
