use std::cmp::Ordering;
use std::collections::HashMap;
use std::fmt::{Debug, Display, Formatter};
use std::iter::Peekable;
use std::mem;
use std::str::CharIndices;

#[derive(Copy, Clone, Debug, Default, PartialEq, Eq, Hash)]
pub struct Index(pub usize, pub usize);

impl Display for Index {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:{}", self.0, self.1)
    }
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct Token {
    pub position: Index,
    pub kind: TokenKind,
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
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
    Mod,
    Power,

    And,
    Or,
    Not,

    Comma,

    CompareEquals,
    CompareNotEquals,
    CompareGreater,
    CompareGreaterEqual,
    CompareLess,
    CompareLessEqual,

    Equals,

    Number(i64),
    Name(String),
    ConstString(String),

    Print,
    Assert,
    Var,
    If,
    Elif,
    Else,
    Def,
    Pass,
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
                TokenKind::ConstString(s) => format!("[STRING {}]", s),
                TokenKind::Print => "(PRINT)".to_string(),
                TokenKind::LParen => "<(>".to_string(),
                TokenKind::RParen => "<)>".to_string(),
                TokenKind::Var => "(VAR)".to_string(),
                TokenKind::Equals => "(=)".to_string(),
                TokenKind::CompareEquals => "(?=)".to_string(),
                TokenKind::CompareNotEquals => "!=".to_string(),
                TokenKind::CompareGreater => ">".to_string(),
                TokenKind::CompareGreaterEqual => ">=".to_string(),
                TokenKind::CompareLess => "<".to_string(),
                TokenKind::CompareLessEqual => "<=".to_string(),
                TokenKind::If => "(if)".to_string(),
                TokenKind::Else => "(else)".to_string(),
                TokenKind::Assert => "(assert)".to_string(),
                TokenKind::Def => "(def)".to_string(),
                TokenKind::Comma => "<,>".to_string(),
                TokenKind::Elif => "(elif)".to_string(),
                TokenKind::Mod => "(mod)".to_string(),
                TokenKind::Power => "(**)".to_string(),
                TokenKind::Pass => "(pass)".to_string(),
                TokenKind::Or => "(or)".to_string(),
                TokenKind::And => "(and)".to_string(),
                TokenKind::Not => "(not)".to_string(),
            }
        )
    }
}

impl Token {
    pub fn get_string(&self) -> Option<&String> {
        match &self.kind {
            TokenKind::Name(n) => Some(n),
            TokenKind::ConstString(s) => Some(s),
            _ => None,
        }
    }

    pub fn get_number(&self) -> Option<i64> {
        match &self.kind {
            TokenKind::Number(n) => Some(*n),
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
    brackets: Vec<Token>,
}

impl<'input> Lexer<'input> {
    fn new(input_string: &str) -> Lexer {
        use self::TokenKind::*;
        let keywords = vec![
            ("assert", Assert),
            ("print", Print),
            ("var", Var),
            ("if", If),
            ("elif", Elif),
            ("else", Else),
            ("def", Def),
            ("mod", Mod),
            ("pass", Pass),
            ("or", Or),
            ("and", And),
            ("not", Not),
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
            brackets: vec![],
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

    fn can_indent(&self) -> bool {
        self.brackets.is_empty()
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
                if !self.can_indent() {
                    is_reading_indentation = false;
                    continue;
                }

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
                    if self.can_indent() {
                        match result.last() {
                            Some(x)
                                if mem::discriminant(&x.kind) == mem::discriminant(&LineEnd) => {}
                            _ => {
                                result.push(token!(LineEnd));
                            }
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

                '#' => {
                    self.read_while(&|c| c != '\n');
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
                    let index = self.compute_index();
                    self.input_iterator.next();
                    match self.input_iterator.peek() {
                        Some((_, '*')) => {
                            result.push(token!(index, Power));
                            self.input_iterator.next();
                        }
                        _ => {
                            result.push(token!(index, Star));
                        }
                    }
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

                    self.read_while(&|c| c.is_alphanumeric() || c == '_');

                    let end_idx = self.compute_input_shift();
                    let name = &self.input_string[start_idx..end_idx];
                    //keywords
                    result.push(token!(token_index, self.keyword_or_name(name)))
                }

                '`' => {
                    self.input_iterator.next(); //skip `
                    let start_idx = self.compute_input_shift();
                    let token_index = self.compute_index();

                    self.read_while(&|c| c != '`');
                    let end_idx = self.compute_input_shift();
                    self.input_iterator.next(); //skip closing `
                    let name = &self.input_string[start_idx..end_idx];
                    result.push(token!(token_index, TokenKind::Name(name.to_string())))
                }

                '"' => {
                    let token_index = self.compute_index();
                    self.input_iterator.next(); //skip opening "
                    let start_idx = self.compute_input_shift();
                    self.read_while(&|c| c != '"');
                    if self.input_iterator.peek().is_none() {
                        return Err(format!("unterminated string at [{}]", token_index));
                    }
                    let end_idx = self.compute_input_shift();
                    self.input_iterator.next(); //skip closing "
                    let s = self.input_string[start_idx..end_idx].to_string();
                    result.push(token!(token_index, TokenKind::ConstString(s)));
                }

                '(' => {
                    let token = token!(LParen);
                    result.push(token.clone());
                    self.brackets.push(token);
                    self.input_iterator.next();
                }
                ')' => {
                    let token = token!(RParen);
                    if let Some(TokenKind::LParen) = self.brackets.last().map(|t| t.kind.clone()) {
                        self.brackets.pop();
                        result.push(token);
                        self.input_iterator.next();
                    } else {
                        return Err(format!(
                            "encountered unbalanced `)` at [{}] (matched with {})",
                            token.position,
                            match self.brackets.last() {
                                None => {
                                    "nothing".to_string()
                                }
                                Some(t) => {
                                    format!("{} at [{}]", t.kind, t.position)
                                }
                            }
                        ));
                    }
                }

                '=' => {
                    let possible_token_index = self.compute_index();
                    self.input_iterator.next(); //skip first =
                    match self.input_iterator.peek().copied() {
                        Some((_, '=')) => {
                            //==
                            result.push(token!(possible_token_index, CompareEquals));
                            self.input_iterator.next(); //skip second =
                        }
                        _ => {
                            result.push(token!(possible_token_index, Equals));
                        }
                    }
                }

                '!' => {
                    let possible_token_index = self.compute_index();
                    self.input_iterator.next(); //skip first !
                    match self.input_iterator.peek().copied() {
                        Some((_, '=')) => {
                            //reading !=
                            result.push(token!(possible_token_index, CompareNotEquals));
                            self.input_iterator.next(); //skip =
                        }
                        Some((_, any_other)) => {
                            return Err(format!(
                                "unexpected character {} at {}",
                                any_other,
                                self.compute_index()
                            ))
                        }
                        _ => {
                            return Err("unexpected end after reading !".to_string());
                        }
                    }
                }

                '<' => {
                    let possible_token_index = self.compute_index();
                    self.input_iterator.next(); //skip <
                    match self.input_iterator.peek().copied() {
                        Some((_, '=')) => {
                            // <=
                            result.push(token!(possible_token_index, CompareLessEqual));
                            self.input_iterator.next(); //skip =
                        }
                        _ => {
                            result.push(token!(possible_token_index, CompareLess));
                        }
                    }
                }

                '>' => {
                    let possible_token_index = self.compute_index();
                    self.input_iterator.next(); //skip >
                    match self.input_iterator.peek().copied() {
                        Some((_, '=')) => {
                            // >=
                            result.push(token!(possible_token_index, CompareGreaterEqual));
                            self.input_iterator.next(); //skip =
                        }
                        _ => {
                            result.push(token!(possible_token_index, CompareGreater));
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

        if !self.brackets.is_empty() {
            let msg = self
                .brackets
                .iter()
                .map(|item| format!("unbalanced {} at [{}]", item.kind, item.position))
                .collect::<Vec<_>>();
            let msg = msg.join("\n");
            return Err(msg);
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
        while let Some((idx, character)) = self.input_iterator.peek() {
            let character = *character;
            if predicate(character) {
                if character == '\n' {
                    self.line_number += 1;
                    self.line_start = idx + 1;
                    eprintln!("warning: encountered newline inside token");
                }

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
                '#' => {
                    self.read_while(&|c| c != '\n');
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
