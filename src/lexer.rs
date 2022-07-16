use std::fmt;

use crate::Position;
use crate::{Token, TokenType};
use TokenType::*;

pub struct Lexer<'a> {
    source: &'a str,
    ch: char,
    position: Position<'a>,
}

#[derive(Clone, PartialEq, Eq)]
pub struct LexError<'a> {
    message: String,
    start: Position<'a>,
    end: Position<'a>,
}

impl<'a> LexError<'a> {
    fn new(message: String, start: Position<'a>, end: Position<'a>) -> Self {
        Self {
            message,
            start,
            end,
        }
    }
}

impl<'a> fmt::Display for LexError<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}-{} {}", self.start, self.end, self.message)
    }
}

type LexResult<'a> = Result<Option<Token<'a>>, LexError<'a>>;

impl<'a> Lexer<'a> {
    pub fn new(source: &'a str) -> Self {
        Self {
            source,
            ch: source.chars().nth(0).unwrap_or('\0'),
            position: Position::new(&source),
        }
    }

    fn advance(&mut self) {
        self.position.advance();
        self.ch = self.source.chars().nth(self.position.index).unwrap_or('\0');
    }

    pub fn next_token(&mut self) -> LexResult {
        loop {
            match self.ch {
                ' ' | '\t' | '\r' => self.advance(),
                _ => break,
            }
        }

        let start = self.position;

        match self.ch {
            '0'..='9' => self.number(),
            '"' => self.string(),
            '\'' => self.char(),
            'a'..='z' | 'A'..='Z' | '_' | 'Α'..='ω' | '∞' => self.word(),
            '=' => {
                self.advance();
                Ok(Some(match self.ch {
                    '=' => {
                        self.advance();
                        Token::new(EqEq, start, self.position)
                    }
                    _ => Token::new(Eq, start, self.position),
                }))
            }
            '+' => {
                self.advance();
                Ok(Some(match self.ch {
                    '=' => {
                        self.advance();
                        Token::new(AddEq, start, self.position)
                    }
                    _ => Token::new(Add, start, self.position),
                }))
            }
            '-' => {
                self.advance();
                Ok(Some(match self.ch {
                    '=' => {
                        self.advance();
                        Token::new(SubEq, start, self.position)
                    }
                    _ => Token::new(Sub, start, self.position),
                }))
            }
            '*' => {
                self.advance();
                Ok(Some(match self.ch {
                    '=' => {
                        self.advance();
                        Token::new(MulEq, start, self.position)
                    }
                    _ => Token::new(Mul, start, self.position),
                }))
            }
            '/' => {
                self.advance();
                Ok(Some(match self.ch {
                    '=' => {
                        self.advance();
                        Token::new(DivEq, start, self.position)
                    }
                    _ => Token::new(Div, start, self.position),
                }))
            }
            '^' => {
                self.advance();
                Ok(Some(match self.ch {
                    '=' => {
                        self.advance();
                        Token::new(PowEq, start, self.position)
                    }
                    _ => Token::new(Pow, start, self.position),
                }))
            }
            '<' => {
                self.advance();
                Ok(Some(match self.ch {
                    '=' => {
                        self.advance();
                        Token::new(Lte, start, self.position)
                    }
                    _ => Token::new(Lt, start, self.position),
                }))
            }
            '>' => {
                self.advance();
                Ok(Some(match self.ch {
                    '=' => {
                        self.advance();
                        Token::new(Gte, start, self.position)
                    }
                    _ => Token::new(Gt, start, self.position),
                }))
            }
            '(' => {
                self.advance();
                Ok(Some(Token::new(LParen, start, self.position)))
            }
            ')' => {
                self.advance();
                Ok(Some(Token::new(RParen, start, self.position)))
            }
            '[' => {
                self.advance();
                Ok(Some(Token::new(LBracket, start, self.position)))
            }
            ']' => {
                self.advance();
                Ok(Some(Token::new(RBracket, start, self.position)))
            }
            '{' => {
                self.advance();
                Ok(Some(Token::new(LBrace, start, self.position)))
            }
            '}' => {
                self.advance();
                Ok(Some(Token::new(RBrace, start, self.position)))
            }
            ':' => {
                self.advance();
                Ok(Some(Token::new(Colon, start, self.position)))
            }
            '\n' | ';' => {
                self.advance();
                Ok(Some(Token::new(Newline, start, self.position)))
            }
            '\0' => Ok(None),
            _ => Err(LexError::new(
                format!("Illegal character: '{}'", self.ch),
                start,
                self.position,
            )),
        }
    }

    fn number(&mut self) -> LexResult {
        let start = self.position;
        let mut num_str: String = self.ch.to_string();
        let mut decimals = 0;
        self.advance();

        while "0123456789.".contains(self.ch) {
            if self.ch == '.' {
                decimals += 1;
            }
            num_str.push(self.ch);
            self.advance();
        }

        if decimals > 0 {
            match num_str.parse::<f64>() {
                Ok(x) => Ok(Some(Token::new(Float(x), start, self.position))),
                Err(_) => Err(LexError::new(
                    "Invalid float".to_string(),
                    start,
                    self.position,
                )),
            }
        } else {
            match num_str.parse::<u32>() {
                Ok(x) => Ok(Some(Token::new(Int(x), start, self.position))),
                Err(_) => Err(LexError::new(
                    "Invalid int".to_string(),
                    start,
                    self.position,
                )),
            }
        }
    }

    fn string(&mut self) -> LexResult {
        let start = self.position;
        self.advance();
        let mut string = String::new();
        let mut escape = false;

        while self.ch != '"' {
            if self.ch == '\\' {
                escape = true;
                self.advance();
                continue;
            }
            string.push(if escape {
                escape = false;
                match self.ch {
                    'n' => '\n',
                    't' => '\t',
                    'r' => '\r',
                    '\\' => '\\',
                    ch => ch,
                }
            } else {
                self.ch
            });
            self.advance();
        }
        self.advance();

        Ok(Some(Token::new(Str(string), start, self.position)))
    }

    fn char(&mut self) -> LexResult {
        let start = self.position;
        self.advance();

        let ch = match self.ch {
            '\\' => {
                self.advance();
                match self.ch {
                    'n' => '\n',
                    't' => '\t',
                    'r' => '\r',
                    '\\' => '\\',
                    ch => ch,
                }
            }
            ch => ch,
        };
        self.advance();

        if self.ch != '\'' {
            return Err(LexError::new(
                "Char is too long".to_string(),
                start,
                self.position,
            ));
        }
        self.advance();

        Ok(Some(Token::new(Char(ch), start, self.position)))
    }

    fn word(&mut self) -> LexResult {
        let start = self.position;
        let mut word: String = self.ch.to_string();
        self.advance();

        while self.ch != '\0' {
            match self.ch {
                'a'..='z' | 'A'..='Z' | '0'..='9' | '_' | 'Α'..='ω' | '∞' => {
                    word.push(self.ch);
                    self.advance();
                }
                _ => break,
            };
        }

        Ok(Some(Token::new(
            match word.as_str() {
                "true" => Bool(true),
                "false" => Bool(false),
                "not" => Not,
                "and" => And,
                "or" => Or,
                "in" => In,
                "if" => If,
                "else" => Else,
                "while" => While,
                "for" => For,
                "return" => Return,
                _ => Identifier(word),
            },
            start,
            self.position,
        )))
    }
}
