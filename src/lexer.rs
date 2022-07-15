use std::str::Chars;

use crate::Position;
use crate::{Token, TokenType};
use TokenType::*;

pub struct Lexer<'a> {
    source: &'a str,
    chars: Chars<'a>,
    current_char: char,
    position: Position<'a>,
}

impl<'a> Lexer<'a> {
    pub fn new(source: &'a str) -> Self {
        let chars = source.chars();
        Self {
            source,
            chars,
            current_char: chars.next().unwrap_or('\0'),
            position: Position::new(&source),
        }
    }

    fn advance(&mut self) {
        let next = self.chars.next();
        self.current_char = match next {
            Some(ch) => ch,
            None => '\0',
        };
        self.position.advance();
    }

    pub fn lex(&mut self) -> Vec<Token> {
        let mut tokens: Vec<Token> = vec![];
        let mut token = self.next_token();
        while token.ty != EOF {
            tokens.push(token);
            token = self.next_token();
        }
        tokens.push(token);
        tokens
    }

    pub fn next_token(&mut self) -> Token {
        let start = self.position;
        match self.current_char {
            ' ' | '\t' | '\r' => {
                self.advance();
                Token::new(Whitespace, start, self.position)
            }
            '0'..='9' => self.number(),
            '"' => self.string(),
            '\'' => self.char(),
            'a'..='z' | 'A'..='Z' | '_' | 'Α'..='ω' | '∞' => self.word(),
            '=' => {
                self.advance();
                match self.current_char {
                    '=' => {
                        self.advance();
                        Token::new(EqEq, start, self.position)
                    }
                    _ => Token::new(Eq, start, self.position),
                }
            }
            '+' => {
                self.advance();
                match self.current_char {
                    '=' => {
                        self.advance();
                        Token::new(AddEq, start, self.position)
                    }
                    _ => Token::new(Add, start, self.position),
                }
            }
            '-' => {
                self.advance();
                match self.current_char {
                    '=' => {
                        self.advance();
                        Token::new(SubEq, start, self.position)
                    }
                    _ => Token::new(Sub, start, self.position),
                }
            }
            '*' => {
                self.advance();
                match self.current_char {
                    '=' => {
                        self.advance();
                        Token::new(MulEq, start, self.position)
                    }
                    _ => Token::new(Mul, start, self.position),
                }
            }
            '/' => {
                self.advance();
                match self.current_char {
                    '=' => {
                        self.advance();
                        Token::new(DivEq, start, self.position)
                    }
                    _ => Token::new(Div, start, self.position),
                }
            }
            '^' => {
                self.advance();
                match self.current_char {
                    '=' => {
                        self.advance();
                        Token::new(PowEq, start, self.position)
                    }
                    _ => Token::new(Pow, start, self.position),
                }
            }
            '<' => {
                self.advance();
                match self.current_char {
                    '=' => {
                        self.advance();
                        Token::new(Lte, start, self.position)
                    }
                    _ => Token::new(Lt, start, self.position),
                }
            }
            '>' => {
                self.advance();
                match self.current_char {
                    '=' => {
                        self.advance();
                        Token::new(Gte, start, self.position)
                    }
                    _ => Token::new(Gt, start, self.position),
                }
            }
            '(' => {
                self.advance();
                Token::new(LParen, start, self.position)
            }
            ')' => {
                self.advance();
                Token::new(RParen, start, self.position)
            }
            '[' => {
                self.advance();
                Token::new(LBracket, start, self.position)
            }
            ']' => {
                self.advance();
                Token::new(RBracket, start, self.position)
            }
            '{' => {
                self.advance();
                Token::new(LBrace, start, self.position)
            }
            '}' => {
                self.advance();
                Token::new(RBrace, start, self.position)
            }
            '\n' | ';' => {
                self.advance();
                Token::new(Newline, start, self.position)
            }
            '\0' => Token::new(EOF, start, self.position),
            _ => panic!("Illegal character: '{}'", self.current_char),
        }
    }

    fn number(&mut self) -> Token {
        let start = self.position;
        let mut num_str: String = self.current_char.to_string();
        let mut decimals = 0;
        self.advance();

        while "0123456789.".contains(self.current_char) {
            if self.current_char == '.' {
                decimals += 1;
            }
            num_str.push(self.current_char);
            self.advance();
        }

        if decimals > 0 {
            Token::new(Float(num_str.parse::<f64>().unwrap()), start, self.position)
        } else {
            Token::new(Int(num_str.parse::<u32>().unwrap()), start, self.position)
        }
    }

    fn string(&mut self) -> Token {
        let start = self.position;
        self.advance();
        let mut string = String::new();
        let mut escape = false;

        while self.current_char != '"' {
            if self.current_char == '\\' {
                escape = true;
                self.advance();
                continue;
            }
            string.push(if escape {
                escape = false;
                match self.current_char {
                    'n' => '\n',
                    't' => '\t',
                    'r' => '\r',
                    '\\' => '\\',
                    ch => ch,
                }
            } else {
                self.current_char
            });
            self.advance();
        }
        self.advance();

        Token::new(Str(&string), start, self.position)
    }

    fn char(&mut self) -> Token {
        let start = self.position;
        self.advance();

        let ch = match self.current_char {
            '\\' => {
                self.advance();
                match self.current_char {
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

        if self.current_char != '\'' {
            panic!("char is too long");
        }
        self.advance();

        Token::new(Char(ch), start, self.position)
    }

    fn word(&mut self) -> Token {
        let start = self.position;
        let mut word: String = self.current_char.to_string();
        self.advance();

        while self.current_char != '\0' {
            match self.current_char {
                'a'..='z' | 'A'..='Z' | '0'..='9' | '_' | 'Α'..='ω' | '∞' => {
                    word.push(self.current_char);
                    self.advance();
                }
                _ => break,
            };
        }

        Token::new(
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
                "in" => In,
                "return" => Return,
                _ => Identifier(&word),
            },
            start,
            self.position,
        )
    }
}
