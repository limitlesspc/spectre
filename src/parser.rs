use std::fmt;

use crate::{BinaryOp, Node, NodeType, Position, Token, TokenType, UnaryOp};
use TokenType::*;

pub struct Parser<'a> {
    tokens: Vec<Token<'a>>,
    token: Token<'a>,
    index: usize,
}

#[derive(Clone, PartialEq, Eq)]
pub struct ParseError<'a> {
    message: String,
    start: Position<'a>,
    end: Position<'a>,
}

impl<'a> ParseError<'a> {
    fn new(message: String, start: Position<'a>, end: Position<'a>) -> Self {
        Self {
            message,
            start,
            end,
        }
    }
}

impl<'a> fmt::Display for ParseError<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}-{} {}", self.start, self.end, self.message)
    }
}

pub type ParseResult<'a> = Result<Node<'a>, ParseError<'a>>;

impl<'a> Parser<'a> {
    pub fn new(tokens: Vec<Token<'a>>) -> Self {
        Self {
            token: tokens[0].clone(),
            tokens,
            index: 0,
        }
    }

    fn advance(&mut self) {
        self.index += 1;
        self.token = self
            .tokens
            .get(self.index)
            .unwrap_or(&Token::new(
                TokenType::EOF,
                Position::new(""),
                Position::new(""),
            ))
            .clone();
    }

    fn skip_newlines(&mut self) -> usize {
        let mut newlines: usize = 0;
        while self.token.ty == Newline {
            self.advance();
            newlines += 1;
        }
        newlines
    }

    pub fn parse(&mut self) -> ParseResult<'a> {
        self.statements()
    }

    fn statements(&mut self) -> ParseResult<'a> {
        let start = self.token.start;
        let mut statements: Vec<Node> = vec![];
        self.skip_newlines();

        statements.push(self.statement()?);

        let mut more_statements = true;

        loop {
            let newlines = self.skip_newlines();
            if newlines == 0 {
                more_statements = false;
            }

            if !more_statements || self.token.ty == RBrace {
                break;
            }

            let statement = self.statement()?;
            if statement.ty == NodeType::EOF {
                more_statements = false;
                continue;
            }
            statements.push(statement);
        }

        Ok(Node::new(
            NodeType::Statements(statements),
            start,
            self.token.end,
        ))
    }

    fn statement(&mut self) -> ParseResult<'a> {
        let start = self.token.start;
        match self.token.ty {
            Return => {
                self.advance();
                Ok(Node::new(
                    NodeType::Return(Box::new(self.expr()?)),
                    start,
                    self.token.end,
                ))
            }
            _ => self.expr(),
        }
    }

    fn expr(&mut self) -> ParseResult<'a> {
        let start = self.token.start;
        let node = self.or_expr()?;

        macro_rules! expr {
            ($(($token:tt, $op:tt)),*) => {
                match self.token.ty {
                    $(
                        $token => {
                            self.advance();
                            Ok(Node::new(NodeType::Binary(Box::new(node), BinaryOp::$op, Box::new(self.or_expr()?)), start, self.token.end))
                        }
                    )*,
                    _ => Ok(node),
                }
            };
        }

        expr!(
            (Eq, Eq),
            (AddEq, Add),
            (SubEq, Sub),
            (MulEq, Mul),
            (DivEq, Div)
        )
    }

    fn or_expr(&mut self) -> ParseResult<'a> {
        let start = self.token.start;
        let node = self.and_expr()?;

        match self.token.ty {
            Or => {
                self.advance();
                Ok(Node::new(
                    NodeType::Binary(Box::new(node), BinaryOp::Or, Box::new(self.or_expr()?)),
                    start,
                    self.token.end,
                ))
            }
            _ => Ok(node),
        }
    }

    fn and_expr(&mut self) -> ParseResult<'a> {
        let start = self.token.start;
        let node = self.not_expr()?;

        match self.token.ty {
            And => {
                self.advance();
                Ok(Node::new(
                    NodeType::Binary(Box::new(node), BinaryOp::And, Box::new(self.and_expr()?)),
                    start,
                    self.token.end,
                ))
            }
            _ => Ok(node),
        }
    }

    fn not_expr(&mut self) -> ParseResult<'a> {
        let start = self.token.start;
        match self.token.ty {
            Not => {
                self.advance();
                Ok(Node::new(
                    NodeType::Unary(UnaryOp::Not, Box::new(self.not_expr()?)),
                    start,
                    self.token.end,
                ))
            }
            _ => self.comp_expr(),
        }
    }

    fn comp_expr(&mut self) -> ParseResult<'a> {
        let start = self.token.start;
        let node = self.arith_expr()?;

        macro_rules! comp_expr {
            ($($token:tt),*) => {
                match self.token.ty {
                    $(
                        $token => {
                            self.advance();
                            Ok(Node::new(NodeType::Binary(Box::new(node), BinaryOp::$token, Box::new(self.arith_expr()?)), start, self.token.end))
                        },
                    )*
                    _ => Ok(node),
                }
            };
        }

        comp_expr!(EqEq, Neq, Lt, Lte, Gt, Gte)
    }

    fn arith_expr(&mut self) -> ParseResult<'a> {
        let start = self.token.start;
        let node = self.term()?;

        macro_rules! arith_expr {
            ($($token:tt),*) => {
                match self.token.ty {
                    $(
                        $token => {
                            self.advance();
                            Ok(Node::new(NodeType::Binary(Box::new(node), BinaryOp::$token, Box::new(self.arith_expr()?)), start, self.token.end))
                        },
                    )*
                    _ => Ok(node),
                }
            };
        }

        arith_expr!(Add, Sub)
    }

    fn term(&mut self) -> ParseResult<'a> {
        let start = self.token.start;
        let node = self.factor()?;

        macro_rules! term {
            ($($token:tt),*) => {
                match self.token.ty {
                    $(
                        $token => {
                            self.advance();
                            Ok(Node::new(NodeType::Binary(Box::new(node), BinaryOp::$token, Box::new(self.term()?)), start, self.token.end))
                        },
                    )*
                    _ => Ok(node),
                }
            };
        }

        term!(Mul, Div)
    }

    fn factor(&mut self) -> ParseResult<'a> {
        let start = self.token.start;
        match self.token.ty {
            Add => {
                self.advance();
                Ok(Node::new(
                    NodeType::Unary(UnaryOp::Pos, Box::new(self.factor()?)),
                    start,
                    self.token.end,
                ))
            }
            Sub => {
                self.advance();
                Ok(Node::new(
                    NodeType::Unary(UnaryOp::Neg, Box::new(self.factor()?)),
                    start,
                    self.token.end,
                ))
            }
            _ => self.power(),
        }
    }

    fn power(&mut self) -> ParseResult<'a> {
        let start = self.token.start;
        let node = self.call()?;

        match self.token.ty {
            Pow => {
                self.advance();
                Ok(Node::new(
                    NodeType::Binary(Box::new(node), BinaryOp::Pow, Box::new(self.power()?)),
                    start,
                    self.token.end,
                ))
            }
            _ => Ok(node),
        }
    }

    fn call(&mut self) -> ParseResult<'a> {
        let start = self.token.start;
        let node = self.index()?;

        match self.token.ty {
            LParen => {
                self.advance();

                match node.ty {
                    NodeType::Identifier(_) => {
                        let args = self.list(RParen)?;
                        Ok(Node::new(
                            NodeType::Call(Box::new(node), args),
                            start,
                            self.token.end,
                        ))
                    }
                    _ => Err(ParseError::new(
                        "expected identifier".to_string(),
                        start,
                        self.token.end,
                    )),
                }
            }
            _ => Ok(node),
        }
    }

    fn index(&mut self) -> ParseResult<'a> {
        let start = self.token.start;
        let node = self.atom()?;

        match self.token.ty {
            LBracket => {
                self.advance();
                let index = self.expr()?;
                match self.token.ty {
                    RBracket => {
                        self.advance();
                        Ok(Node::new(
                            NodeType::Index(Box::new(node), Box::new(index)),
                            start,
                            self.token.end,
                        ))
                    }
                    _ => Err(ParseError::new(
                        "expected ']'".to_string(),
                        start,
                        self.token.end,
                    )),
                }
            }
            _ => Ok(node),
        }
    }

    fn atom(&mut self) -> ParseResult<'a> {
        let start = self.token.start;
        match self.token.clone().ty {
            Int(value) => {
                self.advance();
                Ok(Node::new(NodeType::Int(value), start, self.token.end))
            }
            Float(value) => {
                self.advance();
                Ok(Node::new(NodeType::Float(value), start, self.token.end))
            }
            Bool(value) => {
                self.advance();
                Ok(Node::new(NodeType::Bool(value), start, self.token.end))
            }
            Str(value) => {
                self.advance();
                Ok(Node::new(NodeType::Str(value), start, self.token.end))
            }
            Char(value) => {
                self.advance();
                Ok(Node::new(NodeType::Char(value), start, self.token.end))
            }
            Identifier(name) => {
                self.advance();
                Ok(Node::new(NodeType::Identifier(name), start, self.token.end))
            }
            LParen => {
                self.advance();
                let node = self.expr()?;

                if self.token.ty != RParen {
                    return Err(ParseError::new(
                        "expected ')'".to_string(),
                        self.token.start,
                        self.token.end,
                    ));
                }
                self.advance();

                Ok(node)
            }
            LBracket => self.array_expr(),
            If => self.if_expr(),
            While => self.while_expr(),
            For => self.for_expr(),
            Fn => self.fn_expr(),
            EOF => Ok(Node::new(
                NodeType::EOF,
                Position::new(""),
                Position::new(""),
            )),
            _ => Err(ParseError::new(
                "expected int, float, bool, str, identifier, '(', 'if', 'while', 'for', or 'fn'"
                    .to_string(),
                start,
                self.token.end,
            )),
        }
    }

    fn array_expr(&mut self) -> ParseResult<'a> {
        let start = self.token.start;
        self.advance();

        let nodes = self.list(RBracket)?;

        Ok(Node::new(NodeType::Array(nodes), start, self.token.end))
    }

    fn if_expr(&mut self) -> ParseResult<'a> {
        let start = self.token.start;
        self.advance();

        let condition = self.expr()?;

        let body = match self.token.ty {
            Colon => {
                self.advance();
                self.statement()
            }
            LBrace => self.block(),
            _ => Err(ParseError::new(
                "expected ':' or '{'".to_string(),
                self.token.start,
                self.token.end,
            )),
        }?;

        self.skip_newlines();
        let else_node = match self.token.ty {
            Else => Some(Box::new(self.else_expr()?)),
            _ => None,
        };

        Ok(Node::new(
            NodeType::If(Box::new(condition), Box::new(body), else_node),
            start,
            self.token.end,
        ))
    }

    fn else_expr(&mut self) -> ParseResult<'a> {
        let start = self.token.start;
        self.advance();

        match self.token.ty {
            Colon => {
                self.advance();
                self.statement()
            }
            LBrace => self.block(),
            If => self.if_expr(),
            _ => Err(ParseError::new(
                "expected ':', '{', or 'if'".to_string(),
                start,
                self.token.end,
            )),
        }
    }

    fn while_expr(&mut self) -> ParseResult<'a> {
        let start = self.token.start;
        self.advance();

        let condition = self.expr()?;

        let body = match self.token.ty {
            Colon => {
                self.advance();
                self.statement()
            }
            LBrace => self.block(),
            _ => Err(ParseError::new(
                "expected ':' or '{'".to_string(),
                start,
                self.token.end,
            )),
        }?;

        Ok(Node::new(
            NodeType::While(Box::new(condition), Box::new(body)),
            start,
            self.token.end,
        ))
    }

    fn for_expr(&mut self) -> ParseResult<'a> {
        let start = self.token.start;
        self.advance();

        let identifier = match self.token.clone().ty {
            Identifier(name) => Ok(Node::new(NodeType::Identifier(name), start, self.token.end)),
            _ => Err(ParseError::new(
                "expected identifier".to_string(),
                start,
                self.token.end,
            )),
        }?;
        self.advance();

        if self.token.ty != In {
            return Err(ParseError::new(
                "expected 'in'".to_string(),
                self.token.start,
                self.token.end,
            ));
        }
        self.advance();

        let iterable = self.expr()?;

        let body = match self.token.ty {
            Colon => {
                self.advance();
                self.statement()
            }
            LBrace => self.block(),
            _ => Err(ParseError::new(
                "expected ':' or '{'".to_string(),
                self.token.start,
                self.token.end,
            )),
        }?;

        Ok(Node::new(
            NodeType::For(Box::new(identifier), Box::new(iterable), Box::new(body)),
            start,
            self.token.end,
        ))
    }

    fn fn_expr(&mut self) -> ParseResult<'a> {
        let start = self.token.start;
        self.advance();

        let name = match self.token.clone().ty {
            Identifier(name) => Ok(Node::new(
                NodeType::Identifier(name.clone()),
                self.token.start,
                self.token.end,
            )),
            _ => {
                return Err(ParseError::new(
                    "expected identifier".to_string(),
                    self.token.start,
                    self.token.end,
                ))
            }
        }?;
        self.advance();

        if self.token.ty != LParen {
            return Err(ParseError::new(
                "expected '('".to_string(),
                self.token.start,
                self.token.end,
            ));
        }
        self.advance();

        let mut args: Vec<Node> = vec![];

        while self.token.ty != RParen {
            let name = match self.token.clone().ty {
                Identifier(name) => Ok(Node::new(
                    NodeType::Identifier(name.clone()),
                    self.token.start,
                    self.token.end,
                )),
                _ => {
                    return Err(ParseError::new(
                        "expected identifier".to_string(),
                        start,
                        self.token.end,
                    ))
                }
            }?;
            self.advance();

            match self.token.clone().ty {
                Comma => self.advance(),
                RParen => {}
                _ => {
                    return Err(ParseError::new(
                        "expected ',' or ')'".to_string(),
                        self.token.start,
                        self.token.end,
                    ))
                }
            };

            args.push(name);
        }

        if self.token.ty != RParen {
            return Err(ParseError::new(
                "expected ')'".to_string(),
                self.token.start,
                self.token.end,
            ));
        }
        self.advance();

        let body = match self.token.ty {
            LBrace => self.block(),
            _ => Err(ParseError::new(
                "expected '{'".to_string(),
                self.token.start,
                self.token.end,
            )),
        }?;

        Ok(Node::new(
            NodeType::Fn(Box::new(name), args, Box::new(body)),
            start,
            self.token.end,
        ))
    }

    fn list(&mut self, end: TokenType) -> Result<Vec<Node<'a>>, ParseError<'a>> {
        let start = self.token.start;
        let mut nodes: Vec<Node> = vec![];

        while self.token.ty != end {
            nodes.push(self.expr()?);
            let start = self.token.start;
            match self.token.clone().ty {
                Comma => self.advance(),
                t if t == end => {}
                _ => {
                    return Err(ParseError::new(
                        format!("expected ',' or '{}'", end),
                        start,
                        self.token.end,
                    ))
                }
            };
        }

        if self.token.ty != end {
            return Err(ParseError::new(
                format!("expected '{}'", end),
                start,
                self.token.end,
            ));
        }
        self.advance();

        Ok(nodes)
    }

    fn block(&mut self) -> ParseResult<'a> {
        self.advance();

        let statements = self.statements()?;

        if self.token.ty != RBrace {
            return Err(ParseError::new(
                "expected '}'".to_string(),
                self.token.start,
                self.token.end,
            ));
        }
        self.advance();

        Ok(statements)
    }
}
