use std::slice::Iter;

use crate::{BinaryOp, Node, NodeType, Position, Token, TokenType, UnaryOp, EOF_NODE, EOF_TOKEN};
use TokenType::*;

pub struct Parser<'a> {
    tokens: Iter<'a, Token<'a>>,
    token: &'a Token<'a>,
}

struct ParserError<'a> {
    message: &'a str,
    start: Position<'a>,
    end: Position<'a>,
}

impl<'a> ParserError<'a> {
    fn new(message: &'a str, start: Position<'a>, end: Position<'a>) -> Self {
        ParserError {
            message,
            start,
            end,
        }
    }
}

impl<'a> Parser<'a> {
    pub fn new(tokens: Vec<Token<'a>>) -> Self {
        let tokens_iter = tokens.iter();
        let token = tokens_iter.next().unwrap_or(&EOF_TOKEN);
        Self {
            tokens: tokens_iter,
            token,
        }
    }

    fn error(&self, message: &'a str) -> ParserError<'a> {
        ParserError {
            message,
            start: self.token.start,
            end: self.token.end,
        }
    }

    fn advance(&'a mut self) {
        let next = self.tokens.next();
        self.token = match next {
            Some(token) => token,
            None => &EOF_TOKEN,
        };
    }

    fn skip_newlines(&'a mut self) -> u32 {
        let mut newlines: u32 = 0;
        while self.token.ty == Newline {
            self.advance();
            newlines += 1;
        }
        newlines
    }

    pub fn parse(&'a mut self) -> Result<Node, ParserError> {
        self.statements()
    }

    fn statements(&'a mut self) -> Result<Node, ParserError> {
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
            if statement == EOF_NODE {
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

    pub fn statement(&'a mut self) -> Result<Node, ParserError> {
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

    fn expr(&'a mut self) -> Result<Node, ParserError> {
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
            (DivEq, Div),
        )
    }

    fn or_expr(&'a mut self) -> Result<Node, ParserError> {
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

    fn and_expr(&'a mut self) -> Result<Node, ParserError> {
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

    fn not_expr(&'a mut self) -> Result<Node, ParserError> {
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

    fn comp_expr(&'a mut self) -> Result<Node, ParserError> {
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

    fn arith_expr(&'a mut self) -> Result<Node, ParserError> {
        let start = self.token.start;
        let node = self.term()?;

        macro_rules! arith_expr {
            ($($token:tt),*) => {
                match self.token.ty {
                    $(
                        $token => {
                            self.advance();
                            Ok(Node::new(NodeType::Binary(Box::new(node), BinaryOp::$token, Box::new(self.arith_expr()?)), self.token.start, self.token.end))
                        },
                    )*
                    _ => Ok(node),
                }
            };
        }

        arith_expr!(Add, Sub)
    }

    fn term(&'a mut self) -> Result<Node, ParserError> {
        let start = self.token.start;
        let node = self.factor()?;

        macro_rules! term {
            ($($token:tt),*) => {
                match self.token.ty {
                    $(
                        $token => {
                            self.advance();
                            Ok(Node::new(NodeType::Binary(Box::new(node), BinaryOp::$token, Box::new(self.term()?)), self.token.start, self.token.end))
                        },
                    )*
                    _ => Ok(node),
                }
            };
        }

        term!(Mul, Div)
    }

    fn factor(&'a mut self) -> Result<Node, ParserError> {
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
            _ => self.call(),
        }
    }

    fn power(&'a mut self) -> Result<Node, ParserError> {
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

    fn call(&'a mut self) -> Result<Node, ParserError> {
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
                    _ => Err(ParserError::new(
                        "expected identifier",
                        start,
                        self.token.end,
                    )),
                }
            }
            _ => Ok(node),
        }
    }

    fn index(&'a mut self) -> Result<Node, ParserError> {
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
                    _ => Err(ParserError::new("expected ']'", start, self.token.end)),
                }
            }
            _ => Ok(node),
        }
    }

    fn atom(&'a mut self) -> Result<Node, ParserError> {
        let start = self.token.start;
        match self.token.ty {
            Int(value) => {
                self.advance();
                Ok(Node::new(NodeType::Int(value),start,self.token.end))
            }
            Float(value) => {
                self.advance();
                Ok(Node::new(NodeType::Float(value),start,self.token.end))
            }
            Bool(value) => {
                self.advance();
                Ok(Node::new(NodeType::Bool(value),start,self.token.end))
            }
            Str(value) => {
                self.advance();
                Ok(Node::new(NodeType::Str(value),start,self.token.end))
            }
            Char(value) => {
                self.advance();
                Ok(Node::new(NodeType::Char(value),start,self.token.end))
            }
            Identifier(name) => {
                self.advance();
                Ok(Node::new(NodeType::Identifier(name),start,self.token.end))
            }
            LParen => {
                self.advance();
                let node = self.expr()?;

                if self.token.ty != RParen {
                    return Err(ParserError::new("expected ')'", self.token.start, self.token.end));
                }
                self.advance();

                Ok(node)
            }
            LBracket => self.array_expr(),
            If => self.if_expr(),
            While => self.while_expr(),
            For => self.for_expr(),
            Fn => self.fn_expr(),
            EOF => Ok(EOF_NODE),
            _ => Err(ParserError::new("expected int, float, bool, str, type, identifier, '(', 'if', 'while', 'for', or 'fn'", start, self.token.end)),
        }
    }

    fn array_expr(&'a mut self) -> Result<Node, ParserError> {
        let start = self.token.start;
        self.advance();

        let nodes = self.list(RBracket)?;

        Ok(Node::new(NodeType::Array(nodes), start, self.token.end))
    }

    fn if_expr(&'a mut self) -> Result<Node, ParserError> {
        let start = self.token.start;
        self.advance();

        let condition = self.expr()?;

        let body = match self.token.ty {
            Colon => {
                self.advance();
                self.statement()
            }
            LBrace => self.block(),
            _ => Err(ParserError::new(
                "expected ':' or '{'",
                self.token.start,
                self.token.end,
            )),
        }?;

        let newlines = self.skip_newlines();
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

    fn else_expr(&'a mut self) -> Result<Node, ParserError> {
        let start = self.token.start;
        self.advance();

        match self.token.ty {
            Colon => {
                self.advance();
                self.statement()
            }
            LBrace => self.block(),
            If => self.if_expr(),
            _ => Err(ParserError::new(
                "expected ':', '{', or 'if'",
                self.token.start,
                self.token.end,
            )),
        }
    }

    fn while_expr(&'a mut self) -> Result<Node, ParserError> {
        let start = self.token.start;
        self.advance();

        let condition = self.expr()?;

        let body = match self.token.ty {
            Colon => {
                self.advance();
                self.statement()
            }
            LBrace => self.block(),
            _ => Err(ParserError::new(
                "expected ':' or '{'",
                self.token.start,
                self.token.end,
            )),
        }?;

        Ok(Node::new(
            NodeType::While(Box::new(condition), Box::new(body)),
            start,
            self.token.end,
        ))
    }

    fn for_expr(&'a mut self) -> Result<Node, ParserError> {
        let start = self.token.start;
        self.advance();

        let identifier = match self.token.ty {
            Identifier(name) => Ok(Node::new(
                NodeType::Identifier(name),
                self.token.start,
                self.token.end,
            )),
            _ => Err(ParserError::new(
                "expected identifier",
                self.token.start,
                self.token.end,
            )),
        }?;
        self.advance();

        if self.token.ty != In {
            return Err(ParserError::new(
                "expected 'in'",
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
            _ => Err(ParserError::new(
                "expected ':' or '{'",
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

    fn fn_expr(&'a mut self) -> Result<Node, ParserError> {
        let start = self.token.start;
        self.advance();

        let name = match self.token.ty {
            Identifier(name) => Ok(Node::new(
                NodeType::Identifier(name.clone()),
                self.token.start,
                self.token.end,
            )),
            _ => {
                return Err(ParserError::new(
                    "expected identifier",
                    self.token.start,
                    self.token.end,
                ))
            }
        }?;
        self.advance();

        if self.token.ty != LParen {
            return Err(ParserError::new(
                "expected '('",
                self.token.start,
                self.token.end,
            ));
        }
        self.advance();

        let mut args: Vec<Node> = vec![];

        while self.token.ty != RParen {
            let name = match self.token.ty {
                Identifier(name) => Ok(Node::new(
                    NodeType::Identifier(name.clone()),
                    self.token.start,
                    self.token.end,
                )),
                _ => {
                    return Err(ParserError::new(
                        "expected identifier",
                        start,
                        self.token.end,
                    ))
                }
            }?;
            self.advance();

            match self.token.ty {
                Comma => self.advance(),
                RParen => {}
                _ => {
                    return Err(ParserError::new(
                        "expected ',' or ')'",
                        self.token.start,
                        self.token.end,
                    ))
                }
            };

            args.push(name);
        }

        if self.token.ty != RParen {
            return Err(ParserError::new(
                "expected ')'",
                self.token.start,
                self.token.end,
            ));
        }
        self.advance();

        let body = match self.token.ty {
            LBrace => self.block(),
            _ => Err(ParserError::new(
                "expected '{'",
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

    fn list(&'a mut self, end: TokenType) -> Result<Vec<Node>, ParserError> {
        let start = self.token.start;
        let mut nodes: Vec<Node> = vec![];

        while self.token.ty != end {
            nodes.push(self.expr()?);
            match self.token.ty {
                Comma => self.advance(),
                t if t == end => {}
                _ => {
                    return Err(ParserError::new(
                        &format!("expected ',' or '{}'", end),
                        self.token.start,
                        self.token.end,
                    ))
                }
            };
        }

        if self.token.ty != end {
            panic!("expected '{}'", end);
        }
        self.advance();

        Ok(nodes)
    }

    fn block(&'a mut self) -> Result<Node, ParserError> {
        self.advance();

        let statements = self.statements()?;

        if self.token.ty != RBrace {
            return Err(ParserError::new(
                "expected '}'",
                self.token.start,
                self.token.end,
            ));
        }
        self.advance();

        Ok(statements)
    }
}