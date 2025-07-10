use std::collections::HashMap;
use std::io::{self, Write};

#[derive(Debug, Clone, PartialEq)]
enum Expr {
    Number(i64),
    StringLiteral(String),
    BoolLiteral(bool), // Added for boolean literals
    Var(String),
    BinOp(Box<Expr>, Op, Box<Expr>),
    Call(String, Vec<Expr>),
}

#[derive(Debug, Clone, Copy, PartialEq)]
enum Op {
    Add,
    Sub,
    Mul,
    Div,
    Equal, // ==
    NotEqual, // !=
    LessThan, // <
    GreaterThan, // >
    LessThanEqual, // <=
    GreaterThanEqual, // >=
}

#[derive(Debug, Clone)]
enum Stmt {
    VarDecl(String, Expr),
    Assign(String, Expr), // Added for variable assignment
    Expr(Expr),
    Return(Expr),
    Function(String, Vec<String>, Vec<Stmt>),
    Print(Expr),
    If(Expr, Vec<Stmt>, Option<Vec<Stmt>>), // Added for if/else control flow
}

#[derive(Debug, Clone, PartialEq)] // Added PartialEq for comparisons
enum RuntimeValue {
    Int(i64),
    String(String),
    Bool(bool),
    None,
}

#[derive(Debug)]
enum RuntimeError {
    Msg(String),
}

#[derive(Clone)]
struct Function {
    params: Vec<String>,
    body: Vec<Stmt>,
}

struct Interpreter {
    vars: Vec<HashMap<String, RuntimeValue>>, // stack of scopes
    funcs: HashMap<String, Function>,
    return_value: Option<RuntimeValue>,
}

impl Interpreter {
    fn new() -> Self {
        Self {
            vars: vec![HashMap::new()],
            funcs: HashMap::new(),
            return_value: None,
        }
    }

    fn push_scope(&mut self) {
        self.vars.push(HashMap::new());
    }

    fn pop_scope(&mut self) {
        self.vars.pop();
    }

    fn declare_var(&mut self, name: &str, val: RuntimeValue) {
        if let Some(scope) = self.vars.last_mut() {
            scope.insert(name.to_string(), val);
        }
    }

    fn assign_var(&mut self, name: &str, val: RuntimeValue) -> Result<(), RuntimeError> {
        for scope in self.vars.iter_mut().rev() {
            if scope.contains_key(name) {
                scope.insert(name.to_string(), val);
                return Ok(());
            }
        }
        Err(RuntimeError::Msg(format!("Undefined variable '{}'", name)))
    }

    fn get_var(&self, name: &str) -> Result<RuntimeValue, RuntimeError> {
        for scope in self.vars.iter().rev() {
            if let Some(val) = scope.get(name) {
                return Ok(val.clone());
            }
        }
        Err(RuntimeError::Msg(format!("Undefined variable '{}'", name)))
    }

    fn eval_expr(&mut self, expr: &Expr) -> Result<RuntimeValue, RuntimeError> {
        match expr {
            Expr::Number(n) => Ok(RuntimeValue::Int(*n)),
            Expr::StringLiteral(s) => Ok(RuntimeValue::String(s.clone())),
            Expr::BoolLiteral(b) => Ok(RuntimeValue::Bool(*b)), // Handle boolean literals
            Expr::Var(name) => self.get_var(name),
            Expr::BinOp(lhs, op, rhs) => {
                let l = self.eval_expr(lhs)?;
                let r = self.eval_expr(rhs)?;
                match op {
                    Op::Add | Op::Sub | Op::Mul | Op::Div => {
                        match (l.clone(), r.clone()) { // Clone here
                            (RuntimeValue::Int(a), RuntimeValue::Int(b)) => match op {
                                Op::Add => Ok(RuntimeValue::Int(a + b)),
                                Op::Sub => Ok(RuntimeValue::Int(a - b)),
                                Op::Mul => Ok(RuntimeValue::Int(a * b)),
                                Op::Div => {
                                    if b == 0 {
                                        Err(RuntimeError::Msg("Division by zero".to_string()))
                                    } else {
                                        Ok(RuntimeValue::Int(a / b))
                                    }
                                }
                                _ => unreachable!(),
                            },
                            _ => Err(RuntimeError::Msg(format!("Invalid operands for {:?}: {:?}, {:?}", op, l, r))),
                        }
                    }
                    Op::Equal => Ok(RuntimeValue::Bool(l == r)),
                    Op::NotEqual => Ok(RuntimeValue::Bool(l != r)),
                    Op::LessThan => match (l.clone(), r.clone()) { // Clone here
                        (RuntimeValue::Int(a), RuntimeValue::Int(b)) => Ok(RuntimeValue::Bool(a < b)),
                        _ => Err(RuntimeError::Msg(format!("Invalid operands for < : {:?}, {:?}", l, r))),
                    },
                    Op::GreaterThan => match (l.clone(), r.clone()) { // Clone here
                        (RuntimeValue::Int(a), RuntimeValue::Int(b)) => Ok(RuntimeValue::Bool(a > b)),
                        _ => Err(RuntimeError::Msg(format!("Invalid operands for > : {:?}, {:?}", l, r))),
                    },
                    Op::LessThanEqual => match (l.clone(), r.clone()) { // Clone here
                        (RuntimeValue::Int(a), RuntimeValue::Int(b)) => Ok(RuntimeValue::Bool(a <= b)),
                        _ => Err(RuntimeError::Msg(format!("Invalid operands for <= : {:?}, {:?}", l, r))),
                    },
                    Op::GreaterThanEqual => match (l.clone(), r.clone()) { // Clone here
                        (RuntimeValue::Int(a), RuntimeValue::Int(b)) => Ok(RuntimeValue::Bool(a >= b)),
                        _ => Err(RuntimeError::Msg(format!("Invalid operands for >= : {:?}, {:?}", l, r))),
                    },
                }
            }
            Expr::Call(name, args) => {
                if name == "gimme" {
                    if !args.is_empty() {
                        return Err(RuntimeError::Msg("gimme() takes no arguments".to_string()));
                    }
                    let mut input = String::new();
                    io::stdin().read_line(&mut input)
                        .map_err(|e| RuntimeError::Msg(format!("Failed to read input: {}", e)))?;
                    Ok(RuntimeValue::String(input.trim_end().to_string()))
                } else {
                    let func_data = self
                        .funcs
                        .get(name)
                        .ok_or_else(|| RuntimeError::Msg(format!("Undefined function '{}'", name)))?
                        .clone();

                    if args.len() != func_data.params.len() {
                        return Err(RuntimeError::Msg(format!(
                            "Function '{}' expects {} arguments, got {}",
                            name,
                            func_data.params.len(),
                            args.len()
                        )));
                    }

                    let func_params = func_data.params.clone();
                    let func_body = func_data.body.clone();

                    let mut evaluated_args = Vec::new();
                    for arg in args {
                        evaluated_args.push(self.eval_expr(arg)?);
                    }

                    self.push_scope();

                    for (param, val) in func_params.iter().zip(evaluated_args.into_iter()) {
                        self.declare_var(param, val);
                    }

                    self.return_value = None;
                    for stmt in &func_body {
                        self.exec_stmt(stmt)?;
                        if self.return_value.is_some() {
                            break;
                        }
                    }

                    let ret = self.return_value.take().unwrap_or(RuntimeValue::None);

                    self.pop_scope();

                    Ok(ret)
                }
            }
        }
    }

    fn exec_stmt(&mut self, stmt: &Stmt) -> Result<(), RuntimeError> {
        match stmt {
            Stmt::VarDecl(name, expr) => {
                let val = self.eval_expr(expr)?;
                self.declare_var(name, val);
                Ok(())
            }
            Stmt::Assign(name, expr) => {
                let val = self.eval_expr(expr)?;
                self.assign_var(name, val)?;
                Ok(())
            }
            Stmt::Expr(expr) => {
                self.eval_expr(expr)?;
                Ok(())
            }
            Stmt::Return(expr) => {
                let val = self.eval_expr(expr)?;
                self.return_value = Some(val);
                Ok(())
            }
            Stmt::Function(name, params, body) => {
                if self.funcs.contains_key(name) {
                    return Err(RuntimeError::Msg(format!(
                        "Function '{}' already defined",
                        name
                    )));
                }
                self.funcs.insert(
                    name.clone(),
                    Function {
                        params: params.clone(),
                        body: body.clone(),
                    },
                );
                Ok(())
            }
            Stmt::Print(expr) => {
                let val = self.eval_expr(expr)?;
                match val {
                    RuntimeValue::Int(i) => println!("{}", i),
                    RuntimeValue::String(s) => println!("{}", s),
                    RuntimeValue::Bool(b) => println!("{}", b), // Print boolean values
                    RuntimeValue::None => println!("None"),
                }
                Ok(())
            }
            Stmt::If(condition, then_body, else_body) => { // Handle If statement
                let cond_val = self.eval_expr(condition)?;
                match cond_val {
                    RuntimeValue::Bool(b) => {
                        if b {
                            self.push_scope();
                            for stmt in then_body {
                                self.exec_stmt(stmt)?;
                            }
                            self.pop_scope();
                        } else if let Some(else_stmts) = else_body {
                            self.push_scope();
                            for stmt in else_stmts {
                                self.exec_stmt(stmt)?;
                            }
                            self.pop_scope();
                        }
                        Ok(())
                    }
                    _ => Err(RuntimeError::Msg("If condition must be a boolean".to_string())),
                }
            }
        }
    }
}

// Very simple tokenizer & parser for the language

#[derive(Debug, Clone, PartialEq)]
enum Token {
    Var,
    Func,
    Ret,
    Ident(String),
    Number(i64),
    StringLiteral(String),
    True, // Added for boolean literal
    False, // Added for boolean literal
    Plus,
    Minus,
    Star,
    Slash,
    LParen,
    RParen,
    LBrace,
    RBrace,
    Comma,
    Semicolon,
    Equal, // = (assignment)
    EqEq, // ==
    NotEq, // !=
    Lt, // <
    Gt, // >
    LtEq, // <=
    GtEq, // >=
    If, // Added for control flow
    Else, // Added for control flow
}

struct Lexer<'a> {
    input: &'a str,
    pos: usize,
    chars: Vec<char>,
}

impl<'a> Lexer<'a> {
    fn new(input: &'a str) -> Self {
        Self {
            input,
            pos: 0,
            chars: input.chars().collect(),
        }
    }

    fn next_token(&mut self) -> Option<Token> {
        self.skip_whitespace();

        if self.pos >= self.chars.len() {
            return None;
        }

        let c = self.chars[self.pos];

        match c {
            '+' => {
                self.pos += 1;
                Some(Token::Plus)
            }
            '-' => {
                self.pos += 1;
                Some(Token::Minus)
            }
            '*' => {
                self.pos += 1;
                Some(Token::Star)
            }
            '/' => {
                self.pos += 1;
                Some(Token::Slash)
            }
            '(' => {
                self.pos += 1;
                Some(Token::LParen)
            }
            ')' => {
                self.pos += 1;
                Some(Token::RParen)
            }
            '{' => {
                self.pos += 1;
                Some(Token::LBrace)
            }
            '}' => {
                self.pos += 1;
                Some(Token::RBrace)
            }
            ',' => {
                self.pos += 1;
                Some(Token::Comma)
            }
            ';' => {
                self.pos += 1;
                Some(Token::Semicolon)
            }
            '=' => {
                self.pos += 1;
                if self.pos < self.chars.len() && self.chars[self.pos] == '=' {
                    self.pos += 1;
                    Some(Token::EqEq)
                } else {
                    Some(Token::Equal)
                }
            }
            '!' => {
                self.pos += 1;
                if self.pos < self.chars.len() && self.chars[self.pos] == '=' {
                    self.pos += 1;
                    Some(Token::NotEq)
                } else {
                    // Handle single '!' if it becomes a future operator
                    None // For now, single '!' is not a token
                }
            }
            '<' => {
                self.pos += 1;
                if self.pos < self.chars.len() && self.chars[self.pos] == '=' {
                    self.pos += 1;
                    Some(Token::LtEq)
                } else {
                    Some(Token::Lt)
                }
            }
            '>' => {
                self.pos += 1;
                if self.pos < self.chars.len() && self.chars[self.pos] == '=' {
                    self.pos += 1;
                    Some(Token::GtEq)
                } else {
                    Some(Token::Gt)
                }
            }
            '"' => Some(self.lex_string()),
            c if c.is_ascii_digit() => Some(self.lex_number()),
            c if is_ident_start(c) => Some(self.lex_ident()),
            _ => None,
        }
    }

    fn skip_whitespace(&mut self) {
        loop {
            let mut changed = false;
            while self.pos < self.chars.len() && self.chars[self.pos].is_whitespace() {
                self.pos += 1;
                changed = true;
            }
            // Handle single-line comments
            if self.pos + 1 < self.chars.len() && self.chars[self.pos] == '/' && self.chars[self.pos + 1] == '/' {
                self.pos += 2; // Consume "//"
                while self.pos < self.chars.len() && self.chars[self.pos] != '\n' && self.chars[self.pos] != '\r' {
                    self.pos += 1;
                }
                changed = true;
            }
            if !changed {
                break;
            }
        }
    }

    fn lex_number(&mut self) -> Token {
        let start = self.pos;
        while self.pos < self.chars.len() && self.chars[self.pos].is_ascii_digit() {
            self.pos += 1;
        }
        let s: String = self.chars[start..self.pos].iter().collect();
        let n = s.parse::<i64>().unwrap();
        Token::Number(n)
    }

    fn lex_string(&mut self) -> Token {
        self.pos += 1; // Consume the opening quote
        let start = self.pos;
        while self.pos < self.chars.len() && self.chars[self.pos] != '"' {
            self.pos += 1;
        }
        let s: String = self.chars[start..self.pos].iter().collect();
        self.pos += 1; // Consume the closing quote
        Token::StringLiteral(s)
    }

    fn lex_ident(&mut self) -> Token {
        let start = self.pos;
        while self.pos < self.chars.len() && is_ident_continue(self.chars[self.pos]) {
            self.pos += 1;
        }
        let s: String = self.chars[start..self.pos].iter().collect();

        match s.as_str() {
            "var" => Token::Var,
            "func" => Token::Func,
            "ret" => Token::Ret,
            "true" => Token::True, // Added for boolean literal
            "false" => Token::False, // Added for boolean literal
            "if" => Token::If, // Added for control flow
            "else" => Token::Else, // Added for control flow
            _ => Token::Ident(s),
        }
    }
}

fn is_ident_start(c: char) -> bool {
    c.is_ascii_alphabetic() || c == '_'
}

fn is_ident_continue(c: char) -> bool {
    c.is_ascii_alphanumeric() || c == '_'
}

struct Parser<'a> {
    lexer: Lexer<'a>,
    cur_token: Option<Token>,
    peek_token: Option<Token>,
}

impl<'a> Parser<'a> {
    fn new(input: &'a str) -> Self {
        let mut lexer = Lexer::new(input);
        let cur_token = lexer.next_token();
        let peek_token = lexer.next_token();
        Self {
            lexer,
            cur_token,
            peek_token,
        }
    }

    fn next_token(&mut self) {
        self.cur_token = self.peek_token.clone();
        self.peek_token = self.lexer.next_token();
    }

    fn parse_program(&mut self) -> Result<Vec<Stmt>, String> {
        let mut stmts = Vec::new();

        while self.cur_token.is_some() {
            stmts.push(self.parse_stmt()?);
        }

        Ok(stmts)
    }


    fn parse_stmt(&mut self) -> Result<Stmt, String> {
        match &self.cur_token {
            Some(Token::Var) => self.parse_var_decl_stmt(),
            Some(Token::Func) => self.parse_function_stmt(),
            Some(Token::Ret) => self.parse_return_stmt(),
            Some(Token::Ident(s)) if s == "shit" => self.parse_print_stmt(),
            Some(Token::If) => self.parse_if_stmt(), // Handle 'if' statement
            Some(Token::Ident(_)) if self.peek_token == Some(Token::Equal) => self.parse_assignment_stmt(),
            _ => self.parse_expr_stmt(),
        }
    }

    fn parse_var_decl_stmt(&mut self) -> Result<Stmt, String> {
        // var x = expr;
        self.next_token(); // consume 'var'
        let name = match &self.cur_token {
            Some(Token::Ident(s)) => s.clone(),
            _ => return Err("Expected identifier after var".into()),
        };
        self.next_token();
        if self.cur_token != Some(Token::Equal) {
            return Err("Expected '=' after identifier in var declaration".into());
        }
        self.next_token(); // consume '='
        let expr = self.parse_expr()?;
        if self.cur_token != Some(Token::Semicolon) {
            return Err("Expected ';' after expression".into());
        }
        self.next_token(); // consume ';'
        Ok(Stmt::VarDecl(name, expr)) // Changed from Stmt::Let
    }

    fn parse_function_stmt(&mut self) -> Result<Stmt, String> {
        // func name(params) { body }
        self.next_token(); // consume 'func'
        let name = match &self.cur_token {
            Some(Token::Ident(s)) => s.clone(),
            _ => return Err("Expected function name after func".into()),
        };
        self.next_token();

        if self.cur_token != Some(Token::LParen) {
            return Err("Expected '(' after function name".into());
        }
        self.next_token();

        let mut params = Vec::new();
        if self.cur_token != Some(Token::RParen) {
            loop {
                match &self.cur_token {
                    Some(Token::Ident(s)) => params.push(s.clone()),
                    _ => return Err("Expected identifier in function parameter list".into()),
                }
                self.next_token();
                if self.cur_token == Some(Token::Comma) {
                    self.next_token();
                } else {
                    break;
                }
            }
        }

        if self.cur_token != Some(Token::RParen) {
            return Err("Expected ')' after function parameters".into());
        }
        self.next_token();

        if self.cur_token != Some(Token::LBrace) {
            return Err("Expected '{' to start function body".into());
        }
        self.next_token();

        let mut body = Vec::new();
        while self.cur_token != Some(Token::RBrace) && self.cur_token.is_some() {
            body.push(self.parse_stmt()?);
        }

        if self.cur_token != Some(Token::RBrace) {
            return Err("Expected '}' at end of function body".into());
        }
        self.next_token(); // consume '}'

        Ok(Stmt::Function(name, params, body))
    }

    fn parse_return_stmt(&mut self) -> Result<Stmt, String> {
        self.next_token(); // consume 'ret'
        let expr = self.parse_expr()?;
        if self.cur_token != Some(Token::Semicolon) {
            return Err("Expected ';' after return expression".into());
        }
        self.next_token();
        Ok(Stmt::Return(expr))
    }

    fn parse_print_stmt(&mut self) -> Result<Stmt, String> {
        self.next_token(); // consume 'shit'
        if self.cur_token != Some(Token::LParen) {
            return Err("Expected '(' after 'shit'".into());
        }
        self.next_token(); // consume '('
        let expr = self.parse_expr()?;
        if self.cur_token != Some(Token::RParen) {
            return Err("Expected ')' after print expression".into());
        }
        self.next_token(); // consume ')'
        if self.cur_token != Some(Token::Semicolon) {
            return Err("Expected ';' after print statement".into());
        }
        self.next_token(); // consume ';'
        Ok(Stmt::Print(expr))
    }

    fn parse_expr_stmt(&mut self) -> Result<Stmt, String> {
        let expr = self.parse_expr()?;
        if self.cur_token != Some(Token::Semicolon) {
            return Err("Expected ';' after expression".into());
        }
        self.next_token();
        Ok(Stmt::Expr(expr))
    }

    fn parse_expr(&mut self) -> Result<Expr, String> {
        self.parse_binary_expr(0)
    }

    fn parse_binary_expr(&mut self, min_prec: u8) -> Result<Expr, String> {
        let mut lhs = self.parse_primary()?;

        while let Some(op) = self.cur_token.as_ref().and_then(token_to_op) {
            let prec = op_prec(op);
            if prec < min_prec {
                break;
            }
            self.next_token();

            let rhs = self.parse_binary_expr(prec + 1)?;
            lhs = Expr::BinOp(Box::new(lhs), op, Box::new(rhs));
        }

        Ok(lhs)
    }

    fn parse_primary(&mut self) -> Result<Expr, String> {
        match &self.cur_token {
            Some(Token::Number(n)) => {
                let val = *n;
                self.next_token();
                Ok(Expr::Number(val))
            }
            Some(Token::StringLiteral(s)) => {
                let val = s.clone();
                self.next_token();
                Ok(Expr::StringLiteral(val))
            }
            Some(Token::True) => { // Handle boolean literal
                self.next_token();
                Ok(Expr::BoolLiteral(true))
            }
            Some(Token::False) => { // Handle boolean literal
                self.next_token();
                Ok(Expr::BoolLiteral(false))
            }
            Some(Token::Ident(name)) => {
                let name = name.clone();
                self.next_token();
                if self.cur_token == Some(Token::LParen) {
                    // function call
                    self.next_token();
                    let mut args = Vec::new();
                    if self.cur_token != Some(Token::RParen) {
                        loop {
                            let expr = self.parse_expr()?;
                            args.push(expr);
                            if self.cur_token == Some(Token::Comma) {
                                self.next_token();
                            } else {
                                break;
                            }
                        }
                    }
                    if self.cur_token != Some(Token::RParen) {
                        return Err("Expected ')' after function arguments".into());
                    }
                    self.next_token();
                    Ok(Expr::Call(name, args))
                } else {
                    // variable
                    Ok(Expr::Var(name))
                }
            }
            Some(Token::LParen) => {
                self.next_token();
                let expr = self.parse_expr()?;
                if self.cur_token != Some(Token::RParen) {
                    return Err("Expected ')'".into());
                }
                self.next_token();
                Ok(expr)
            }
            _ => Err("Unexpected token in expression".into()),
        }
    }

    fn parse_if_stmt(&mut self) -> Result<Stmt, String> {
        self.next_token(); // consume 'if'
        if self.cur_token != Some(Token::LParen) {
            return Err("Expected '(' after 'if'".into());
        }
        self.next_token(); // consume '('
        let condition = self.parse_expr()?;
        if self.cur_token != Some(Token::RParen) {
            return Err("Expected ')' after if condition".into());
        }
        self.next_token(); // consume ')'

        if self.cur_token != Some(Token::LBrace) {
            return Err("Expected '{' to start if body".into());
        }
        self.next_token(); // consume '{'

        let mut then_body = Vec::new();
        while self.cur_token != Some(Token::RBrace) && self.cur_token.is_some() {
            then_body.push(self.parse_stmt()?);
        }
        if self.cur_token != Some(Token::RBrace) {
            return Err("Expected '}' at end of if body".into());
        }
        self.next_token(); // consume '}'

        let mut else_body = None;
        if self.cur_token == Some(Token::Else) {
            self.next_token(); // consume 'else'
            if self.cur_token != Some(Token::LBrace) {
                return Err("Expected '{' to start else body".into());
            }
            self.next_token(); // consume '{'
            let mut else_stmts = Vec::new();
            while self.cur_token != Some(Token::RBrace) && self.cur_token.is_some() {
                else_stmts.push(self.parse_stmt()?);
            }
            if self.cur_token != Some(Token::RBrace) {
                return Err("Expected '}' at end of else body".into());
            }
            self.next_token(); // consume '}'
            else_body = Some(else_stmts);
        }

        Ok(Stmt::If(condition, then_body, else_body))
    }

    fn parse_assignment_stmt(&mut self) -> Result<Stmt, String> {
        // cur_token is Ident, peek_token is Equal
        let name = match self.cur_token.take() {
            Some(Token::Ident(s)) => s,
            _ => return Err("Expected identifier for assignment".into()),
        };
        self.next_token(); // cur_token is now Equal
        if self.cur_token != Some(Token::Equal) {
            return Err("Expected '=' after identifier in assignment".into());
        }
        self.next_token(); // cur_token is now the start of the expression
        let expr = self.parse_expr()?;
        if self.cur_token != Some(Token::Semicolon) {
            return Err("Expected ';' after assignment expression".into());
        }
        self.next_token(); // consume ';'
        Ok(Stmt::Assign(name, expr))
    }
}

fn token_to_op(token: &Token) -> Option<Op> {
    match token {
        Token::Plus => Some(Op::Add),
        Token::Minus => Some(Op::Sub),
        Token::Star => Some(Op::Mul),
        Token::Slash => Some(Op::Div),
        Token::EqEq => Some(Op::Equal),
        Token::NotEq => Some(Op::NotEqual),
        Token::Lt => Some(Op::LessThan),
        Token::Gt => Some(Op::GreaterThan),
        Token::LtEq => Some(Op::LessThanEqual),
        Token::GtEq => Some(Op::GreaterThanEqual),
        _ => None,
    }
}

fn op_prec(op: Op) -> u8 {
    match op {
        Op::Add | Op::Sub => 1,
        Op::Mul | Op::Div => 2,
        Op::Equal | Op::NotEqual | Op::LessThan | Op::GreaterThan | Op::LessThanEqual | Op::GreaterThanEqual => 0, // Lowest precedence for comparisons
    }
}

use std::fs;
use std::env;

fn main() {
    let args: Vec<String> = env::args().collect();

    let mut interpreter = Interpreter::new();

    if args.len() > 1 {
        // File mode
        let file_path = &args[1];
        let code = match fs::read_to_string(file_path) {
            Ok(c) => c,
            Err(e) => {
                eprintln!("Error reading file {}: {}", file_path, e);
                return;
            }
        };

        let mut parser = Parser::new(&code);
        match parser.parse_program() {
            Ok(stmts) => {
                for stmt in stmts {
                    if let Err(e) = interpreter.exec_stmt(&stmt) {
                        eprintln!("Runtime Error: {:?}", e);
                        break;
                    } else if let Stmt::Expr(expr) = stmt {
                        match interpreter.eval_expr(&expr) {
                            Ok(RuntimeValue::Int(i)) => println!("{}", i),
                            Ok(_) => (),
                            Err(e) => eprintln!("Runtime Error: {:?}", e),
                        }
                    }
                }
            }
            Err(e) => eprintln!("Parse Error: {}", e),
        }
    } else {
        // REPL mode
        println!("Welcome to Mini Esolang REPL!");
        println!("Supports variables, functions, operators, and error handling.");
        println!("Type 'exit;' to quit.");

        loop {
            print!(">>> ");
            io::stdout().flush().unwrap();

            let mut input = String::new();
            if io::stdin().read_line(&mut input).unwrap() == 0 {
                break; // EOF
            }

            if input.trim() == "exit;" {
                println!("Bye!");
                break;
            }

            let mut parser = Parser::new(&input);
            match parser.parse_program() {
                Ok(stmts) => {
                    let mut error_occurred = false;
                    for stmt in stmts {
                        if let Err(e) = interpreter.exec_stmt(&stmt) {
                            eprintln!("Runtime Error: {:?}", e);
                            error_occurred = true;
                            break;
                        } else if let Stmt::Expr(expr) = stmt {
                            // print expression result
                            match interpreter.eval_expr(&expr) {
                                Ok(RuntimeValue::Int(i)) => println!("{}", i),
                                Ok(_) => (),
                                Err(e) => eprintln!("Runtime Error: {:?}", e),
                            }
                        }
                    }
                    if error_occurred {
                        continue;
                    }
                }
                Err(e) => eprintln!("Parse Error: {}", e),
            }
        }
    }
}
