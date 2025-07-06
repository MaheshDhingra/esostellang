use std::collections::HashMap;
use std::io::{self, Write};

#[derive(Debug, Clone, PartialEq)]
enum Expr {
    Number(i64),
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
}

#[derive(Debug, Clone)]
enum Stmt {
    Let(String, Expr),
    Expr(Expr),
    Return(Expr),
    Function(String, Vec<String>, Vec<Stmt>),
}

#[derive(Debug, Clone)]
enum RuntimeValue {
    Int(i64),
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
            Expr::Var(name) => self.get_var(name),
            Expr::BinOp(lhs, op, rhs) => {
                let l = self.eval_expr(lhs)?;
                let r = self.eval_expr(rhs)?;
                match (l, r) {
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
                    },
                    _ => Err(RuntimeError::Msg("Invalid operands".to_string())),
                }
            }
            Expr::Call(name, args) => {
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

                // Extract params and body before mutable borrows
                let func_params = func_data.params.clone();
                let func_body = func_data.body.clone();

                // Evaluate args
                let mut evaluated_args = Vec::new();
                for arg in args {
                    evaluated_args.push(self.eval_expr(arg)?);
                }

                // Push new scope
                self.push_scope();

                // Assign params to args
                for (param, val) in func_params.iter().zip(evaluated_args.into_iter()) {
                    self.declare_var(param, val);
                }

                // Execute function body
                self.return_value = None;
                for stmt in &func_body {
                    self.exec_stmt(stmt)?;
                    if self.return_value.is_some() {
                        break;
                    }
                }

                let ret = self.return_value.take().unwrap_or(RuntimeValue::None);

                // Pop scope
                self.pop_scope();

                Ok(ret)
            }
        }
    }

    fn exec_stmt(&mut self, stmt: &Stmt) -> Result<(), RuntimeError> {
        match stmt {
            Stmt::Let(name, expr) => {
                let val = self.eval_expr(expr)?;
                self.declare_var(name, val);
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
        }
    }
}

// Very simple tokenizer & parser for the language

#[derive(Debug, Clone, PartialEq)]
enum Token {
    Let,
    Fn,
    Return,
    Ident(String),
    Number(i64),
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
    Equal,
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
                Some(Token::Equal)
            }
            c if c.is_ascii_digit() => Some(self.lex_number()),
            c if is_ident_start(c) => Some(self.lex_ident()),
            _ => None,
        }
    }

    fn skip_whitespace(&mut self) {
        while self.pos < self.chars.len() && self.chars[self.pos].is_whitespace() {
            self.pos += 1;
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

    fn lex_ident(&mut self) -> Token {
        let start = self.pos;
        while self.pos < self.chars.len() && is_ident_continue(self.chars[self.pos]) {
            self.pos += 1;
        }
        let s: String = self.chars[start..self.pos].iter().collect();

        match s.as_str() {
            "let" => Token::Let,
            "fn" => Token::Fn,
            "return" => Token::Return,
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

    fn expect_cur(&self, expected: &Token) -> bool {
        if let Some(t) = &self.cur_token {
            std::mem::discriminant(t) == std::mem::discriminant(expected)
        } else {
            false
        }
    }

    fn parse_stmt(&mut self) -> Result<Stmt, String> {
        match &self.cur_token {
            Some(Token::Let) => self.parse_let_stmt(),
            Some(Token::Fn) => self.parse_function_stmt(),
            Some(Token::Return) => self.parse_return_stmt(),
            _ => self.parse_expr_stmt(),
        }
    }

    fn parse_let_stmt(&mut self) -> Result<Stmt, String> {
        // let x = expr;
        self.next_token(); // consume 'let'
        let name = match &self.cur_token {
            Some(Token::Ident(s)) => s.clone(),
            _ => return Err("Expected identifier after let".into()),
        };
        self.next_token();
        if self.cur_token != Some(Token::Equal) {
            return Err("Expected '=' after identifier in let statement".into());
        }
        self.next_token(); // consume '='
        let expr = self.parse_expr()?;
        if self.cur_token != Some(Token::Semicolon) {
            return Err("Expected ';' after expression".into());
        }
        self.next_token(); // consume ';'
        Ok(Stmt::Let(name, expr))
    }

    fn parse_function_stmt(&mut self) -> Result<Stmt, String> {
        // fn name(params) { body }
        self.next_token(); // consume 'fn'
        let name = match &self.cur_token {
            Some(Token::Ident(s)) => s.clone(),
            _ => return Err("Expected function name after fn".into()),
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
        self.next_token(); // consume 'return'
        let expr = self.parse_expr()?;
        if self.cur_token != Some(Token::Semicolon) {
            return Err("Expected ';' after return expression".into());
        }
        self.next_token();
        Ok(Stmt::Return(expr))
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
}

fn token_to_op(token: &Token) -> Option<Op> {
    match token {
        Token::Plus => Some(Op::Add),
        Token::Minus => Some(Op::Sub),
        Token::Star => Some(Op::Mul),
        Token::Slash => Some(Op::Div),
        _ => None,
    }
}

fn op_prec(op: Op) -> u8 {
    match op {
        Op::Add | Op::Sub => 1,
        Op::Mul | Op::Div => 2,
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
