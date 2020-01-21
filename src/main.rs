use std::io;
use std::fmt;
use std::io::Write;


/// Calculator implementation using a tiny lexer and parser.
fn main() {
    println!("ProCalc");

    loop {
        print!(">  ");

        if let Err(x) = io::stdout().flush() {
            println!("Could not flush: {}", x);
        }

        // Read a line from the command line (e.g. 4+5) and parse the line to produce a result
        let mut line = String::new();

        io::stdin().read_line(&mut line)
            .ok()
            .expect("Unable to parse line!");

        if line == String::from("exit"){
            break;
        }

        let lexed = lex(&line);
        match parse(&lexed) {
            Ok(parsed) => {
                println!("{}", eval(parsed));
            },
            Err(msg) => {
                println!("{}", msg);
            },
        }
    }
}


// Enums

/// Possible expressions.
/// Used to build an expression tree for parsing.
/// # Example
///
/// 8 + 4 === Addition(Operand(8), Operand(4))
///
#[derive(Debug)]
enum Expr {
    Operand(i64),
    Addition(Box<Expr>, Box<Expr>),
    Subtraction(Box<Expr>, Box<Expr>),
    Multiplication(Box<Expr>, Box<Expr>),
    Division(Box<Expr>, Box<Expr>),
    Negate(Box<Expr>),
}


/// Calculator Tokens
/// Used to tokenize input as part of lexer.
#[derive(Debug)]
enum Token {
    Operand(i64),
    Add,
    Sub,
    Multi,
    Div,
    Lbracket,
    Rbracket,
    EOF
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        match self {
            &Expr::Operand(i) => write!(f, "{}", i),
            &Expr::Addition(ref i, ref j) => write!(f, "{} + {}", i, j),
            &Expr::Subtraction(ref i, ref j) => write!(f, "{} - {}", i, j),
            &Expr::Multiplication(ref i, ref j) => write!(f, "{} *   {}", i, j),
            &Expr::Division(ref i, ref j) => write!(f, "{} / {}", i, j),
            &Expr::Negate(ref i) => write!(f, "-{}" , i),
        }
    }
}

/// Recursively evaluate an expression
fn eval(expr: Expr) -> i64 {
    match expr {
        Expr::Operand(i) => i,
        Expr::Addition(i, j) => eval(*i) + eval(*j),
        Expr::Subtraction(i, j) => eval(*i) - eval(*j),
        Expr::Multiplication(i, j) => eval(*i) * eval(*j),
        Expr::Division(i, j) => eval(*i) / eval(*j),
        Expr::Negate(i) => eval(*i) * -1,
    }
}

fn is_digit(c: &char) -> bool {
    match *c {
        '0'..='9' => true,
        _ => false
    }
}

/// Executes the lexing operation
/// Slices the input into Tokens in preparations for Parsing
fn lex(line: &str) -> Vec<Token> {
    let mut tokens: Vec<Token> = vec![];
    let mut iter = line.chars().peekable();

    while let Some(c) = iter.next() {
        match c {
            '+' => tokens.push(Token::Add),
            '-' => tokens.push(Token::Sub),
            '*' => tokens.push(Token::Multi),
            '/' => tokens.push(Token::Div),
            '(' => tokens.push(Token::Lbracket),
            ')' => tokens.push(Token::Rbracket),
            '0'..='9' => {
                // Check if this operand consists of more than one digit (e.g. 54 -> '5' & '4')
                // If so construct entire operand using Vector
                let mut consuming = true;
                let mut operand: Vec<char> = vec![];

                operand.push(c);

                while consuming {
                    if let Some(c_digit) = iter.peek(){
                        if is_digit(c_digit) {
                            operand.push(*c_digit);
                        } else {
                            consuming = false;
                        }
                    } else {
                        consuming = false;
                    }

                    if consuming {
                        iter.next();
                    }
                }

                let n: String = operand.iter().cloned().collect();

                if let Ok(i) = n.parse::<i64>() {
                    tokens.push(Token::Operand(i));
                }
            }
            _ => (), // try match whitespace
        }
    }

    tokens.push(Token::EOF);
    tokens
}

/// Create expressions based off binary operation tokens
fn binary_op_expr(binary_op: &Token, lhs: Expr, rhs: Expr) -> Expr {
    match *binary_op{
        Token::Add => Expr::Addition(Box::new(lhs), Box::new(rhs)),
        Token::Sub => Expr::Subtraction(Box::new(lhs), Box::new(rhs)),
        Token::Multi => Expr::Multiplication(Box::new(lhs), Box::new(rhs)),
        Token::Div => Expr::Division(Box::new(lhs), Box::new(rhs)),
        _ => unreachable!()
    }
}

/// Create the internal operator precedence hierarchy
fn op_precedence(token: &Token) -> i8 {
    match *token {
        Token::Add | Token::Sub => 10,
        Token::Multi | Token::Div => 20,
        _ => -1,
    }
}

/// Fetch a token from the token list. If there are no tokens left, produce an error
fn get_token<'a, 'b>(tokens: &'a Vec<Token>, current_token: &'b mut usize) -> Result<&'a Token, &'static str> {
    if let Some(token) = tokens.get(*current_token){
        Ok(token)
    } else {
        Err("Token list is exhausted.")
    }
}

/// Consume a token -> moves to the next token
fn eat_token(current_token: &mut usize) {
    *current_token += 1;
}

/// Executes parsing of tokens produced by lexer.
/// Builds an expression tree.
/// Method: Recursive descent with operator precedence climbing
/// Should call lex(&...) before parsing.
fn parse(tokens: &Vec<Token>) -> Result<Expr, &'static str> {
    if tokens.len() == 0 {
        return Err("Failed to parse: No tokens.");
    }

    let mut current_token: usize = 0;

    parse_expr(tokens, &mut current_token)
}

/// Parse the Left hand side (lhs) first - this will be the 'primary'.
/// Based on the lhs, evaluate the remaining tokens on the rhs
fn parse_expr(tokens: &Vec<Token>, current_token: &mut usize) -> Result<Expr, &'static str> {
    let lhs = parse_primary(tokens, current_token)?;
    parse_binary_ops_rhs(tokens, current_token, 0, lhs)
}

/// Checks that the primary/opening token for the expression is valid (i.e. left bracket) and sends
/// to relevant expression parsing variants
fn parse_primary(tokens: &Vec<Token>, current_token: &mut usize) -> Result<Expr, &'static str> {
    let token = get_token(tokens, current_token)?;
    match *token {
        Token::Sub => parse_negate(tokens, current_token),
        Token::Operand(_) => parse_numeral(tokens, current_token),
        Token::Lbracket => parse_bracket(tokens, current_token),
        _ => Err("Unrecognised primary expression. Only negation, left bracket & operands allowed.")
    }
}

fn parse_binary_ops_rhs(tokens: &Vec<Token>, current_token: &mut usize, expr_precedence: i8, mut left_expr: Expr)
    -> Result<Expr, &'static str> {
    loop {
        let token = get_token(tokens, current_token)?;
        let token_precedence = op_precedence(token);

        if token_precedence < expr_precedence {
            return Ok(left_expr)
        } else {
            // since non-binary operators have negative precedence, the token must be a binary op.
            eat_token(current_token); // eat binary op

            let mut right_expr = parse_primary(tokens, current_token)?;

            // Decipher if the next operator should take the current RHS as its LHS.
            // We do this by assessing the next precedence

            let next_op = get_token(tokens, current_token)?;
            let next_precedence = op_precedence(next_op);

            if token_precedence < next_precedence {
                right_expr = parse_binary_ops_rhs(tokens,
                                                  current_token,
                                                  expr_precedence + 1,
                                                  right_expr)?
            }

            left_expr = binary_op_expr(token, left_expr, right_expr)
        }
    }
}

fn parse_negate(tokens: &Vec<Token>, current_token: &mut usize) -> Result<Expr, &'static str> {
    eat_token(current_token); // eat the minus

    let token = get_token(tokens, current_token)?;
    match *token {
        Token::Operand(i) => {
            eat_token(current_token); // eat operand
            Ok(Expr::Negate(Box::new(Expr::Operand(i))))
        },
        _ => Err(dbg!("Failed to negate: Can only negate an operand"))
    }
}

fn parse_numeral(tokens: &Vec<Token>, current_token: &mut usize) -> Result<Expr, &'static str> {
    let token = get_token(tokens, current_token)?;

    match *token {
        Token::Operand(i) => {
            eat_token(current_token); // eat operand
            Ok(Expr::Operand(i))
        },
        _ => unreachable!()
    }
}

fn parse_bracket(tokens: &Vec<Token>, current_token: &mut usize) -> Result<Expr, &'static str> {
    eat_token(current_token); // eat left bracket

    let expr = parse_expr(tokens, current_token)?;

    let token = get_token(tokens, current_token)?;

    match *token {
        Token::Rbracket => {
            eat_token(current_token); // eat right bracket
            Ok(expr)
        },
        _ => Err("Unmatched/unclosed left bracket (parenthesis)"),
    }
}
