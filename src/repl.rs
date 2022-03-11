use lalrpop_util::lalrpop_mod;
use valheim_core::isa::typed::Reg;
use valheim_core::memory::VirtAddr;

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Stmt {
  Eval(Expr),
  AddWatch(Expr),
  RemoveWatch(u64),
  AddBreakpoint(VirtAddr),
  RemoveBreakpoint(VirtAddr),
  ShowRelevantAsm,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Expr {
  Literal(u64),
  Register(Reg),
  Deref(Box<Expr>),
  Assign(Box<Expr>, Box<Expr>),
  Add(Box<Expr>, Box<Expr>),
  Sub(Box<Expr>, Box<Expr>),
  Mul(Box<Expr>, Box<Expr>),
  Div(Box<Expr>, Box<Expr>),
}

#[derive(Debug, Clone)]
pub struct ReplParseError {
  pub kind: ReplParseErrorKind,
  pub location: usize,
}

#[derive(Debug, Clone)]
pub enum ReplParseErrorKind {
  LiteralTooLarge,
}

lalrpop_mod!(pub grammar);

pub struct ReplParser {}

impl ReplParser {
  pub fn parse(src: &str) -> Option<Stmt> {
    match grammar::StmtParser::new().parse(src) {
      Ok(stmt) => Some(stmt),
      Err(_) => None,
    }
  }
}

