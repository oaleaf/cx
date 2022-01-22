#![allow(dead_code)] // TODO: remove

pub mod comma_list;
pub mod name;

use std::fmt::Debug;

use comma_list::CommaList;
use name::Name;

#[derive(Debug)]
pub struct AstStruct {
    struct_kw: StructKw,
}

impl AstStruct {
    pub fn new(struct_kw: StructKw) -> Self {
        AstStruct { struct_kw }
    }
}

pub trait FromSpan {
    fn from_span(span: SourceCodeSpan) -> Self;
}

pub trait Keyword {
    fn keyword(&self) -> &'static str;
}

macro_rules! token {
    ($name:ident) => {
        #[derive(Copy, Clone, Debug, Ord, PartialOrd, Eq, PartialEq, Hash)]
        pub struct $name {
            pub span: SourceCodeSpan,
        }

        impl FromSpan for $name {
            fn from_span(span: SourceCodeSpan) -> Self {
                Self { span }
            }
        }
    };
}

token!(OpenParen);
token!(CloseParen);
token!(OpenBracket);
token!(CloseBracket);
token!(OpenBrace);
token!(CloseBrace);
token!(Dot);
token!(Colon);
token!(Colon2);
token!(At);
token!(Hash);
token!(Comma);
token!(Plus);
token!(Minus);
token!(Asterisk);
token!(Slash);
token!(Percent);
token!(Amp2);
token!(Pipe2);
token!(Bang);

token!(Eq);

token!(Lt);
token!(Gt);
token!(Lte);
token!(Gte);
token!(Eq2);
token!(BangEq);

token!(Newline);
token!(NewlineContinuation);

macro_rules! kw {
    ($name:ident, $kw:literal) => {
        token!($name);

        impl Keyword for $name {
            fn keyword(&self) -> &'static str {
                $kw
            }
        }
    };
}

kw!(FnKw, "fn");
kw!(StructKw, "struct");
kw!(UsnKw, "usn");

type TextPos = u16;

#[derive(Copy, Clone, Ord, PartialOrd, Eq, PartialEq, Hash)]
pub struct SourceCodeSpan {
    pub left: TextPos,
    pub right: TextPos,
}

impl SourceCodeSpan {
    pub fn new(left: TextPos, right: TextPos) -> Self {
        Self { left, right }
    }
}

impl Debug for SourceCodeSpan {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}..{}", self.left, self.right)
    }
}

#[derive(Debug)]
pub struct FnDecl<'input> {
    pub fn_kw: FnKw,
    pub name: Name<'input>,
    pub fn_param_list: FnParams<'input>,
    pub ret_ty: Option<RetTy<'input>>,
    pub body: Block<'input>,
}

impl<'input> FnDecl<'input> {
    pub fn new(
        fn_kw: FnKw,
        name: Name<'input>,
        fn_param_list: FnParams<'input>,
        ret_ty: Option<RetTy<'input>>,
        body: Block<'input>,
    ) -> Self {
        Self {
            fn_kw,
            name,
            fn_param_list,
            ret_ty,
            body,
        }
    }
}

#[derive(Debug)]
pub struct FnParams<'input> {
    pub open_paren: OpenParen,
    pub params: FnParamList<'input>,
    pub close_paren: CloseParen,
}

impl<'input> FnParams<'input> {
    pub fn new(
        open_paren: OpenParen,
        params: FnParamList<'input>,
        close_paren: CloseParen,
    ) -> Self {
        Self {
            open_paren,
            params,
            close_paren,
        }
    }
}

#[derive(Debug)]
pub struct FnParamList<'input> {
    pub pos_params: CommaList<PosFnParam<'input>>,
    pub default_params: CommaList<DefaultFnParam<'input>>,
}

impl<'input> FnParamList<'input> {
    pub fn new(
        params: (
            CommaList<PosFnParam<'input>>,
            CommaList<DefaultFnParam<'input>>,
        ),
    ) -> Self {
        Self {
            pos_params: params.0,
            default_params: params.1,
        }
    }
}

#[derive(Debug)]
pub struct PosFnParam<'input> {
    pub name: Name<'input>,
    pub colon: Option<Colon>,
    pub ty: Ty<'input>,
}

impl<'input> PosFnParam<'input> {
    pub fn new(name: Name<'input>, colon: Option<Colon>, ty: Ty<'input>) -> Self {
        Self { name, colon, ty }
    }
}

#[derive(Debug)]
pub struct DefaultFnParam<'input> {
    pub name: Name<'input>,
    pub colon: Option<Colon>,
    pub ty: Option<Ty<'input>>,
    pub eq: Eq,
    pub default: Expr<'input>,
}

impl<'input> DefaultFnParam<'input> {
    pub fn new(
        name: Name<'input>,
        ty: Option<(Option<Colon>, Ty<'input>)>,
        eq: Eq,
        default: Expr<'input>,
    ) -> Self {
        let (colon, ty) = ty.map_or((None, None), |(a, b)| (a, Some(b)));
        Self {
            name,
            colon,
            ty,
            eq,
            default,
        }
    }
}

#[derive(Debug)]
pub enum Ty<'input> {
    Plain(Name<'input>),
}

impl<'input> Ty<'input> {
    pub fn plain(name: Name<'input>) -> Self {
        Self::Plain(name)
    }
}

#[derive(Debug)]
pub struct RetTy<'input> {
    pub colon: Option<Colon>,
    pub ty: Ty<'input>,
}

impl<'input> RetTy<'input> {
    pub fn new(colon: Option<Colon>, ty: Ty<'input>) -> Self {
        Self { colon, ty }
    }
}

#[derive(Debug)]
pub struct Block<'input> {
    pub open_brace: OpenBrace,
    pub item_list: ItemList<'input>,
    pub close_brace: CloseBrace,
}

impl<'input> Block<'input> {
    pub fn new(
        open_brace: OpenBrace,
        item_list: ItemList<'input>,
        close_brace: CloseBrace,
    ) -> Self {
        Self {
            open_brace,
            item_list,
            close_brace,
        }
    }
}

#[derive(Debug)]
pub struct ItemList<'input> {
    pub item_list: Vec<Item<'input>>,
}

impl<'input> ItemList<'input> {
    pub fn new(item_list: Vec<Item<'input>>) -> Self {
        Self { item_list }
    }
}

#[derive(Debug)]
pub struct StmtList<'input> {
    pub stmt_list: Vec<Stmt<'input>>,
}

impl<'input> StmtList<'input> {
    pub fn new(stmt_list: Vec<Stmt<'input>>) -> Self {
        Self { stmt_list }
    }
}

#[derive(Debug)]
pub enum Stmt<'input> {
    Expr(Expr<'input>),
    None,
}

#[derive(Debug)]
pub enum Expr<'input> {
    Block(Block<'input>),
    Name(Name<'input>),
    Number(Number<'input>),

    Tuple(OpenParen, CommaList<Expr<'input>>, CloseParen),
    Field(
        Box<Expr<'input>>,
        Option<NewlineContinuation>,
        Dot,
        Name<'input>,
    ),
    Index(
        Box<Expr<'input>>,
        OpenBracket,
        Box<Expr<'input>>,
        CloseBracket,
    ),
    Call(Box<Expr<'input>>, Dot, FnArgs<'input>),

    InfixBinOp(Box<Expr<'input>>, InfixBinOp, Box<Expr<'input>>),
    PrefixUnaryOp(PrefixUnaryOp, Box<Expr<'input>>),
}

macro_rules! op {
    ($kind_name:ident, [$($name:ident($t:ty)),* $(,)?]) => {
        #[derive(Debug)]
        pub enum $kind_name {
            $($name($t),)*
        }
        $(
            impl From<$t> for $kind_name {
                fn from(op: $t) -> Self {
                    Self::$name (op)
                }
            }
        )*
    };
}

op!(
    InfixBinOp,
    [
        Plus(Plus),
        Minus(Minus),
        Times(Asterisk),
        Divide(Slash),
        Modulo(Percent),
        And(Amp2),
        Or(Pipe2),
        Lt(Lt),
        Gt(Gt),
        Lte(Lte),
        Gte(Gte),
        Eq(Eq2),
        BangEq(BangEq),
    ]
);

op!(PrefixUnaryOp, [Plus(Plus), Minus(Minus), LogicalNot(Bang),]);

#[derive(Debug)]
pub struct FnArgs<'input> {
    pub open_paren: OpenParen,
    pub params: FnArgList<'input>,
    pub close_paren: CloseParen,
}

impl<'input> FnArgs<'input> {
    pub fn new(open_paren: OpenParen, params: FnArgList<'input>, close_paren: CloseParen) -> Self {
        Self {
            open_paren,
            params,
            close_paren,
        }
    }
}

#[derive(Debug)]
pub struct FnArgList<'input> {
    pub pos_args: CommaList<PosFnArg<'input>>,
    pub default_args: CommaList<DefaultFnArg<'input>>,
}

impl<'input> FnArgList<'input> {
    pub fn new(args: (CommaList<PosFnArg<'input>>, CommaList<DefaultFnArg<'input>>)) -> Self {
        Self {
            pos_args: args.0,
            default_args: args.1,
        }
    }
}

#[derive(Debug)]
pub struct PosFnArg<'input> {
    pub expr: Expr<'input>,
}

impl<'input> PosFnArg<'input> {
    pub fn new(expr: Expr<'input>) -> Self {
        Self { expr }
    }
}

#[derive(Debug)]
pub struct DefaultFnArg<'input> {
    pub name: Name<'input>,
    pub eq: Eq,
    pub expr: Expr<'input>,
}

impl<'input> DefaultFnArg<'input> {
    pub fn new(name: Name<'input>, eq: Eq, expr: Expr<'input>) -> Self {
        Self { name, eq, expr }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Number<'input> {
    pub value: &'input str,
    pub base: NumberBase,
    pub suffix: Option<NumberSuffix>,
    pub span: SourceCodeSpan,
}

impl<'input> Number<'input> {
    pub fn new_bin(v: &'input str, span: SourceCodeSpan) -> Self {
        Self {
            value: v,
            base: NumberBase::Bin,
            suffix: NumberSuffix::parse_from_ending(v),
            span,
        }
    }

    pub fn new_oct(v: &'input str, span: SourceCodeSpan) -> Self {
        Self {
            value: v,
            base: NumberBase::Oct,
            suffix: NumberSuffix::parse_from_ending(v),
            span,
        }
    }

    pub fn new_dec(v: &'input str, span: SourceCodeSpan) -> Self {
        Self {
            value: v,
            base: NumberBase::Dec,
            suffix: NumberSuffix::parse_from_ending(v),
            span,
        }
    }

    pub fn new_hex(v: &'input str, span: SourceCodeSpan) -> Self {
        Self {
            value: v,
            base: NumberBase::Hex,
            suffix: NumberSuffix::parse_from_ending(v),
            span,
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum NumberBase {
    Bin,
    Oct,
    Dec,
    Hex,
}

#[derive(Debug, Clone, Copy)]
pub enum NumberSuffix {
    I8,
    U8,
    I16,
    U16,
    I32,
    U32,
    I64,
    U64,
    Isize,
    Usize,
}

impl NumberSuffix {
    fn parse_from_ending(s: &str) -> Option<Self> {
        let r = if s.ends_with("i8") {
            Self::I8
        } else if s.ends_with("u8") {
            Self::U8
        } else if s.ends_with("i16") {
            Self::I16
        } else if s.ends_with("u16") {
            Self::U16
        } else if s.ends_with("i32") {
            Self::I32
        } else if s.ends_with("u32") {
            Self::U32
        } else if s.ends_with("i64") {
            Self::I64
        } else if s.ends_with("u64") {
            Self::U64
        } else if s.ends_with("isize") {
            Self::Isize
        } else if s.ends_with("usize") {
            Self::Usize
        } else {
            return None;
        };

        Some(r)
    }
}

#[derive(Debug)]
pub enum Item<'input> {
    UsnStmt(UsnStmt<'input>),
    Fn(FnDecl<'input>),
    Stmt(Stmt<'input>),
    None,
}

#[derive(Debug)]
pub struct SourceFileRoot<'input> {
    pub items: Vec<Item<'input>>,
}

impl<'input> SourceFileRoot<'input> {
    pub fn new(items: Vec<Item<'input>>) -> Self {
        Self { items }
    }
}

#[derive(Debug)]
pub struct UsnStmt<'input> {
    pub usn: UsnKw,
    pub path: Path<'input>,
    pub new_line: Newline,
}

impl<'input> UsnStmt<'input> {
    pub fn new(usn: UsnKw, path: Path<'input>, new_line: Newline) -> Self {
        Self {
            usn,
            path,
            new_line,
        }
    }
}

#[derive(Debug)]
pub struct Path<'input> {
    pub leading_colon2: Option<Colon2>,
    pub leading_name: Name<'input>,
    pub trailing_names: Vec<(Colon2, Name<'input>)>,
}

impl<'input> Path<'input> {
    pub fn new(
        leading_colon2: Option<Colon2>,
        leading_name: Name<'input>,
        trailing_names: Vec<(Colon2, Name<'input>)>,
    ) -> Self {
        Self {
            leading_colon2,
            leading_name,
            trailing_names,
        }
    }
}

#[derive(Debug)]
pub struct InnerAttrList<'input> {
    pub attr_list: Vec<InnerAttr<'input>>,
}

impl<'input> InnerAttrList<'input> {
    pub fn new(attr_list: Vec<InnerAttr<'input>>) -> Self {
        Self { attr_list }
    }
}

#[derive(Debug)]
pub enum InnerAttr<'input> {
    Name(Hash, Bang, OpenBracket, Name<'input>, CloseBracket),
    NameEqExpr(
        Hash,
        Bang,
        OpenBracket,
        Name<'input>,
        Eq,
        Expr<'input>,
        CloseBracket,
    ),
}
