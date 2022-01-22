use std::collections::BTreeMap;
use std::ops::Index;
use string_interner::DefaultSymbol;

#[derive(Debug)]
pub struct CXContext {
    pub string_interner: CXStringInterner,
    pub namespace_context: CXNamespaceContext,
}

impl CXContext {
    pub fn new() -> Self {
        Self {
            string_interner: CXStringInterner::new(),
            namespace_context: CXNamespaceContext::new(),
        }
    }
}

#[derive(Debug)]
pub struct CXStringInterner {
    interner: string_interner::StringInterner,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct CXInternedString(DefaultSymbol);

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct CXInternedName(CXInternedString);

impl CXStringInterner {
    pub fn new() -> Self {
        Self {
            interner: string_interner::StringInterner::new(),
        }
    }

    pub fn intern<T>(&mut self, s: T) -> CXInternedString
    where
        T: AsRef<str>,
    {
        CXInternedString(self.interner.get_or_intern(s))
    }

    pub fn intern_name<T>(&mut self, s: T) -> CXInternedName
    where
        T: AsRef<str>,
    {
        CXInternedName(self.intern(s))
    }
}

impl Index<CXInternedString> for CXStringInterner {
    type Output = str;

    fn index(&self, s: CXInternedString) -> &str {
        self.interner.resolve(s.0).expect("missing from interner")
    }
}

impl Index<CXInternedName> for CXStringInterner {
    type Output = str;

    fn index(&self, index: CXInternedName) -> &Self::Output {
        &self[index.0]
    }
}

#[derive(Debug)]
pub struct CXNamespaceContext {
    pub namespaces: BTreeMap<CXNamespacePath, CXNamespace>,
    pub global_namespace: CXNamespace,
}

impl CXNamespaceContext {
    pub fn new() -> Self {
        Self {
            namespaces: BTreeMap::new(),
            global_namespace: CXNamespace::new_global(),
        }
    }
}

#[derive(Debug)]
pub struct CXTyRepr {
    pub core: CXTyBase,
    pub sub: Option<CXGenericSubstitutionMap>,
}

#[derive(Debug)]
pub struct CXGenericSubstitutionMap {
    pub substitutions: BTreeMap<CXInternedName, CXGenericSubstitution>,
}

#[derive(Debug)]
pub struct CXGenericSubstitution {
    pub ty: CXTyRepr,
}

#[derive(Debug)]
pub enum CXTyBase {
    Primitive(CXPrimitiveTy),
    Struct(CXStructTy),
    Adt(CXAdtTy),
}

#[derive(Debug)]
pub enum CXPrimitiveTy {
    I8,
    U8,
    I16,
    U16,
    I32,
    U32,
    Isize,
    Usize,
    Str,
    Unit,
}

#[derive(Debug)]
pub struct CXStructTy {
    pub name: CXInternedName,
    pub generic_variables: Option<CxGenericVariables>,
}

#[derive(Debug)]
pub struct CXAdtTy {
    pub variants: Vec<CXAdtVariant>,
    pub generic_variables: Option<CxGenericVariables>,
}

#[derive(Debug)]
pub struct CXAdtVariant {
    pub name: CXInternedName,
    pub data: Option<CXStructTy>,
}

#[derive(Debug)]
pub struct CxGenericVariables {
    pub vars: Vec<CXGenericVariable>,
}

#[derive(Debug)]
pub struct CXGenericVariable {
    pub name: CXInternedName,
    pub bounds: Vec<CXGenericBound>,
}

#[derive(Debug)]
pub struct CXGenericBound {
    pub bound: CXAbstractTy,
}

#[derive(Debug)]
pub struct CXAbstractTy {}

#[derive(Debug)]
pub struct CXNamespace {
    pub namespace_path: CXNamespacePath,
    pub items: Vec<CXNamespaceItem>,
}

impl CXNamespace {
    fn new_global() -> Self {
        Self {
            namespace_path: CXNamespacePath { fragments: vec![] },
            items: Vec::new(),
        }
    }
}

#[derive(Debug)]
pub enum CXNamespaceItem {
    StructDecl(CXStructDecl),
    EnumDecl(CXEnumDecl),
    FunctionDecl(CXFunctionDecl),
    Namespace(CXNamespace),
}

#[derive(Debug)]
pub struct CXStructDecl {
    pub name: CXInternedName,
    pub fields: Vec<CXStructField>,
}

#[derive(Debug)]
pub struct CXStructField {
    pub name: CXInternedName,
    pub ty: CXTyRepr,
}

#[derive(Debug)]
pub struct CXEnumDecl {
    pub name: CXInternedName,
    pub variants: Vec<CXEnumVariant>,
}

#[derive(Debug)]
pub struct CXEnumVariant {
    pub name: CXInternedName,
    pub resolved_tag: i64,
}

#[derive(Debug)]
pub struct CXFunctionDecl {
    pub name: CXInternedName,
    pub params: Vec<CXPositionalParam>,
    pub default_params: Vec<CXDefaultParam>,
    pub return_ty: CXTyRepr,
}

impl PartialEq for CXNamespace {
    fn eq(&self, other: &Self) -> bool {
        self.namespace_path == other.namespace_path
    }
}

#[derive(Debug)]
pub struct CXPositionalParam {
    pub name: CXInternedName,
    pub ty: CXTyRepr,
}

#[derive(Debug)]
pub struct CXDefaultParam {
    pub name: CXInternedName,
    pub ty: CXTyRepr,
    pub default_value: CXDefaultValue,
}

#[derive(Debug)]
pub struct CXDefaultValue {
    pub expr: CXExpr,
}

#[derive(Debug)]
pub enum CXExpr {}

#[derive(Debug, Ord, PartialOrd, Eq, PartialEq)]
pub struct CXNamespacePath {
    pub fragments: Vec<CXNamespaceNameFrag>,
}

#[derive(Debug, Clone, Ord, PartialOrd, Eq, PartialEq, Hash)]
pub struct CXNamespaceNameFrag {
    pub name: CXInternedName,
}
