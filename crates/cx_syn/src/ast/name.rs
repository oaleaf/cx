use crate::ast::SourceCodeSpan;

#[derive(Copy, Clone, Debug, Ord, PartialOrd, Eq, PartialEq, Hash)]
pub struct Name<'input> {
    name: &'input str,
    span: SourceCodeSpan,
}

impl<'input> Name<'input> {
    pub fn new(name: &'input str, span: SourceCodeSpan) -> Self {
        Self { name, span }
    }

    pub fn name(&self) -> &'input str {
        self.name
    }

    pub fn span(&self) -> SourceCodeSpan {
        self.span
    }
}

impl<'input> AsRef<str> for Name<'input> {
    fn as_ref(&self) -> &str {
        self.name
    }
}

impl<'a> From<Name<'a>> for NameOwned {
    fn from(name: Name<'a>) -> Self {
        Self {
            name: name.name.into(),
            span: name.span,
        }
    }
}

#[derive(Clone, Debug, Ord, PartialOrd, Eq, PartialEq, Hash)]
pub struct NameOwned {
    name: String,
    span: SourceCodeSpan,
}

impl NameOwned {
    pub fn new(name: String, span: SourceCodeSpan) -> Self {
        Self { name, span }
    }

    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn span(&self) -> SourceCodeSpan {
        self.span
    }
}

impl<'a> From<&'a NameOwned> for Name<'a> {
    fn from(n: &'a NameOwned) -> Self {
        Self {
            name: &n.name,
            span: n.span,
        }
    }
}

pub trait NameOwner<'input> {
    fn name<'a>(&'a self) -> Name<'input>
    where
        'a: 'input;
}

impl<'input> NameOwner<'input> for Name<'input> {
    fn name<'a>(&'a self) -> Name<'input>
    where
        'a: 'input,
    {
        *self
    }
}

impl<'input> NameOwner<'input> for NameOwned {
    fn name<'a>(&'a self) -> Name<'input>
    where
        'a: 'input,
    {
        Name::from(self)
    }
}
