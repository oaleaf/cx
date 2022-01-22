use crate::ast::Comma;

#[derive(Debug)]
pub struct CommaList<T> {
    pub elements: Vec<(T, Comma)>,
    pub trailing_element: Option<Box<T>>,
}

impl<T> CommaList<T> {
    pub fn new(elements: Vec<(T, Comma)>, trailing_element: Option<T>) -> Self {
        Self {
            elements,
            trailing_element: trailing_element.map(Box::new),
        }
    }
}
