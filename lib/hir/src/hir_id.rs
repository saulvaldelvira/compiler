use std::fmt;


#[derive(Clone,Copy)]
pub struct HirId(pub usize);

impl fmt::Debug for HirId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}
