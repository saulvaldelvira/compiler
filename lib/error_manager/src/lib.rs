use core::any::Any;
use core::fmt;
use std::{borrow::Cow, io};

pub use span::{FilePosition, Span};

/// An error sent to the [`ErrorManager`]
///
/// # Rationale for `Error: Any`
/// [ErrorManager] stores errors on a `Box<dyn Error>`. This is
/// convenient, beacause we can pass arround the same `ErrorManager` to
/// multiple compilation stages, each one defining custom error types.
/// Since they all implement [Error], they can be emitted to the same ErrorManager.
///
/// The problem comes when we need to get back the original Error type.
/// For example, if we're testing the identification stage, we know it'll send
/// IdentificationError to the ErrorManager.
///
/// If Error extends Any we can upcast the `&dyn Error`, to `&dyn Any`,
/// and use `downcast_ref` to get the concrete error type
///
/// ## Example
/// ```
/// use error_manager::{Error, ErrorManager, Span};
/// use core::any::Any;
/// use core::fmt;
///
/// struct MyError(Span);
///
/// impl Error for MyError {
///     fn get_span(&self) -> Span { self.0 }
///     fn write_msg(&self, out: &mut dyn fmt::Write) -> fmt::Result {
///         Ok(())
///     }
/// }
///
/// fn do_something(em: &mut ErrorManager) {
///     // Something goes wrong...
///     em.emit_error(MyError(Span { offset: 12, len: 6 }));
/// }
///
/// let mut em = ErrorManager::new();
/// do_something(&mut em);
///
/// // Now we test the collected errors
/// let mut errors = em.errors().iter().map(|err| {
///     let err: &dyn Any = &**err;
///     err.downcast_ref::<MyError>().unwrap()
/// });
///
/// let expected = errors.next().unwrap();
/// assert_eq!(expected.0, Span { offset: 12, len: 6 });
/// ```
///
/// This allows us to test errors more effectively.
/// Another option would be to write the error we get from the ErrorManager
/// into a String, and test that string.
/// But that would require memory allocations, and more virtual calls.
pub trait Error : Any {
    fn get_span(&self) -> Span;
    fn write_msg(&self, out: &mut dyn fmt::Write) -> fmt::Result;
}

pub struct StringError {
    pub msg: Cow<'static, str>,
    pub span: Span,
}

impl Error for StringError {
    fn write_msg(&self, out: &mut dyn fmt::Write) -> fmt::Result {
        write!(out, "{}", self.msg)
    }

    fn get_span(&self) -> Span { self.span }
}

/// Registers [errors](Error)
pub struct ErrorManager {
    errors: Vec<Box<dyn Error>>,
    warnings: Vec<Box<dyn Error>>,
}

fn print_error(err: &dyn Error, src: &str, out: &mut dyn fmt::Write) -> fmt::Result {
    let FilePosition {
        start_line,
        start_col,
        ..
    } = err.get_span().file_position(src);
    write!(out, "[{start_line}:{start_col}]: ")?;
    err.write_msg(out)?;
    writeln!(out)
}

impl ErrorManager {
    pub fn new() -> Self {
        Self {
            errors: Vec::new(),
            warnings: Vec::new(),
        }
    }

    pub fn emit_error(&mut self, err: impl Error + 'static) { self.errors.push(Box::new(err)); }

    pub fn emit_warning(&mut self, err: impl Error + 'static) { self.warnings.push(Box::new(err)); }

    pub fn n_errors(&self) -> usize { self.errors.len() }

    pub fn has_errors(&self) -> bool { !self.errors.is_empty() }

    pub fn errors(&self) -> &[Box<dyn Error>] { &self.errors }

    /// Gets an iterator over the errors inside `self`, casting them to the
    /// concrete error type specified
    ///
    /// This is **super** unsafe, and should only be used on tests
    ///
    /// # Panics
    /// If if fails to downcast an error into `E`
    pub fn errors_iterator_cast<E: Error>(&self) -> impl Iterator<Item = &E> {
        self.errors.iter().map(|err| {
            let err: &dyn Any = &**err;
            err.downcast_ref::<E>().unwrap()
        })
    }

    pub fn print_errors(&self, src: &str, out: &mut dyn io::Write) -> fmt::Result {
        let mut buf = String::new();
        for err in &self.errors {
            out.write_all("ERROR ".as_bytes()).unwrap();
            print_error(&**err, src, &mut buf)?;
            out.write_all(buf.as_bytes()).unwrap();
            buf.clear();
        }
        Ok(())
    }

    pub fn n_warnings(&self) -> usize { self.warnings.len() }

    pub fn clear_warnings(&mut self) { self.warnings.clear(); }

    pub fn print_warnings(&self, src: &str, out: &mut dyn io::Write) -> fmt::Result {
        let mut buf = String::new();
        for err in &self.warnings {
            out.write_all("WARNING ".as_bytes()).unwrap();
            print_error(&**err, src, &mut buf)?;
            out.write_all(buf.as_bytes()).unwrap();
            buf.clear();
        }
        Ok(())
    }
}

impl Default for ErrorManager {
    fn default() -> Self { Self::new() }
}
