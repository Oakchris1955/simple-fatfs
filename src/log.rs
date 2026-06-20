/// Re-exports of [`log`]s macros that act as noop for release builds
///
/// Use them to not pollute other libraries or applications with our logging
/// messages that aren't useful for them.

// expr_nop macro is a noop copy of [`log`]'s message macros
// [`log`] can be found here: <https://github.com/rust-lang/log>

#[cfg(not(debug_assertions))]
macro_rules! expr_nop {
    (logger: $logger:expr, target: $target:expr, $($arg:tt)+) => {};

    (logger: $logger:expr, $($arg:tt)+) => {};

    (target: $target:expr, $($arg:tt)+) => {};

    ($($arg:tt)+) => {};
}

#[cfg(debug_assertions)]
#[allow(unused_imports)]
pub(crate) use log::{debug, error, info, trace, warn};

#[cfg(not(debug_assertions))]
#[allow(unused_imports)]
pub(crate) use expr_nop as debug;
#[cfg(not(debug_assertions))]
#[allow(unused_imports)]
pub(crate) use expr_nop as error;
#[cfg(not(debug_assertions))]
#[allow(unused_imports)]
pub(crate) use expr_nop as info;
#[cfg(not(debug_assertions))]
#[allow(unused_imports)]
pub(crate) use expr_nop as trace;
#[cfg(not(debug_assertions))]
#[allow(unused_imports)]
pub(crate) use expr_nop as warn;
