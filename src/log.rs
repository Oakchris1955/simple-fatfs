//! Re-exports of [`log`]s macros that act as noop for release builds
//!
//! Use them to not pollute other libraries or applications with our logging
//! messages that aren't useful for them.
#![allow(unused_imports)]

// expr_nop macro is a noop copy of [`log`]'s message macros
// [`log`] can be found here: <https://github.com/rust-lang/log>
macro_rules! expr_nop {
    (logger: $logger:expr, target: $target:expr, $($arg:tt)+) => {};

    (logger: $logger:expr, $($arg:tt)+) => {};

    (target: $target:expr, $($arg:tt)+) => {};

    ($($arg:tt)+) => {};
}

mod noop_macros {
    pub(crate) use expr_nop as debug;
    pub(crate) use expr_nop as error;
    pub(crate) use expr_nop as info;
    pub(crate) use expr_nop as trace;
    pub(crate) use expr_nop as warn;
}

mod macros {
    use super::*;

    #[cfg(feature = "defmt")]
    pub(crate) use defmt::{debug, error, info, trace, warn};
    #[cfg(feature = "log")]
    pub(crate) use log::{debug, error, info, trace, warn};
    #[cfg(all(not(feature = "log"), not(feature = "defmt")))]
    pub(crate) use noop_macros::*;
}

/// Logging macros for messages useful for local development and normally
/// wouldn't be useful to downstream consumers
pub(crate) mod local_log {
    use super::*;

    #[cfg(all(debug_assertions, feature = "log"))]
    #[allow(unused_imports)]
    pub(crate) use macros::{debug, error, info, trace, warn};

    #[cfg(any(not(debug_assertions), not(feature = "log")))]
    #[allow(unused_imports)]
    pub(crate) use noop_macros::*;
}

/// Logging macros for messages that normally would be useful to downstream consumers
pub(crate) mod global_log {
    use super::*;

    #[allow(unused_imports)]
    pub(crate) use macros::{debug, error, info, trace, warn};
}
