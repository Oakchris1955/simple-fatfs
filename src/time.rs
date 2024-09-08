use core::fmt;

use time::{macros::date, OffsetDateTime, PrimitiveDateTime};

pub(crate) const EPOCH: PrimitiveDateTime = date!(1980 - 01 - 01).midnight();

/// An object that can measure and return the current time
pub trait Clock: fmt::Debug {
    /// Returns the current date and time in your local timezone
    /// (https://learn.microsoft.com/en-us/windows/win32/sysinfo/file-times)
    fn now(&self) -> PrimitiveDateTime;
}

/// The default [`Clock`] component
///
/// Returns the current local time in a `std` environment.
/// In a `no-std` environment, it just returns the [`EPOCH`]
#[derive(Debug)]
#[allow(missing_copy_implementations)]
pub struct DefaultClock;

pub(crate) static STATIC_DEFAULT_CLOCK: DefaultClock = DefaultClock {};

impl Default for DefaultClock {
    fn default() -> Self {
        DefaultClock {}
    }
}

impl Clock for DefaultClock {
    fn now(&self) -> PrimitiveDateTime {
        #[cfg(feature = "std")]
        {
            // https://stackoverflow.com/a/76149536/

            // TODO: make the trait return an error to handle such cases
            let now_odt = OffsetDateTime::now_local().unwrap();
            let now_pdt = PrimitiveDateTime::new(now_odt.date(), now_odt.time());

            now_pdt
        }
        #[cfg(not(feature = "std"))]
        EPOCH
    }
}
