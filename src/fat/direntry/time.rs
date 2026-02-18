use core::num;

use crate::time::EPOCH;

use ::time;
use bitfield_struct::bitfield;
use time::{Date, PrimitiveDateTime, Time};
use zerocopy::{byteorder::little_endian::U16, FromBytes, Immutable, IntoBytes};

#[bitfield(u16, repr = U16, from = U16::new, into = U16::get)]
#[derive(Immutable, FromBytes, IntoBytes)]
pub(crate) struct TimeAttribute {
    /// Multiply by 2
    #[bits(5)]
    seconds: u8,
    #[bits(6)]
    minutes: u8,
    #[bits(5)]
    hour: u8,
}

impl From<Time> for TimeAttribute {
    fn from(value: Time) -> Self {
        Self::new()
            .with_seconds(value.second() / 2)
            .with_minutes(value.minute())
            .with_hour(value.hour())
    }
}

#[bitfield(u16, repr = U16, from = U16::new, into = U16::get)]
#[derive(Immutable, FromBytes, IntoBytes)]
pub(crate) struct DateAttribute {
    #[bits(5)]
    day: u8,
    #[bits(4)]
    month: u8,
    #[bits(7)]
    year: u8,
}

impl From<Date> for DateAttribute {
    fn from(value: Date) -> Self {
        Self::new()
            .with_day(value.day())
            .with_month(value.month().into())
            .with_year(
                u8::try_from(value.year() - EPOCH.year())
                    .expect("TODO: proper time handling for such a case"),
            )
    }
}

impl TryFrom<TimeAttribute> for Time {
    type Error = ();

    fn try_from(value: TimeAttribute) -> Result<Self, Self::Error> {
        time::parsing::Parsed::new()
            .with_hour_24(value.hour())
            .and_then(|parsed| parsed.with_minute(value.minutes()))
            .and_then(|parsed| parsed.with_second(value.seconds() * 2))
            .and_then(|parsed| parsed.try_into().ok())
            .ok_or(())
    }
}

impl TryFrom<DateAttribute> for Date {
    type Error = ();

    fn try_from(value: DateAttribute) -> Result<Self, Self::Error> {
        time::parsing::Parsed::new()
            .with_year(i32::from(value.year()) + EPOCH.year())
            .and_then(|parsed| parsed.with_month(value.month().try_into().ok()?))
            .and_then(|parsed| parsed.with_day(num::NonZeroU8::new(value.day())?))
            .and_then(|parsed| parsed.try_into().ok())
            .ok_or(())
    }
}

#[derive(Immutable, FromBytes, IntoBytes, Debug, Clone, Copy)]
#[repr(transparent)]
pub(crate) struct EntryCreationTime(CreationTime);

#[derive(Immutable, FromBytes, IntoBytes, Default, Debug, Clone, Copy)]
#[repr(C, packed)]
pub(crate) struct CreationTime {
    pub(crate) hundredths_of_second: u8,
    pub(crate) time: TimeAttribute,
    pub(crate) date: DateAttribute,
}

impl EntryCreationTime {
    pub(crate) fn get(&self) -> Option<CreationTime> {
        (self.0.date.0 != 0 && self.0.time.0 != 0).then_some(self.0)
    }
}

impl TryFrom<EntryCreationTime> for Option<PrimitiveDateTime> {
    type Error = ();

    fn try_from(value: EntryCreationTime) -> Result<Self, Self::Error> {
        match value.get() {
            Some(creation_time) => {
                let mut time: Time = creation_time.time.try_into()?;

                let new_seconds = time.second() + creation_time.hundredths_of_second / 100;
                let milliseconds = u16::from(creation_time.hundredths_of_second) % 100 * 10;
                time = time
                    .replace_second(new_seconds)
                    .map_err(|_| ())?
                    .replace_millisecond(milliseconds)
                    .map_err(|_| ())?;

                let date: Date = creation_time.date.try_into()?;

                Ok(Some(PrimitiveDateTime::new(date, time)))
            }
            None => Ok(None),
        }
    }
}

impl From<PrimitiveDateTime> for EntryCreationTime {
    fn from(value: PrimitiveDateTime) -> Self {
        Self(CreationTime {
            hundredths_of_second: (value.second() % 2) * 100
                + u8::try_from(value.millisecond() / 10).expect("this will be in the range 0..100"),
            time: TimeAttribute::from(value.time()),
            date: DateAttribute::from(value.date()),
        })
    }
}

impl From<Option<PrimitiveDateTime>> for EntryCreationTime {
    fn from(value: Option<PrimitiveDateTime>) -> Self {
        match value {
            Some(value) => value.into(),
            None => Self(Default::default()),
        }
    }
}

#[derive(Immutable, FromBytes, IntoBytes, Debug, Clone, Copy)]
#[repr(C)]
pub(crate) struct EntryModificationTime {
    pub(crate) time: TimeAttribute,
    pub(crate) date: DateAttribute,
}

impl TryFrom<EntryModificationTime> for PrimitiveDateTime {
    type Error = ();

    fn try_from(value: EntryModificationTime) -> Result<Self, Self::Error> {
        Ok(PrimitiveDateTime::new(
            value.date.try_into()?,
            value.time.try_into()?,
        ))
    }
}

impl From<PrimitiveDateTime> for EntryModificationTime {
    fn from(value: PrimitiveDateTime) -> Self {
        Self {
            time: TimeAttribute::from(value.time()),
            date: DateAttribute::from(value.date()),
        }
    }
}

#[derive(Immutable, FromBytes, IntoBytes, Debug, Clone, Copy)]
pub(crate) struct EntryLastAccessedTime(DateAttribute);

impl EntryLastAccessedTime {
    pub(crate) fn get(&self) -> Option<DateAttribute> {
        let i = self.0;
        (i.0 != 0).then_some(i)
    }
}

impl TryFrom<EntryLastAccessedTime> for Option<Date> {
    type Error = ();

    fn try_from(value: EntryLastAccessedTime) -> Result<Self, Self::Error> {
        value.get().map(|date| date.try_into()).transpose()
    }
}

impl From<Date> for EntryLastAccessedTime {
    fn from(value: Date) -> Self {
        Self(DateAttribute::from(value))
    }
}

impl From<Option<Date>> for EntryLastAccessedTime {
    fn from(value: Option<Date>) -> Self {
        match value {
            Some(value) => value.into(),
            None => Self(Default::default()),
        }
    }
}
