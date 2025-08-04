use core::num;

use crate::time::EPOCH;

use ::time;
use akin::akin;
use bincode::{impl_borrow_decode, Decode, Encode};
use bitfield_struct::bitfield;
use time::{Date, PrimitiveDateTime, Time};

#[bitfield(u16)]
#[derive(Encode, Decode)]
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

#[bitfield(u16)]
#[derive(Encode, Decode)]
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
            .with_year((value.year() - EPOCH.year()) as u8)
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

#[derive(Debug, Clone, Copy)]
pub(crate) struct EntryCreationTime(Option<CreationTime>);

#[derive(Encode, Decode, Debug, Clone, Copy)]
pub(crate) struct CreationTime {
    pub(crate) hundredths_of_second: u8,
    pub(crate) time: TimeAttribute,
    pub(crate) date: DateAttribute,
}

impl<Context> bincode::Decode<Context> for EntryCreationTime {
    fn decode<D: bincode::de::Decoder<Context = Context>>(
        decoder: &mut D,
    ) -> Result<Self, bincode::error::DecodeError> {
        let hundredths_of_second = <u8 as bincode::Decode<Context>>::decode(decoder)?;
        let time = <u16 as bincode::Decode<Context>>::decode(decoder)?;
        let date = <u16 as bincode::Decode<Context>>::decode(decoder)?;

        Ok(Self(if time == 0 || date == 0 {
            None
        } else {
            Some(CreationTime {
                hundredths_of_second,
                time: TimeAttribute::from_bits(time),
                date: DateAttribute::from_bits(date),
            })
        }))
    }
}

impl_borrow_decode!(EntryCreationTime);

impl bincode::Encode for EntryCreationTime {
    fn encode<E: bincode::enc::Encoder>(
        &self,
        encoder: &mut E,
    ) -> Result<(), bincode::error::EncodeError> {
        match self.0 {
            Some(creation_time) => {
                bincode::Encode::encode(&creation_time, encoder)?;
            }
            None => {
                akin! {
                    let &field_type = [u8, u16, u16];

                    bincode::Encode::encode(&0_~*field_type, encoder)?;
                };
            }
        }

        Ok(())
    }
}

impl TryFrom<EntryCreationTime> for Option<PrimitiveDateTime> {
    type Error = ();

    fn try_from(value: EntryCreationTime) -> Result<Self, Self::Error> {
        match value.0 {
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
        Self(Some(CreationTime {
            hundredths_of_second: (value.second() % 2) * 100 + (value.millisecond() / 10) as u8,
            time: value.time().into(),
            date: value.date().into(),
        }))
    }
}

impl From<Option<PrimitiveDateTime>> for EntryCreationTime {
    fn from(value: Option<PrimitiveDateTime>) -> Self {
        match value {
            Some(value) => value.into(),
            None => Self(None),
        }
    }
}

#[derive(Encode, Decode, Debug, Clone, Copy)]
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
            time: value.time().into(),
            date: value.date().into(),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub(crate) struct EntryLastAccessedTime(Option<DateAttribute>);

impl<Context> bincode::Decode<Context> for EntryLastAccessedTime {
    fn decode<D: bincode::de::Decoder<Context = Context>>(
        decoder: &mut D,
    ) -> Result<Self, bincode::error::DecodeError> {
        let bits = <u16 as bincode::Decode<Context>>::decode(decoder)?;

        Ok(Self(if bits == 0 {
            None
        } else {
            Some(DateAttribute::from_bits(bits))
        }))
    }
}

impl_borrow_decode!(EntryLastAccessedTime);

impl bincode::Encode for EntryLastAccessedTime {
    fn encode<E: bincode::enc::Encoder>(
        &self,
        encoder: &mut E,
    ) -> Result<(), bincode::error::EncodeError> {
        let bits = match self.0 {
            Some(date) => date.into_bits(),
            None => 0,
        };

        bincode::Encode::encode(&bits, encoder)?;

        Ok(())
    }
}

impl TryFrom<EntryLastAccessedTime> for Option<Date> {
    type Error = ();

    fn try_from(value: EntryLastAccessedTime) -> Result<Self, Self::Error> {
        value.0.map(|date| date.try_into()).transpose()
    }
}

impl From<Date> for EntryLastAccessedTime {
    fn from(value: Date) -> Self {
        Self(Some(value.into()))
    }
}

impl From<Option<Date>> for EntryLastAccessedTime {
    fn from(value: Option<Date>) -> Self {
        match value {
            Some(value) => value.into(),
            None => Self(None),
        }
    }
}
