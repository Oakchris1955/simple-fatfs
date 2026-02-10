use core::marker::PhantomData;
use zerocopy::{FromBytes, Immutable, IntoBytes};

#[repr(transparent)]
#[derive(Immutable, FromBytes, IntoBytes, Clone, Copy, Debug, Default)]
pub(crate) struct Bitfield<R, I, B> {
    repr: R,
    _i: PhantomData<I>,
    _b: PhantomData<B>,
}

impl<R: Into<I>, I: Into<B>, B> Bitfield<R, I, B> {
    pub(crate) fn get(self) -> B {
        let intermediate: I = self.repr.into();
        intermediate.into()
    }
}

impl<B: Into<I>, I: Into<R>, R> From<B> for Bitfield<R, I, B> {
    fn from(value: B) -> Self {
        let intermediate: I = value.into();
        Self {
            repr: intermediate.into(),
            _i: PhantomData,
            _b: PhantomData,
        }
    }
}
