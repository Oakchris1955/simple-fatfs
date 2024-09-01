/// Sets the low `n` bits of a [`u32`] to `1`
///
/// https://users.rust-lang.org/t/how-to-make-an-integer-with-n-bits-set-without-overflow/63078/3
pub(crate) fn setbits_u32(n: u8) -> u32 {
    u32::MAX >> (u32::BITS - u32::from(n))
}
