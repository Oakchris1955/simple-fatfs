//! Type aliases for various important data types for readability
//!
//! Some Index types have their corresponding Count type
//! (e.g. [`ClusterIndex`] and [`ClusterCount`]).
//! These are both under the hood aliased to the same data type.
//! The only reason to keep the separated is too make the code more readable:
//! for example, if a function returns a [`ClusterIndex`], we know that it
//! returns the index of a particular cluster, whereas if it were to return
//! [`ClusterCount`], it could return how many clusters are needed for
//! a particular action or belong to an object.

#[cfg(not(feature = "lba64"))]
/// The number/offset/position of a block.
///
/// A block is defined by the [`BlockBase`](crate::fat::BlockBase) trait.
///
/// Depending on the feature `lba64` it is either [`u32`] or [`u64`].
pub type BlockIndex = u32;
#[cfg(feature = "lba64")]
/// The number/offset/position of a block.
///
/// A block is defined by the [`BlockBase`](crate::fat::BlockBase) trait.
///
/// Depending on the feature `lba64` it is either [`u32`] or [`u64`].
pub type BlockIndex = u64;

/// Can be used instead of [`BlockIndex`] to make code more understandable
///
/// Note: since this is a type alias of [`BlockIndex`], the `lba64` feature flag
/// causes its type to change alongside [`BlockIndex`]
pub type BlockCount = BlockIndex;

/// The size type of a block
/// [`u32`] was chosen instead of [`u16`], since some flash memories are known
/// to have block of 64 KBs (which is 1 more byte than what [`u16`] can handle),
/// and in some (rare) cases, even more. Furthemore, since [`u32`] can express
/// block sizes up to 4 GBs, it is more than enough for every flash memory out there.
pub type BlockSize = u32;

pub(crate) type ClusterIndex = u32;
pub(crate) type ClusterCount = ClusterIndex;

pub(crate) type SectorIndex = u32;
pub(crate) type SectorCount = SectorIndex;

pub(crate) type EntryIndex = u16;
pub(crate) type EntryCount = EntryIndex;

pub(crate) type FATEntryIndex = u32;
pub(crate) type FATEntryCount = FATEntryIndex;

pub(crate) type FATEntryValue = u32;

pub(crate) type FileSize = u32;
