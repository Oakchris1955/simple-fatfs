use crate::block_io::{BlockBase, BlockRead, BlockWrite};
pub use crate::fat::types::BlockIndex;
use crate::{BlockCount, BlockSize};
use alloc::boxed::Box;
use core::array;
use core::fmt::{Debug, Display, Formatter};
use core::iter;
use core::ops::{Deref, DerefMut};
use embedded_io::{ErrorKind, ErrorType};

/// Translate between different hardware and software "virtual" block sizes.
///
/// This is useful if, for example the underlying flash can only be written in
/// 64 KiB blocks, but of course 512 byte blocks are used by fat.
/// (In some cases 1, 2 or 4 KiB blocks are used by fat, depending on how
/// it's formatted but it's always ok to have a smaller block size.)
///
/// This is done by creating one or more buffer(s) in the size of a hardware
/// block and the smaller virtual blocks are read/written into the buffer.
///
/// Always use flush on the translation layer, which will save the current
/// block buffer(s) if needed and flushes the underlying level.
///
/// As soon as the translation level is dropped the underlying storage can
/// be accessed again.
///
/// Example:
/// ```rust
/// # use simple_fatfs::block_io::*;
/// # struct Store();
/// # impl embedded_io::ErrorType for Store { type Error = BlockTranslatorError; }
/// impl BlockBase for Store {
///     fn block_size(&self) -> BlockSize { 65536 }
///     // ...
///     # fn block_count(&self) -> BlockCount {
///         # 1
///     # }
/// }
/// # impl BlockRead for Store {
///     # fn read(&mut self, _: BlockIndex, _: &mut [u8]) -> Result<(), Self::Error> { Ok(()) }
/// # }
/// # impl BlockWrite for Store {
///     # fn write(&mut self, _: BlockIndex, _: &[u8]) -> Result<(), Self::Error> { Ok(()) }
///     # fn flush(&mut self) -> Result<(), Self::Error> { Ok(()) }
/// # }
///
/// // create storage
/// let mut storage = Store(/*...*/);
///
/// // create buffer and the translation level
/// let mut buffer = [0u8; 65536];
/// let mut translated = BlockTranslator::<512, _, _, _>::new_with_buffer(&mut storage, [&mut buffer])?;
///
/// // write one block and flush it
/// translated.write(0, &[11; 512])?;
/// translated.flush()?;
///
/// # Ok::<(), BlockTranslatorError>(())
/// ```
///
/// The following must held true, otherwise an error occurs:
/// * virtual block size <= hardware block size <= buffer size
/// * at least one buffer
/// * virtual and hardware block sizes must be greater than 0 and a power of two
///
/// # Errors
///
/// The following errors will be reported at compile time (when using `new`):
/// * number of buffers (BUFS) must be greater than zero
/// * buffer size must be bigger or equal than virtual block size
/// * virtual block size must be a power of two
///
/// The following errors will be returned by `new`:
/// * buffer size must be greater or equal than the hardware block size
/// * hardware block size must be a power of two
/// * hardware block size must greater or equal than virtual block size

#[derive(Debug)]
pub struct BlockTranslator<'a, const VBS: BlockSize, const BUF_SIZE: usize, const BUFS: usize, S>
where
    S: BlockWrite,
{
    vbs_per_hbs: u32,
    buffers: [Buffer<'a, BUF_SIZE>; BUFS],
    storage: S,
    /// BUFS==1: unused
    /// BUFS==2: buffer number of the last used buffer
    /// BUFS>=3: next "timestamp" to be used
    next: usize,
}

#[derive(Debug)]
enum BufferLocation<'a, const BUF_SIZE: usize> {
    Borrowed(&'a mut [u8; BUF_SIZE]),
    Owned(Box<[u8; BUF_SIZE]>),
}

#[derive(Debug)]
struct Buffer<'a, const BUF_SIZE: usize> {
    buffer: BufferLocation<'a, BUF_SIZE>,
    stored_block: BlockIndex,
    status: BlockTranslatorStatus,
    /// BUFS<=2: unused
    /// BUFS>=3: "timestamp" when buffer was used last
    last_used: usize,
}

#[derive(Debug, Eq, PartialEq)]
enum BlockTranslatorStatus {
    Unknown,
    Read,
    Modified,
}

/// This error represents a mismatch between the underlying storage and the supplied block size.
#[non_exhaustive]
#[derive(Copy, Clone)]
pub enum BlockTranslatorError {
    /// Buffer size must be greater or equal than the hardware block size
    BufferSizeTooSmall,
    /// Hardware block size must be a power of two
    HardwareBlockSizeNotPowerOfTwo,
    /// Hardware block size must greater or equal than virtual block size
    HardwareBlockSizeToSmall,
}

impl Display for BlockTranslatorError {
    fn fmt(&self, f: &mut Formatter<'_>) -> core::fmt::Result {
        f.write_str(match self {
            BlockTranslatorError::BufferSizeTooSmall => {
                "buffer size must be greater or equal than the hardware block size"
            }
            BlockTranslatorError::HardwareBlockSizeNotPowerOfTwo => {
                "hardware block size must be a power of two"
            }
            BlockTranslatorError::HardwareBlockSizeToSmall => {
                "hardware block size must greater or equal than virtual block size"
            }
        })
    }
}

impl Debug for BlockTranslatorError {
    fn fmt(&self, f: &mut Formatter<'_>) -> core::fmt::Result {
        use core::fmt::Display;

        Display::fmt(self, f)
    }
}

impl core::error::Error for BlockTranslatorError {}

impl embedded_io::Error for BlockTranslatorError {
    fn kind(&self) -> ErrorKind {
        ErrorKind::InvalidData
    }
}

impl<const VBS: BlockSize, const BUF_SIZE: usize, const BUFS: usize, S>
    BlockTranslator<'static, VBS, BUF_SIZE, BUFS, S>
where
    S: BlockWrite,
{
    /// Create a new BlockTranslator.
    ///
    /// Example:
    /// Create a BlockTranslator with 1 buffer of 65536 bytes and 512 bytes of virtual block size.
    /// ```no_compile
    /// let mut translated = BlockTranslator::<512, 65536, 1, _>::new(&mut storage)?;
    /// ```
    pub fn new(storage: S) -> Result<Self, BlockTranslatorError> {
        Self::new_internal(
            storage,
            iter::from_fn(|| Some(BufferLocation::Owned(Box::new([0; BUF_SIZE])))),
        )
    }
}

impl<'a, const VBS: BlockSize, const BUF_SIZE: usize, const BUFS: usize, S>
    BlockTranslator<'a, VBS, BUF_SIZE, BUFS, S>
where
    S: BlockWrite,
{
    /// Create a new BlockTranslator.
    ///
    /// Example:
    /// Create a BlockTranslator with 1 buffer of 65536 bytes and 512 bytes of virtual block size.
    /// ```no_compile
    /// let mut buffer = [0u8; 65536];
    /// let mut translated = BlockTranslator::<512, _, _, _>::new_with_buffer(&mut storage, [&mut buffer])?;
    /// ```
    pub fn new_with_buffer(
        storage: S,
        buffer: [&'a mut [u8; BUF_SIZE]; BUFS],
    ) -> Result<Self, BlockTranslatorError> {
        Self::new_internal(storage, buffer.into_iter().map(BufferLocation::Borrowed))
    }

    /// Create a new BlockTranslator.
    fn new_internal<I>(storage: S, mut buffers: I) -> Result<Self, BlockTranslatorError>
    where
        I: Iterator<Item = BufferLocation<'a, BUF_SIZE>>,
    {
        // Compile-time check
        const {
            if BUFS == 0 {
                panic!("number of buffers (BUFS) must be greater than zero");
            }
            // since usize is at least 32-bits long, this is ok
            if BUF_SIZE < VBS as usize {
                panic!("buffer size must be bigger or equal than virtual block size");
            }
            if !VBS.is_power_of_two() {
                panic!("virtual block size must be a power of two");
            }
        }

        let hardware_block_size = storage.block_size();

        if !hardware_block_size.is_power_of_two() {
            return Err(BlockTranslatorError::HardwareBlockSizeNotPowerOfTwo);
        }

        if usize::try_from(hardware_block_size).unwrap() > BUF_SIZE {
            return Err(BlockTranslatorError::BufferSizeTooSmall);
        }

        if hardware_block_size < VBS {
            return Err(BlockTranslatorError::HardwareBlockSizeToSmall);
        }

        Ok(Self {
            storage,
            buffers: array::from_fn::<_, BUFS, _>(|i| Buffer {
                buffer: buffers.next().unwrap(),
                stored_block: 0,
                status: BlockTranslatorStatus::Unknown,
                last_used: i,
            }),
            next: if BUFS >= 3 { BUFS } else { 0 },
            vbs_per_hbs: hardware_block_size / VBS,
        })
    }

    fn go_to_block<'b>(
        &'b mut self,
        block_in_vbs: BlockIndex,
    ) -> Result<(&'b mut Buffer<'a, BUF_SIZE>, usize), S::Error> {
        // assert that all known blocks are distinct
        debug_assert!(!self.buffers.iter().enumerate().any(|(p1, b1)| b1.status
            != BlockTranslatorStatus::Unknown
            && self.buffers.iter().enumerate().any(|(p2, b2)| p1 != p2
                && b2.status != BlockTranslatorStatus::Unknown
                && b1.stored_block == b2.stored_block)));

        let real_block = block_in_vbs / BlockIndex::from(self.vbs_per_hbs);
        #[cfg_attr(feature = "lba64", expect(clippy::cast_possible_truncation))]
        let offset = (block_in_vbs % BlockIndex::from(self.vbs_per_hbs)) as usize;

        let buffer = match BUFS {
            1 => {
                let buffer = &mut self.buffers[0];

                if buffer.stored_block == real_block
                    && buffer.status != BlockTranslatorStatus::Unknown
                {
                    return Ok((buffer, offset));
                }

                buffer
            }
            2 => {
                // check newest buffer
                if self.buffers[self.next].stored_block == real_block
                    && self.buffers[self.next].status != BlockTranslatorStatus::Unknown
                {
                    return Ok((&mut self.buffers[self.next], offset));
                }

                // switch to older buffer
                self.next ^= 1;

                let buffer = &mut self.buffers[self.next];

                // check older buffer
                if buffer.status != BlockTranslatorStatus::Unknown
                    && buffer.stored_block == real_block
                {
                    return Ok((buffer, offset));
                }

                buffer
            }
            _ => {
                if self.next == usize::MAX {
                    // reset all ages because an overflow would happen otherwise
                    let mut ages = [(0, 0); BUFS];
                    for (num, buf) in self.buffers.iter().enumerate() {
                        ages[num] = (num, buf.last_used);
                    }
                    ages.sort_by_key(|&(_, age)| age);
                    for (new_age, (num, _last_used)) in ages.into_iter().enumerate() {
                        self.buffers[num].last_used = new_age;
                    }
                    self.next = BUFS;
                }

                let mut oldest: Option<&'b mut Buffer<'a, BUF_SIZE>> = None;

                // check if the block is already buffered
                for buffer in self.buffers.iter_mut() {
                    if buffer.stored_block == real_block
                        && buffer.status != BlockTranslatorStatus::Unknown
                    {
                        // update last_used, unless it already was the last used
                        if buffer.last_used != self.next - 1 {
                            buffer.last_used = self.next;
                            self.next += 1;
                        }

                        return Ok((buffer, offset));
                    }
                    if oldest.is_none() || buffer.last_used < oldest.as_ref().unwrap().last_used {
                        oldest = Some(buffer);
                    }
                }

                // get oldest buffer
                let buffer = oldest.unwrap();

                buffer.last_used = self.next;
                self.next += 1;

                buffer
            }
        };

        // store block, if required
        if buffer.status == BlockTranslatorStatus::Modified {
            self.storage.write(buffer.stored_block, &*buffer.buffer)?;
        }

        // read block
        buffer.stored_block = real_block;
        self.storage
            .read(buffer.stored_block, &mut *buffer.buffer)?;
        buffer.status = BlockTranslatorStatus::Read;

        Ok((buffer, offset))
    }
}

impl<const VBS: BlockSize, const BUF_SIZE: usize, const BUFS: usize, S> ErrorType
    for BlockTranslator<'_, VBS, BUF_SIZE, BUFS, S>
where
    S: BlockWrite,
{
    type Error = S::Error;
}

impl<const VBS: BlockSize, const BUF_SIZE: usize, const BUFS: usize, S> BlockBase
    for BlockTranslator<'_, VBS, BUF_SIZE, BUFS, S>
where
    S: BlockWrite,
{
    #[inline]
    fn block_size(&self) -> BlockSize {
        VBS
    }

    #[inline]
    fn block_count(&self) -> BlockCount {
        self.storage.block_count() * BlockCount::from(self.vbs_per_hbs)
    }
}

impl<const VBS: BlockSize, const BUF_SIZE: usize, const BUFS: usize, S> BlockRead
    for BlockTranslator<'_, VBS, BUF_SIZE, BUFS, S>
where
    S: BlockWrite,
{
    fn read(
        &mut self,
        mut block_in_vbs: BlockIndex,
        mut buf: &mut [u8],
    ) -> Result<(), Self::Error> {
        while !buf.is_empty() {
            let (this, next) = buf.split_at_mut(VBS.try_into().unwrap());
            let (buffer, offset) = self.go_to_block(block_in_vbs)?;
            this.copy_from_slice(&buffer.buffer[offset..offset + usize::try_from(VBS).unwrap()]);

            // advance
            buf = next;
            block_in_vbs += 1;
        }

        Ok(())
    }
}

impl<const VBS: BlockSize, const BUF_SIZE: usize, const BUFS: usize, S> BlockWrite
    for BlockTranslator<'_, VBS, BUF_SIZE, BUFS, S>
where
    S: BlockWrite,
{
    fn write(&mut self, mut block_in_vbs: BlockIndex, mut buf: &[u8]) -> Result<(), Self::Error> {
        while !buf.is_empty() {
            let (this, next) = buf.split_at(VBS.try_into().unwrap());
            let (buffer, offset) = self.go_to_block(block_in_vbs)?;
            buffer.buffer[offset..offset + usize::try_from(VBS).unwrap()].copy_from_slice(this);
            buffer.status = BlockTranslatorStatus::Modified;

            // advance
            buf = next;
            block_in_vbs += 1;
        }

        Ok(())
    }

    fn flush(&mut self) -> Result<(), Self::Error> {
        for buffer in self.buffers.iter_mut() {
            if buffer.status == BlockTranslatorStatus::Modified {
                self.storage.write(buffer.stored_block, &*buffer.buffer)?;
                buffer.status = BlockTranslatorStatus::Read;
            }
        }

        self.storage.flush()
    }
}

impl<const VBS: BlockSize, const BUF_SIZE: usize, const BUFS: usize, S> Drop
    for BlockTranslator<'_, VBS, BUF_SIZE, BUFS, S>
where
    S: BlockWrite,
{
    fn drop(&mut self) {
        let _ = self.flush();
    }
}

impl<const BUF_SIZE: usize> Deref for BufferLocation<'_, BUF_SIZE> {
    type Target = [u8; BUF_SIZE];

    fn deref(&self) -> &Self::Target {
        match self {
            BufferLocation::Borrowed(e) => e,
            BufferLocation::Owned(b) => b.deref(),
        }
    }
}

impl<const BUF_SIZE: usize> DerefMut for BufferLocation<'_, BUF_SIZE> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        match self {
            BufferLocation::Borrowed(e) => e,
            BufferLocation::Owned(b) => b.deref_mut(),
        }
    }
}
