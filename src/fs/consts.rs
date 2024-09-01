/// The minimum size (in bytes) a sector is allowed to have
pub const SECTOR_SIZE_MIN: usize = 512;
/// The maximum size (in bytes) a sector is allowed to have
pub const SECTOR_SIZE_MAX: usize = 4096;

/// Place this in the BPB _jmpboot field to hang if a computer attempts to boot this partition
/// The first two bytes jump to 0 on all bit modes and the third byte is just a NOP
pub(crate) const INFINITE_LOOP: [u8; 3] = [0xEB, 0xFE, 0x90];
