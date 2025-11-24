use core::array;
use embedded_io::ErrorType;
use simple_fatfs::block_io::{BlockBase, BlockIndex, BlockRead, BlockTranslator, BlockWrite};

#[derive(Debug)]
struct Storage<'a, const BS: usize>(&'a mut [u8; 64]);

impl<const BS: usize> BlockBase for Storage<'_, BS> {
    fn block_size(&self) -> usize {
        BS
    }

    fn block_count(&self) -> usize {
        64 / BS
    }
}

impl<const BS: usize> BlockRead for Storage<'_, BS> {
    fn read(&mut self, block: BlockIndex, buf: &mut [u8]) -> Result<(), Self::Error> {
        let offset = block as usize * BS;
        buf.copy_from_slice(&self.0[offset..offset + buf.len()]);
        Ok(())
    }
}

impl<const BS: usize> ErrorType for Storage<'_, BS> {
    type Error = embedded_io::SliceWriteError;
}

impl<const BS: usize> BlockWrite for Storage<'_, BS> {
    fn write(&mut self, block: BlockIndex, buf: &[u8]) -> Result<(), Self::Error> {
        let offset = block as usize * BS;
        self.0[offset..offset + buf.len()].clone_from_slice(&buf);
        Ok(())
    }

    fn flush(&mut self) -> Result<(), Self::Error> {
        Ok(())
    }
}

#[test]
fn test_block_translator1() {
    let mut translated_c_buffer1 = [0u8; 4];

    run_block_translator([&mut translated_c_buffer1]);
}

#[test]
fn test_block_translator2() {
    let mut translated_c_buffer1 = [0u8; 4];
    let mut translated_c_buffer2 = [0u8; 4];

    run_block_translator([&mut translated_c_buffer1, &mut translated_c_buffer2]);
}

#[test]
fn test_block_translator3() {
    let mut translated_c_buffer1 = [0u8; 4];
    let mut translated_c_buffer2 = [0u8; 4];
    let mut translated_c_buffer3 = [0u8; 4];

    run_block_translator([
        &mut translated_c_buffer1,
        &mut translated_c_buffer2,
        &mut translated_c_buffer3,
    ]);
}

#[test]
fn test_block_translator8() {
    let mut translated_c_buffer1 = [0u8; 4];
    let mut translated_c_buffer2 = [0u8; 4];
    let mut translated_c_buffer3 = [0u8; 4];
    let mut translated_c_buffer4 = [0u8; 4];
    let mut translated_c_buffer5 = [0u8; 4];
    let mut translated_c_buffer6 = [0u8; 4];
    let mut translated_c_buffer7 = [0u8; 4];
    let mut translated_c_buffer8 = [0u8; 4];

    run_block_translator([
        &mut translated_c_buffer1,
        &mut translated_c_buffer2,
        &mut translated_c_buffer3,
        &mut translated_c_buffer4,
        &mut translated_c_buffer5,
        &mut translated_c_buffer6,
        &mut translated_c_buffer7,
        &mut translated_c_buffer8,
    ]);
}

fn run_block_translator<const BUFS: usize>(buffer: [&mut [u8; 4]; BUFS]) {
    // initialize storage with random data, copy it to the second storage
    let mut storage_a = array::from_fn(|_| rand::random());
    let mut storage_b = storage_a.clone();

    // A = 64 * 1 buffer
    let mut storage_a = Storage::<1>(&mut storage_a);
    // B = 16 * 4 buffer
    let mut storage_b = Storage::<4>(&mut storage_b);

    // ensure that total number of bytes are equal
    assert_eq!(
        storage_a.block_size() * storage_a.block_count(),
        storage_b.block_size() * storage_b.block_count()
    );

    // C = translated B into 64 * 1
    let mut translated_c = BlockTranslator::<1, _, _, _>::new(&mut storage_b, buffer).unwrap();

    // ensure that total number of bytes are equal
    assert_eq!(
        storage_a.block_size() * storage_a.block_count(),
        translated_c.block_size() * translated_c.block_count()
    );

    // ensure that block size and count are equal
    assert_eq!(
        (storage_a.block_size(), storage_a.block_count()),
        (translated_c.block_size(), translated_c.block_count())
    );

    // randomly read/write a byte from/into both storages and expect them to be identical
    for _ in 0..100_000 {
        let offset = rand::random_range(0..64);
        if rand::random::<bool>() {
            let mut buf_a = [0u8; 1];
            let mut buf_b = [0u8; 1];
            storage_a.read(offset, &mut buf_a).unwrap();
            translated_c.read(offset, &mut buf_b).unwrap();
            assert_eq!(buf_a, buf_b, "random read with {BUFS} buffers");
        } else {
            let value = [rand::random()];
            storage_a.write(offset, &value).unwrap();
            translated_c.write(offset, &value).unwrap();
        }
    }

    // flush both storages
    storage_a.flush().unwrap();
    translated_c.flush().unwrap();

    // drop the translation level
    drop(translated_c);

    // assure that the underlying storage of both is identical
    assert_eq!(
        storage_a.0, storage_b.0,
        "compare of both storages with {BUFS} buffers"
    );
}
