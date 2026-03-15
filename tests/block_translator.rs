use embedded_io::ErrorType;
use simple_fatfs::block_io::*;

#[derive(Debug)]
struct Storage<'a, const BS: BlockSize>(&'a mut [u8; 64]);

impl<const BS: BlockSize> BlockBase for Storage<'_, BS> {
    fn block_size(&self) -> BlockSize {
        BS
    }

    fn block_count(&self) -> BlockCount {
        (64 / BS).into()
    }
}

impl<const BS: BlockSize> BlockRead for Storage<'_, BS> {
    fn read(&mut self, block: BlockIndex, buf: &mut [u8]) -> Result<(), Self::Error> {
        let offset: usize = (block * BlockIndex::from(BS)).try_into().unwrap();
        buf.copy_from_slice(&self.0[offset..offset + buf.len()]);
        Ok(())
    }
}

impl<const BS: BlockSize> ErrorType for Storage<'_, BS> {
    type Error = embedded_io::SliceWriteError;
}

impl<const BS: BlockSize> BlockWrite for Storage<'_, BS> {
    fn write(&mut self, block: BlockIndex, buf: &[u8]) -> Result<(), Self::Error> {
        let offset: usize = (block * BlockIndex::from(BS)).try_into().unwrap();
        self.0[offset..offset + buf.len()].clone_from_slice(buf);
        Ok(())
    }

    fn flush(&mut self) -> Result<(), Self::Error> {
        Ok(())
    }
}

#[cfg_attr(miri, ignore)]
#[test]
fn test_block_translator1() {
    let mut translated_c_buffer1 = [0u8; 4];

    run_block_translator(Some([&mut translated_c_buffer1]));
}

#[cfg_attr(miri, ignore)]
#[test]
fn test_block_translator2() {
    let mut translated_c_buffer1 = [0u8; 4];
    let mut translated_c_buffer2 = [0u8; 4];

    run_block_translator(Some([&mut translated_c_buffer1, &mut translated_c_buffer2]));
}

#[cfg_attr(miri, ignore)]
#[test]
fn test_block_translator3() {
    let mut translated_c_buffer1 = [0u8; 4];
    let mut translated_c_buffer2 = [0u8; 4];
    let mut translated_c_buffer3 = [0u8; 4];

    run_block_translator(Some([
        &mut translated_c_buffer1,
        &mut translated_c_buffer2,
        &mut translated_c_buffer3,
    ]));
}

#[cfg_attr(miri, ignore)]
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

    run_block_translator(Some([
        &mut translated_c_buffer1,
        &mut translated_c_buffer2,
        &mut translated_c_buffer3,
        &mut translated_c_buffer4,
        &mut translated_c_buffer5,
        &mut translated_c_buffer6,
        &mut translated_c_buffer7,
        &mut translated_c_buffer8,
    ]));
}

#[cfg_attr(miri, ignore)]
#[test]
fn test_block_translator1_heap() {
    run_block_translator::<1>(None);
}

#[cfg_attr(miri, ignore)]
#[test]
fn test_block_translator2_heap() {
    run_block_translator::<2>(None);
}

#[cfg_attr(miri, ignore)]
#[test]
fn test_block_translator3_heap() {
    run_block_translator::<3>(None);
}

#[cfg_attr(miri, ignore)]
#[test]
fn test_block_translator8_heap() {
    run_block_translator::<8>(None);
}

fn run_block_translator<const BUFS: usize>(buffer: Option<[&mut [u8; 4]; BUFS]>) {
    // initialize storage with random data, copy it to the second storage
    let mut array_a: [u8; 64] = rand::random();
    let mut array_b: [u8; 64] = array_a;

    // A = 64 * 1 buffer
    let mut storage_a = Storage::<1>(&mut array_a);
    // B = 16 * 4 buffer
    let mut storage_b = Storage::<4>(&mut array_b);

    // ensure that total number of bytes are equal
    assert_eq!(
        BlockCount::from(storage_a.block_size()) * storage_a.block_count(),
        BlockCount::from(storage_b.block_size()) * storage_b.block_count()
    );

    // C = translated B into 64 * 1
    let mut translated_c = match buffer {
        None => BlockTranslator::<1, _, _, _>::new(&mut storage_b),
        Some(buffer) => BlockTranslator::<1, _, _, _>::new_with_buffer(&mut storage_b, buffer),
    }
    .unwrap();

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
        array_a, array_b,
        "compare of both storages with {BUFS} buffers"
    );
}
