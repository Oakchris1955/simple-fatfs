#[cfg(not(feature = "std"))]
use alloc::{boxed::Box, vec};

use crate::ClusterIndex;

use displaydoc::Display;

/// Inumber data type
///
/// Since every file & directory can be described by either its data or directory
/// cluster, it only makes sense to use that data type.
pub(crate) type Inumber = ClusterIndex;

/// An error type raised by the open inumber table when attempting to register a
/// new inumber.
#[non_exhaustive]
#[derive(Display, Debug, PartialEq, Eq, Clone, Copy)]
pub enum InumberRegisterError {
    /**
     The open inumber table was found to be full while attempting to register
     another inumber.

     Consider using a higher table size with [`set_open_inumber_table_size`]
     or [`with_open_inumber_table_size`] or by opening less files/directories.

     [`set_open_inumber_table_size`]: crate::options::FSOptions::set_open_inumber_table_size
     [`with_open_inumber_table_size`]: crate::options::FSOptions::with_open_inumber_table_size
    */
    TableFull,
    /**
     An inumber was found to have already been registered when attempting to push
     it to the table.
    */
    InumberAlreadyRegistered,
}

#[derive(Debug)]
pub struct OpenInumberTable(Box<[Option<Inumber>]>);

impl OpenInumberTable {
    pub(crate) fn new(len: usize) -> Self {
        Self(vec![Default::default(); len].into_boxed_slice())
    }

    pub(crate) fn is_full(&self) -> bool {
        // the table's entries will be sorted, so this should be None if not full
        self.0.first().unwrap().is_some()
    }

    #[allow(dead_code)]
    pub(crate) fn check(&self, inumber: Inumber) -> bool {
        self.0.binary_search(&Some(inumber)).is_ok()
    }

    /// Register an inumber at the table
    ///
    /// # Errors
    ///
    /// This method will return [`InumberRegisterError::TableFull`] if the table is full.
    ///
    /// If the provided inumber has already been registered,
    /// [`InumberRegisterError::InumberAlreadyRegistered`] will be returned.
    pub(crate) fn register(&mut self, inumber: Inumber) -> Result<(), InumberRegisterError> {
        if self.is_full() {
            return Err(InumberRegisterError::TableFull);
        }

        let insert_index = match self.0.binary_search(&Some(inumber)) {
            Ok(_) => return Err(InumberRegisterError::InumberAlreadyRegistered),
            Err(insert_index) => insert_index,
        };

        self.0[..insert_index].rotate_left(1);
        self.0[insert_index - 1] = Some(inumber);

        Ok(())
    }

    /// Remove a registered structure from the table
    ///
    /// # Errors
    ///
    /// Returns an error if a structure with the provided inumber hadn't
    /// been registered.
    pub(crate) fn unregister(&mut self, inumber: Inumber) -> Result<(), ()> {
        let index = match self.0.binary_search(&Some(inumber)) {
            Ok(structure_index) => structure_index,
            Err(_) => return Err(()),
        };

        self.0[index] = None;
        self.0[..index + 1].rotate_right(1);

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use alloc::vec::Vec;

    use crate::log::local_log;

    use rstest::*;
    use rstest_reuse::*;
    use test_log::test;

    use rand::{random, random_range, seq::SliceRandom};

    /// Allocates a new table with `size`.
    fn table(size: usize) -> OpenInumberTable {
        local_log::trace!("Allocating new table with {} elements", size);

        OpenInumberTable::new(size)
    }

    /// Allocates a new table with `size` & registers `random_range(size / 2..(size - 1))`
    /// distict random elements.
    fn table_w_elements(size: usize) -> OpenInumberTable {
        let mut table = table(size);

        let registered_elements_count = random_range(size / 2..(size - 1));

        local_log::trace!("Registered elements count: {}", registered_elements_count);

        // we use this instead of `rand::random` to prevent any duplicate values
        let mut numbers: Vec<Inumber> = (0..Inumber::try_from(registered_elements_count).unwrap())
            .into_iter()
            .collect();
        numbers.shuffle(&mut rand::rng());
        numbers.truncate(registered_elements_count);
        let usable_numbers = numbers.into_boxed_slice();

        for n in usable_numbers {
            table.register(n).unwrap();
        }

        table
    }

    #[template]
    #[rstest]
    #[case(table_w_elements(random_range(16..=128)))]
    #[case(table_w_elements(random_range(256..=1024)))]
    #[case(table_w_elements(random_range(1024..=4096)))]
    #[case(table_w_elements(random_range(16..=4096)))]
    pub fn table_w_elements_rand_size(#[case] table: OpenInumberTable) {}

    // test cases
    #[test]
    fn check_table_full() {
        const SIZE: usize = 42;

        let mut table = OpenInumberTable::new(SIZE);

        for _ in 0..SIZE {
            // the `random` function samples uniformily, so this shouldn't panic with
            // `OpenStructuresRegisterError::EntryAlreadyRegistered`
            table.register(random()).expect("the table isn't full yet");
        }

        assert_eq!(
            table.register(random()),
            Err(InumberRegisterError::TableFull)
        );
    }

    #[test]
    #[apply(table_w_elements_rand_size)]
    fn unregister_and_check(mut table: OpenInumberTable) {
        let random_index = random();

        table.register(random_index).unwrap();
        assert!(table.check(random_index));

        table.unregister(random_index).unwrap();
        assert!(!table.check(random_index));
    }

    #[test]
    #[apply(table_w_elements_rand_size)]
    fn check_already_registered(mut table: OpenInumberTable) {
        // Normally, this would be bad. However, our sampling space has `Inumber::MAX`
        // elements & the table should at max have less than 5k elements, so very rarely
        // will this even loop.
        let random_index: Inumber = loop {
            let rand_num = random();
            if !table.check(rand_num) {
                break rand_num;
            }
        };

        table.register(random_index).unwrap();
        assert!(table.check(random_index));

        assert_eq!(
            table.register(random_index),
            Err(InumberRegisterError::InumberAlreadyRegistered)
        )
    }
}
