module Data.CapnProto.Pointer where

import Data.CapnProto.Layout

{-
impl <'a> PointerReader<'a> {
    pub fn new_default<'b>() -> PointerReader<'b> {
        PointerReader {
            marker : ::std::marker::PhantomData::<&'b ()>,
            segment : ::std::ptr::null(),
            pointer : ::std::ptr::null(),
            nesting_limit : 0x7fffffff }
    }

    pub fn get_root<'b>(segment : *const SegmentReader, location : *const Word,
                        nesting_limit : i32) -> Result<PointerReader<'b>> {
        unsafe {
            try!(wire_helpers::bounds_check(segment, location,
                                            location.offset(POINTER_SIZE_IN_WORDS as isize),
                                            WirePointerKind::Struct));

            Ok(PointerReader {
                marker : ::std::marker::PhantomData::<&'b ()>,
                segment : segment,
                pointer : ::std::mem::transmute(location),
                nesting_limit : nesting_limit })
        }
    }

    pub fn get_root_unchecked<'b>(location : *const Word) -> PointerReader<'b> {
        PointerReader {
            marker : ::std::marker::PhantomData::<&'b ()>,
            segment : ::std::ptr::null(),
            pointer : unsafe { ::std::mem::transmute(location) },
            nesting_limit : 0x7fffffff }
    }

    pub fn is_null(&self) -> bool {
        self.pointer.is_null() || unsafe { (*self.pointer).is_null() }
    }

    pub fn get_struct(&self, default_value: *const Word) -> Result<StructReader<'a>> {
        let reff : *const WirePointer = if self.pointer.is_null() { zero_pointer() } else { self.pointer };
        unsafe {
            wire_helpers::read_struct_pointer(self.segment, reff,
                                             default_value, self.nesting_limit)
        }
    }

    pub fn get_list(&self, expected_element_size : ElementSize,
                    default_value : *const Word) -> Result<ListReader<'a>> {
        let reff = if self.pointer.is_null() { zero_pointer() } else { self.pointer };
        unsafe {
            wire_helpers::read_list_pointer(self.segment,
                                           reff,
                                           default_value,
                                           expected_element_size, self.nesting_limit)
        }
    }

    pub fn get_text(&self, default_value : *const Word, default_size : ByteCount32) -> Result<text::Reader<'a>> {
        unsafe {
            wire_helpers::read_text_pointer(self.segment, self.pointer, default_value, default_size)
        }
    }

    pub fn get_data(&self, default_value : *const Word, default_size : ByteCount32) -> Result<data::Reader<'a>> {
        unsafe {
            wire_helpers::read_data_pointer(self.segment, self.pointer, default_value, default_size)
        }
    }

    pub fn get_capability(&self) -> Result<Box<ClientHook+Send>> {
        let reff : *const WirePointer = if self.pointer.is_null() { zero_pointer() } else { self.pointer };
        unsafe {
            wire_helpers::read_capability_pointer(self.segment, reff, self.nesting_limit)
        }
    }

    pub fn total_size(&self) -> Result<MessageSize> {
        unsafe {
            wire_helpers::total_size(self.segment, self.pointer, self.nesting_limit)
        }
    }
}

pub struct PointerBuilder<'a> {
    marker : ::std::marker::PhantomData<&'a ()>,
    segment : *mut SegmentBuilder,
    pointer : *mut WirePointer
}

impl <'a> PointerBuilder<'a> {

    #[inline]
    pub fn get_root(segment : *mut SegmentBuilder, location : *mut Word) -> PointerBuilder<'a> {
        PointerBuilder {
            marker : ::std::marker::PhantomData::<&'a ()>,
            segment : segment, pointer : unsafe { ::std::mem::transmute(location) }}
    }

    pub fn is_null(&self) -> bool {
        unsafe { (*self.pointer).is_null() }
    }

    pub fn get_struct(&self, size : StructSize, default_value : *const Word) -> Result<StructBuilder<'a>> {
        unsafe {
            wire_helpers::get_writable_struct_pointer(
                self.pointer,
                self.segment,
                size,
                default_value)
        }
    }

    pub fn get_list(&self, element_size : ElementSize, default_value : *const Word) -> Result<ListBuilder<'a>> {
        unsafe {
            wire_helpers::get_writable_list_pointer(
                self.pointer, self.segment, element_size, default_value)
        }
    }

    pub fn get_struct_list(&self, element_size : StructSize,
                           default_value : *const Word) -> Result<ListBuilder<'a>> {
        unsafe {
            wire_helpers::get_writable_struct_list_pointer(
                self.pointer, self.segment, element_size, default_value)
        }
    }

    pub fn get_text(&self, default_value : *const Word, default_size : ByteCount32) -> Result<text::Builder<'a>> {
        unsafe {
            wire_helpers::get_writable_text_pointer(
                self.pointer, self.segment, default_value, default_size)
        }
    }

    pub fn get_data(&self, default_value : *const Word, default_size : ByteCount32) -> Result<data::Builder<'a>> {
        unsafe {
            wire_helpers::get_writable_data_pointer(
                self.pointer, self.segment, default_value, default_size)
        }
    }

    pub fn get_capability(&self) -> Result<Box<ClientHook+Send>> {
        unsafe {
            wire_helpers::read_capability_pointer(
                &(*self.segment).reader, self.pointer, ::std::i32::MAX)
        }
    }

    pub fn init_struct(&self, size : StructSize) -> StructBuilder<'a> {
        unsafe {
            wire_helpers::init_struct_pointer(self.pointer, self.segment, size)
        }
    }

    pub fn init_list(&self, element_size : ElementSize, element_count : ElementCount32) -> ListBuilder<'a> {
        unsafe {
            wire_helpers::init_list_pointer(
                self.pointer, self.segment, element_count, element_size)
        }
    }

    pub fn init_struct_list(&self, element_count : ElementCount32, element_size : StructSize)
                            -> ListBuilder<'a> {
        unsafe {
            wire_helpers::init_struct_list_pointer(
                self.pointer, self.segment, element_count, element_size)
        }
    }

    pub fn init_text(&self, size : ByteCount32) -> text::Builder<'a> {
        unsafe {
            wire_helpers::init_text_pointer(self.pointer, self.segment, size).value
        }
    }

    pub fn init_data(&self, size : ByteCount32) -> data::Builder<'a> {
        unsafe {
            wire_helpers::init_data_pointer(self.pointer, self.segment, size).value
        }
    }

    pub fn set_struct(&self, value : &StructReader) -> Result<()> {
        unsafe {
            try!(wire_helpers::set_struct_pointer(self.segment, self.pointer, *value));
            Ok(())
        }
    }

    pub fn set_list(&self, value : &ListReader) -> Result<()> {
        unsafe {
            try!(wire_helpers::set_list_pointer(self.segment, self.pointer, *value));
            Ok(())
        }
    }

    pub fn set_text(&self, value : &str) {
        unsafe {
            wire_helpers::set_text_pointer(self.pointer, self.segment, value);
        }
    }

    pub fn set_data(&self, value : &[u8]) {
        unsafe {
            wire_helpers::set_data_pointer(self.pointer, self.segment, value);
        }
    }

    pub fn set_capability(&self, cap : Box<ClientHook+Send>) {
        unsafe {
            wire_helpers::set_capability_pointer(self.segment, self.pointer, cap);
        }
    }

    pub fn copy_from(&self, other: PointerReader) -> Result<()> {
        if other.pointer.is_null()  {
            if !self.pointer.is_null() {
                unsafe {
                    wire_helpers::zero_object(self.segment, self.pointer);
                    *self.pointer = ::std::mem::zeroed();
                }
            }
        } else {
            unsafe {
                try!(wire_helpers::copy_pointer(self.segment, self.pointer, other.segment, other.pointer,
                                                other.nesting_limit));
            }
        }
        Ok(())
    }

    pub fn clear(&self) {
        unsafe {
            wire_helpers::zero_object(self.segment, self.pointer);
            ::std::ptr::write_bytes(self.pointer, 0, 1);
        }
    }

    pub fn as_reader(&self) -> PointerReader<'a> {
        unsafe {
            let segment_reader = &(*self.segment).reader;
            PointerReader {
                marker : ::std::marker::PhantomData::<&'a ()>,
                segment : segment_reader,
                pointer : self.pointer,
                nesting_limit : 0x7fffffff }
        }
    }
}

-}
