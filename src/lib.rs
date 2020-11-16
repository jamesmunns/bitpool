use core::{
    alloc::Layout,
    sync::atomic::{
        AtomicU8,
        AtomicU32,
        AtomicUsize,
        Ordering,
    },
    cell::UnsafeCell,
    mem::MaybeUninit,
    ptr::NonNull,
};

const POOL_SIZE_BYTES: usize = 64 * 1024;

#[derive(Debug)]
pub struct PageMap {
    pages: [AtomicU32; 64],
    hint_1024: AtomicUsize,
    hint_512: AtomicUsize,
    hint_256: AtomicUsize,
    hint_128: AtomicUsize,
    hint_64: AtomicUsize,
}

// 0b01000000_00000000_00000000_00000000 - 1024
// 0b00110000_00000000_00000000_00000000 -  512
// 0b00001111_00000000_00000000_00000000 -  256
// 0b00000000_11111111_00000000_00000000 -  128
// 0b00000000_00000000_11111111_11111111 -   64

const OFFSET_LUT: [usize; 32] = [
    0xFFFF_FFFF,    // Should never be true!   -  0
    0,              // Full 1024 byte block    -  1
    512 * 0,        // 512_0 block             -  2
    512 * 1,        // 512_1 block             -  3
    256 * 0,        // 256_0 block             -  4
    256 * 1,        // 256_1 block             -  5
    256 * 2,        // 256_2 block             -  6
    256 * 3,        // 256_3 block             -  7
    128 * 0,        // 128_0 block             -  8
    128 * 1,        // 128_1 block             -  9
    128 * 2,        // 128_2 block             - 10
    128 * 3,        // 128_3 block             - 11
    128 * 4,        // 128_4 block             - 12
    128 * 5,        // 128_5 block             - 13
    128 * 6,        // 128_6 block             - 14
    128 * 7,        // 128_7 block             - 15
    64 * 0,         // 64_00 block             - 16
    64 * 1,         // 64_01 block             - 17
    64 * 2,         // 64_02 block             - 18
    64 * 3,         // 64_03 block             - 19
    64 * 4,         // 64_04 block             - 20
    64 * 5,         // 64_05 block             - 21
    64 * 6,         // 64_06 block             - 22
    64 * 7,         // 64_07 block             - 23
    64 * 8,         // 64_08 block             - 24
    64 * 9,         // 64_09 block             - 25
    64 * 10,        // 64_10 block             - 26
    64 * 11,        // 64_11 block             - 27
    64 * 12,        // 64_12 block             - 28
    64 * 13,        // 64_13 block             - 29
    64 * 14,        // 64_14 block             - 30
    64 * 15,        // 64_15 block             - 31
];

const LZ_CLAIM_LUT: [u32; 32] = [
    0xFFFF_FFFF, // Should never be true!   -  0
    0x7FFF_FFFF, // Full 1024 byte block    -  1
    0x6CF0_FF00, // 512_0 block             -  2
    0x530F_00FF, // 512_1 block             -  3
    0x68C0_F000, // 256_0 block             -  4
    0x6430_0F00, // 256_1 block             -  5
    0x520C_00F0, // 256_2 block             -  6
    0x5103_000F, // 256_3 block             -  7
    0x6880_C000, // 128_0 block             -  8
    0x6840_3000, // 128_1 block             -  9
    0x6420_0C00, // 128_2 block             - 10
    0x6410_0300, // 128_3 block             - 11
    0x5208_00C0, // 128_4 block             - 12
    0x5204_0030, // 128_5 block             - 13
    0x5102_000C, // 128_6 block             - 14
    0x5101_0003, // 128_7 block             - 15
    0x6880_8000, // 64_00 block             - 16
    0x6880_4000, // 64_01 block             - 17
    0x6440_2000, // 64_02 block             - 18
    0x6440_1000, // 64_03 block             - 19
    0x6220_0800, // 64_04 block             - 20
    0x6220_0400, // 64_05 block             - 21
    0x6110_0200, // 64_06 block             - 22
    0x6110_0100, // 64_07 block             - 23
    0x5808_0080, // 64_08 block             - 24
    0x5808_0040, // 64_09 block             - 25
    0x5404_0020, // 64_10 block             - 26
    0x5404_0010, // 64_11 block             - 27
    0x5202_0008, // 64_12 block             - 28
    0x5202_0004, // 64_13 block             - 29
    0x5101_0002, // 64_14 block             - 30
    0x5101_0001, // 64_15 block             - 31
];

impl PageMap {
    // Looks for a free chunk of data, regardless of alignment.
    //
    // This version starts at the hint offset page and seeks to end,
    // then trys again from the start
    //
    // If successful, it returns a byte offset into the buffer.
    pub fn seek_exact_fast(&self, amt: usize) -> Option<Header> {
        if amt == 0 {
            debug_assert!(false, "ZSTs not supported");
            return None;
        }

        let pow_size = amt.next_power_of_two();
        let (hint, size_mask) = match pow_size {
            1024 => (
                &self.hint_1024,
                0b01000000_00000000_00000000_00000000
            ),
            512 => (
                &self.hint_512,
                0b00110000_00000000_00000000_00000000
            ),
            256 => (
                &self.hint_256,
                0b00001111_00000000_00000000_00000000
            ),
            128 => (
                &self.hint_128,
                0b00000000_11111111_00000000_00000000
            ),
            n if n <= 64 => (
                &self.hint_64,
                0b00000000_00000000_11111111_11111111
            ),
            _ => return None,
        };

        let word_offset = hint.load(Ordering::Acquire);
        assert!(word_offset < self.pages.len());

        let mut retval = None;

        // TODO: Maybe just do this with splitting the slice? But how to keep
        // the index correct with the offset?
        let optimistic = self.pages.iter().enumerate().skip(word_offset);
        let pessimistic = self.pages.iter().enumerate().take(word_offset);

        let search = optimistic.chain(pessimistic);

        for (idx, page) in search {
            let mut bit_offset = 0;

            // In a CAS loop, attempt to claim a bit we need.
            let old_res = page.fetch_update(Ordering::SeqCst, Ordering::SeqCst, |current| {
                // Seek a page at the level we care about
                let matches = (current & size_mask) ^ size_mask;
                if matches == 0 {
                    return None;
                }

                // Find the correct mask for this found bit
                let lz_offset = matches.leading_zeros() as usize;
                let claim_mask = LZ_CLAIM_LUT[lz_offset];

                // Claim match, parent(s), and children
                bit_offset = lz_offset;
                Some(current | claim_mask)
            });

            if old_res.is_ok() {

                // TODO: Attempt shrink?

                hint.fetch_min(idx, Ordering::SeqCst);
                retval = Some(
                    Header {
                        word_offset: idx,
                        alloc_bit: 0x8000_0000 >> bit_offset,
                        page_offset: OFFSET_LUT[bit_offset],
                    }
                );
                break;
            }
        }

        // TODO: Update hint when we DON'T find a block?

        retval
    }
}

#[derive(Debug)]
pub struct BitPool {
    buffer: UnsafeCell<MaybeUninit<[u8; POOL_SIZE_BYTES]>>,
    map: UnsafeCell<MaybeUninit<PageMap>>,
    state: AtomicU8,
}

const BP_STATE_UNINIT: u8 = 0;
const BP_STATE_INITING: u8 = 1;
const BP_STATE_INITED: u8 = 2;

pub struct BitAlloc {
    bp: NonNull<BitPool>,
}

// TODO: lots of duplicate information here, optimize down later
pub struct Header {
    word_offset: usize,

    #[allow(dead_code)]
    alloc_bit: u32,
    page_offset: usize,
}

pub struct AllocContents<T: Sized> {
    header: Header,
    inner: MaybeUninit<T>,
}

pub struct RawBitBox<T: Sized> {
    ptr: NonNull<AllocContents<T>>,
}

impl<T: Sized> RawBitBox<T> {
    pub fn as_mut_ptr(&mut self) -> Option<*mut T> {
        unsafe {
            Some(self.ptr.as_mut().inner.as_mut_ptr())
        }
    }

    pub unsafe fn assume_init(self) -> BitBox<T> {
        BitBox { ptr: self.ptr }
    }
}

pub struct BitBox<T: Sized> {
    ptr: NonNull<AllocContents<T>>,
}

impl<T: Sized> BitBox<T> {
    pub fn as_mut(&mut self) -> Option<&mut T> {
        unsafe {
            Some(&mut *self.ptr.as_mut().inner.as_mut_ptr())
        }
    }
}

impl BitAlloc {
    unsafe fn buf_map(&self) -> (*mut u8, &'static PageMap) {
        let inner = self.bp.as_ref();

        let buf = inner.buffer.get().cast::<u8>();
        let map = &*(inner.map.get().cast::<PageMap>() as *const _);

        (buf, map)
    }

    pub fn alloc_raw<T: Sized>(&self) -> Option<RawBitBox<T>> {
        let layout = Layout::new::<AllocContents<T>>();
        assert!(layout.align() <= 64, "alignment greater than 64 bytes not yet supported");

        let (buf, map) = unsafe { self.buf_map() };
        let header = map.seek_exact_fast(layout.size())?;

        let offset = (header.word_offset * 1024) + header.page_offset;

        let new_ptr = unsafe { buf.add(offset) }.cast::<AllocContents<T>>();
        unsafe {
            (*new_ptr).header = header;
        }

        Some(RawBitBox { ptr: NonNull::new(new_ptr)? })
    }
}

impl BitPool {
    pub const fn new() -> Self {
        Self {
            buffer: UnsafeCell::new(MaybeUninit::uninit()),
            map: UnsafeCell::new(MaybeUninit::uninit()),
            state: AtomicU8::new(BP_STATE_UNINIT),
        }
    }

    #[inline(always)]
    unsafe fn new_bitpool_unchecked(&self) -> BitAlloc {
        BitAlloc { bp: NonNull::new_unchecked(self as *const _ as *mut _) }
    }

    pub fn take_allocator(&self) -> Option<BitAlloc> {
        let prev = self.state.load(Ordering::Acquire);

        // Optimistic: Already initted
        if prev == BP_STATE_INITED {
            return Some(unsafe {
                self.new_bitpool_unchecked()
            });
        }

        // Try to claim the role of initiator
        let prev = self.state.compare_and_swap(BP_STATE_UNINIT, BP_STATE_INITING, Ordering::SeqCst);

        // It's busy, nothing to do
        if prev != BP_STATE_UNINIT {
            return None;
        }

        // Explicitly zero everything
        unsafe {
            let buf_mu = self.buffer.get();
            (*buf_mu).as_mut_ptr().write_bytes(0u8, 1);

            let map_mu = self.map.get();
            (*map_mu).as_mut_ptr().write_bytes(0u8, 1);
        }

        self.state.store(BP_STATE_INITED, Ordering::Relaxed);

        Some(unsafe {
            self.new_bitpool_unchecked()
        })
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
}
