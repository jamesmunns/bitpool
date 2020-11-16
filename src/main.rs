use bitpool::BitPool;

const BITPOOL: BitPool = BitPool::new();



fn main() {
    let alloc = BITPOOL.take_allocator().unwrap();

    let mut ctr = 0;

    let mut sz = 2;

    while let Some(mut raw) = alloc.alloc_raw::<[u8; 999]>() {
        ctr += 1;
        let mut not_raw = unsafe {
            raw.as_mut_ptr().unwrap().write_bytes(0, 1);
            raw.assume_init()
        };

        // println!("{:?}", not_raw.as_mut().unwrap());
        println!("{}", ctr);
    }

    println!("got {} allocs", ctr);
}
