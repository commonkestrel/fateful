use std::ffi::{c_char, c_int};
use byte_strings::c_str;


static mut STATE: u8 = 0x00;

// Will be called on library load
#[no_mangle]
pub extern "C" fn init() -> c_int {
    println!("Rust init");
    return 0;
}

// Will be called whenever the attached port is read from.
#[no_mangle]
pub unsafe extern "C" fn read() -> u8 {
    println!("Rust read");
    STATE
}

// Will be called whenever the attached port is written to.
#[no_mangle]
pub unsafe extern "C" fn write(data: u8) {
    println!("Rust example write");
    STATE = data;
}

// Will be called whenever the attached port is written to.
#[no_mangle]
pub extern "C" fn name() -> *const c_char {
    c_str!("Rust Example").as_ptr()
}
