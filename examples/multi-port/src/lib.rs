#![allow(private_interfaces)]

use std::ffi::{c_char, c_int};
use byte_strings::c_str;

static mut DATA: u32 = 0;

#[no_mangle]
pub static mut ERROR: c_int = 0;

/// Runs once when loaded.
/// `n` represents how many ports were used to initialize this module.
/// The `n` argument in [`read_port`] and [`write_port`]
/// will never be greater than the value passed in here,
/// meaning that if there is a maximum number of ports, here is the place to error.
#[no_mangle]
pub unsafe extern fn init(n: u8) {
    if n > 4 {
        ERROR = -1;
    }
}

#[no_mangle]
pub unsafe extern fn read(n: u8) -> u8 {
    ((DATA >> 8*n) & 0xFF) as u8
}

#[no_mangle]
pub unsafe extern fn write(n: u8, data: u8) {
    let open = DATA & !(0xFF << 8*n);
    DATA = open | ((data as u32) << 8*n);
}

#[no_mangle]
pub extern fn name() -> *const c_char {
    c_str!("Multi-port Example").as_ptr()
}
