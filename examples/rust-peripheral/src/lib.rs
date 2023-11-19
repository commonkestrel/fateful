use byte_strings::c_str;

static mut STATE: u8 = 0x00;

// Will be called on library load
#[no_mangle]
pub extern "C" fn init() {
    println!("Rust init");
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
pub extern "C" fn name() -> *const i8 {
    c_str!("Rust Example").as_ptr()
}
