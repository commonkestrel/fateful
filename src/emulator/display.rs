use std::{
    cell::UnsafeCell,
    marker::PhantomData,
    pin::Pin,
    sync::atomic::{AtomicU16, AtomicU8, Ordering},
};

use async_std::task::JoinHandle;
use minifb::{Scale, ScaleMode, WindowOptions};

const FONT: &[u8; 1 << 12] = include_bytes!("../vga-font.rom");
const WIDTH: usize = 640;
const HEIGHT: usize = 400;

#[derive(Debug)]
pub struct TextBuffer {
    data: Pin<Box<[u8; 1 << 12]>>,
    handle: JoinHandle<()>,
}

struct BufferPtr(*const [u8; 1 << 12]);
unsafe impl Send for BufferPtr {}

impl TextBuffer {
    pub fn spawn() -> TextBuffer {
        let data = Box::pin([0; 1 << 12]);

        let handle = async_std::task::spawn(run_handle(BufferPtr(&*data)));

        TextBuffer { data, handle }
    }

    pub fn get(&self, addr: u16) -> u8 {
        self.data[addr as usize]
    }

    pub fn set(&mut self, addr: u16, data: u8) {
        self.data[addr as usize] = data;
    }
}

async fn run_handle(buffer: BufferPtr) {
    let mut opts = WindowOptions::default();
    opts.scale = Scale::FitScreen;
    opts.scale_mode = ScaleMode::AspectRatioStretch;
    opts.resize = true;
    
    let mut window =
        minifb::Window::new("f8ful", WIDTH, HEIGHT, opts).expect("should be able to create window");
    let mut fb = [0x00000000; WIDTH * HEIGHT];

    while window.is_open() {
        for y in 0..HEIGHT {
            for x in 0..WIDTH {
                let font_x = x % 8;
                let font_y = y % 16;

                let char_x = x / 8;
                let char_y = y / 16;
                let char_idx = char_x + char_y * WIDTH / 8;
                let character = 0x10;
                let character = unsafe { (*buffer.0)[char_idx] };

                let font_addr = ((character as usize) << 4) + font_y;
                let lit = FONT[font_addr] & (1 << (7 - font_x)) > 0;

                // This part isn't part of the actual CPU,
                // the real value will be transmitted via wire instead of stored.
                fb[x + y * WIDTH] = if lit { 0x00FFFFFF } else { 0x00000000 };
            }
        }

        window
            .update_with_buffer(&fb, WIDTH, HEIGHT)
            .expect("unable to write to window");
    }
    
}
