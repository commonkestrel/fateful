use std::{cell::UnsafeCell, marker::PhantomData, pin::Pin, sync::atomic::{AtomicU16, AtomicU8, Ordering}};

use async_std::task::JoinHandle;

const FONT: &[u8; 1 << 12] = include_bytes!("../vga-font.rom");

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

        TextBuffer {data, handle}
    }
}

async fn run_handle(buffer: BufferPtr) {

}
