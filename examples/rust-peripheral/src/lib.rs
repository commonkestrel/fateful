use fateful_peripheral::{ Peripheral, peripheral };
use anyhow::Result;

#[peripheral(name = b"Rust Example")]
struct State {
    data: u8,
}

impl Peripheral for State {
    fn init(ports: u8) -> Result<Self> {
        Ok(State { data: 0 })
    }

    fn read(&mut self, port: u8) -> u8 {
        self.data
    }

    fn write(&mut self, port: u8, data: u8) {
        self.data = data;
    }

    fn reset(&mut self) {
        self.data = 0;
    }
}
