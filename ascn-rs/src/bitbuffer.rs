#[derive(Clone)]
pub struct BitBuffer {
    data: Vec<bool>,
}

impl BitBuffer {
    pub fn new() -> Self {
        Self { data: Vec::new() }
    }

    pub fn add(&mut self, data: u8, length: u8) {
        assert!(length <= 8);

        for i in 0..length {
            self.add_bit(((data >> i) & 1) == 1);
        }
    }

    pub fn add_bit(&mut self, data: bool) {
        self.data.insert(0, data);
    }

    pub fn to_bytes(&self) -> Vec<u8> {
        let mut byte_pos = 0;
        let mut current_byte = 0u8;
        let mut result = Vec::new();

        for bit in self.data.iter().rev() {
            current_byte |= (if *bit { 1 } else { 0 }) << byte_pos;
            byte_pos += 1;

            if byte_pos == 8 {
                result.push(current_byte);
                current_byte = 0u8;
                byte_pos = 0;
            }
        }

        if byte_pos != 0 {
            result.push(current_byte);
        }

        result.reverse();

        result
    }

    pub fn from_bytes(data: &[u8]) -> Self {
        let mut bit_buffer = Self::new();

        for byte in data.iter().rev() {
            for i in 0..8 {
                bit_buffer.add_bit(((byte >> i) & 1) == 1);
            }
        }

        bit_buffer
    }

    pub fn read_bit(&mut self) -> bool {
        self.data.pop().expect("Tried to read an empty bit buffer")
    }

    pub fn read(&mut self, length: u8) -> u8 {
        assert!(length <= 8);

        let mut result = 0u8;

        for i in 0..length {
            let bit = self.read_bit();
            result |= (if bit { 1 } else { 0 }) << i;
        }

        result
    }
}

#[cfg(test)]
mod tests {
    use super::BitBuffer;

    #[test]
    fn simple_read_write() {
        let mut bit_buffer = BitBuffer::new();

        let test_val: u8 = 0b110;

        bit_buffer.add(test_val, 3);
        assert_eq!(bit_buffer.read(3), test_val)
    }

    #[test]
    fn multiple_read_write() {
        let mut bit_buffer = BitBuffer::new();

        bit_buffer.add(0b110, 3);
        bit_buffer.add(0b1101, 4);
        bit_buffer.add(0b11, 2);
        bit_buffer.add(0b11001, 5);

        assert_eq!(bit_buffer.read(3), 0b110);
        assert_eq!(bit_buffer.read(4), 0b1101);
        assert_eq!(bit_buffer.read(2), 0b11);
        assert_eq!(bit_buffer.read(5), 0b11001);
    }

    #[test]
    fn convert_to_bytes() {
        let mut bit_buffer = BitBuffer::new();

        bit_buffer.add(0b110, 3);
        bit_buffer.add(0b1101, 4);
        bit_buffer.add(0b11, 2);
        bit_buffer.add(0b11001, 5);

        let mut bit_buffer = BitBuffer::from_bytes(&bit_buffer.to_bytes());

        assert_eq!(bit_buffer.read(3), 0b110);
        assert_eq!(bit_buffer.read(4), 0b1101);
        assert_eq!(bit_buffer.read(2), 0b11);
        assert_eq!(bit_buffer.read(5), 0b11001);
    }

    #[test]
    fn exam_test() {
        // Triggers the last if statement
        let mut bit_buffer = BitBuffer::new();

        bit_buffer.add(0b110, 3);
        bit_buffer.add(0b1101, 4);
        bit_buffer.add(0b11, 2);

        dbg!(&bit_buffer.clone().to_bytes());

        let mut bit_buffer = BitBuffer::from_bytes(&bit_buffer.to_bytes());

        assert_eq!(bit_buffer.read(3), 0b110);
        assert_eq!(bit_buffer.read(4), 0b1101);
        assert_eq!(bit_buffer.read(2), 0b11);

        // Does not trigger the last if statement

        let mut bit_buffer = BitBuffer::new();

        bit_buffer.add(0b110, 3);
        bit_buffer.add(0b1101, 4);
        bit_buffer.add(0b1, 1);

        dbg!(&bit_buffer.clone().to_bytes());

        let mut bit_buffer = BitBuffer::from_bytes(&bit_buffer.to_bytes());

        assert_eq!(bit_buffer.read(3), 0b110);
        assert_eq!(bit_buffer.read(4), 0b1101);
        assert_eq!(bit_buffer.read(1), 0b1);
    }
}
