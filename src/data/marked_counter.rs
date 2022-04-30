use std::mem;

#[derive(Copy, Clone, Debug)]
pub struct MarkedCounter(usize);

const FLAG_MASK: usize = 1 << (mem::size_of::<usize>() - 1);
const MAX_COUNTER: usize = usize::MAX - FLAG_MASK;

pub const UNMARKED_ONE: MarkedCounter = unsafe { MarkedCounter::new_unchecked(1, false) };

impl MarkedCounter {
    #[cfg(test)]
    pub fn new(counter: usize, flag: bool) -> MarkedCounter {
        #[cfg(debug_assertions)]
        if counter & FLAG_MASK > 0 {
            panic!("value {} is too big for MarkedCounter", counter);
        }
        let value = if flag { counter | FLAG_MASK } else { counter };
        MarkedCounter(value)
    }

    pub const unsafe fn new_unchecked(counter: usize, flag: bool) -> MarkedCounter {
        let value = if flag { counter | FLAG_MASK } else { counter };
        MarkedCounter(value)
    }

    pub fn counter(&self) -> usize {
        self.0 & (!FLAG_MASK)
    }

    pub fn flag(&self) -> bool {
        (self.0 & FLAG_MASK) > 0
    }

    pub fn set_counter(&mut self, counter: usize) {
        #[cfg(debug_assertions)]
        if counter & FLAG_MASK > 0 {
            panic!("value {} is too big for MarkedCounter", counter);
        }
        let value = if self.flag() {
            counter | FLAG_MASK
        } else {
            counter
        };
        self.0 = value;
    }

    pub fn set_flag(&mut self, flag: bool) {
        let counter = self.0 & !(FLAG_MASK);
        let value = if flag { counter | FLAG_MASK } else { counter };
        self.0 = value;
    }

    pub fn inc(&mut self) {
        #[cfg(debug_assertions)]
        if self.counter() == MAX_COUNTER {
            //overflow is only checked in debug
            panic!("overflow in MarkedCounter.inc()");
        }
        self.set_counter(self.counter() + 1);
    }

    pub fn dec(&mut self) {
        #[cfg(debug_assertions)]
        if self.counter() == 0 {
            //overflow is only checked in debug
            panic!("underflow in MarkedCounter.dec()");
        }
        self.set_counter(self.counter() - 1);
    }
}

#[cfg(test)]
mod tests {
    use crate::data::marked_counter::MarkedCounter;
    use std::mem;

    #[test]
    fn should_keep_value() {
        let counter = MarkedCounter::new(1, false);
        assert!(!counter.flag());
        assert_eq!(counter.counter(), 1);
    }

    #[test]
    fn should_work_for_big_values() {
        let big_value = usize::MAX - (1 << (mem::size_of::<usize>() - 1));
        let counter = MarkedCounter::new(big_value, false);
        assert!(!counter.flag());
        assert_eq!(counter.counter(), big_value);
    }
}
