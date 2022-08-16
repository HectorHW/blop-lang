use std::fmt::{Debug, Display};
use std::ops::Deref;
#[derive(Copy, Clone)]
pub struct ShortString<const N: usize> {
    content: [u8; N],
}

impl<const N: usize> ShortString<N> {
    pub fn try_new(content: &str) -> Option<Self> {
        let size = content.len();
        if size >= N {
            return None;
        }

        let mut res: ShortString<N> = Default::default();
        let array = &mut res.content;
        array[0] = size as u8;

        let view = &mut array[1..(1 + size)];
        view.clone_from_slice(content.as_bytes());
        Some(res)
    }

    pub fn as_str(&self) -> &str {
        self
    }

    pub fn try_concat(first: &str, second: &str) -> Option<Self> {
        if first.len() + second.len() >= N {
            return None;
        }

        let mut res: ShortString<N> = Default::default();
        let array = &mut res.content;
        let size = first.len() + second.len();

        array[0] = size as u8;

        array[1..(1 + first.len())].clone_from_slice(first.as_bytes());
        array[(1 + first.len())..(1 + first.len() + second.len())]
            .clone_from_slice(second.as_bytes());

        Some(res)
    }
}

impl<const N: usize> Default for ShortString<N> {
    fn default() -> Self {
        Self { content: [0u8; N] }
    }
}

impl<const N: usize> Deref for ShortString<N> {
    type Target = str;

    fn deref(&self) -> &Self::Target {
        let size = self.content[0] as usize;
        let view = &self.content[1..(1 + size)];
        unsafe { std::str::from_utf8_unchecked(view) }
    }
}

impl<const N: usize> Debug for ShortString<N> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("ShortString")
            .field("content", &self.content)
            .finish()?;

        write!(f, " (\"{}\")", self.as_str())
    }
}

impl<const N: usize> Display for ShortString<N> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.as_str())
    }
}
