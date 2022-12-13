/// Bounded integer within the range `[0, MAX)`.
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub struct Fin<const MAX: u32>(u32);

impl<const MAX: u32> Fin<MAX> {
  pub const fn new(value: u32) -> Self {
    assert!(value < MAX);
    Self(value)
  }

  #[inline(always)]
  pub fn value(self) -> u32 {
    self.0
  }
}

pub(crate) mod workaround {
  pub struct If<const B: bool>;
  pub trait True {}
  impl True for If<true> {}
}
