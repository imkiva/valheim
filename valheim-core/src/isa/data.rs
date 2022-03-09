/// Bounded integer within the range `[0, MAX)`.
pub struct Fin<const MAX: u32>(pub u32);

impl<const MAX: u32> Fin<MAX> {
  pub fn new(value: u32) -> Self {
    debug_assert!(value < MAX);
    Self(value)
  }
}

pub(crate) mod workaround {
  pub struct If<const B: bool>;
  pub trait True {}
  impl True for If<true> {}
}
