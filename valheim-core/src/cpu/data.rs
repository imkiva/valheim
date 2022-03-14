use std::fmt::Debug;

#[derive(Debug, Clone)]
pub enum Either<A: Debug + Clone, B: Debug + Clone> {
  Left(A),
  Right(B),
}
