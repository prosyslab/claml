type t =
  | PtrMemD
  | PtrMemI
  | Mul
  | Div
  | Rem
  | Add
  | Sub
  | Shl
  | Shr
  | Cmp
  | LT
  | GT
  | LE
  | GE
  | EQ
  | NE
  | And
  | Xor
  | Or
  | LAnd
  | LOr
  | Assign
  | MulAssign
  | DivAssign
  | RemAssign
  | AddAssign
  | SubAssign
  | ShlAssign
  | ShrAssign
  | AndAssign
  | XorAssign
  | OrAssign
  | Comma
[@@deriving show]
