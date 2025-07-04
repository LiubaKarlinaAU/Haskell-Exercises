-- Convert color codes, as used on resistors, to a numeric value.
-- https://exercism.org/tracks/haskell/exercises/resistor-color-duo

module ResistorColors (Color(..), value) where
data Color =
    Black
  | Brown
  | Red
  | Orange
  | Yellow
  | Green
  | Blue
  | Violet
  | Grey
  | White
  deriving (Eq, Show, Enum, Bounded)
value :: (Color, Color) -> Int
value (a, b) = toNumber a * 10 + toNumber b where
  toNumber = fromEnum