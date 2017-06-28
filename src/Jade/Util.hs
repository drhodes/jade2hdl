module Jade.Util where
import Text.Format

class (Show a) => Fmt a where
  fmt :: String -> a -> String

instance (Show a, Show b) => Fmt (a, b) where
  fmt s (x1, x2) = format s [show x1, show x2]

instance (Show a1, Show a2, Show a3) => Fmt (a1, a2, a3) where
  fmt s (x1,x2,x3) = format s [show x1, show x2, show x3]

instance (Show a1, Show a2, Show a3, Show a4) => Fmt (a1, a2, a3, a4) where
  fmt s (x1,x2,x3,x4) = format s [show x1, show x2, show x3, show x4]

instance (Show a1, Show a2, Show a3, Show a4, Show a5) =>
         Fmt (a1, a2, a3, a4, a5) where
  fmt s (x1,x2,x3,x4,x5) = format s [show x1, show x2, show x3, show x4, show x5]


