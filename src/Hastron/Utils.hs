module Hastron.Utils where

nextEnum :: (Enum a, Bounded a) => a -> a
nextEnum = turnEnum 1

prevEnum :: (Enum a, Bounded a) => a -> a
prevEnum = turnEnum (-1)

turnEnum :: (Enum a, Bounded a) => Int -> a -> a
turnEnum n e = toEnum $ mod (sum [fromEnum e, n]) enumLength
  where enumLength = succ (fromEnum (maxBound `asTypeOf` e))
