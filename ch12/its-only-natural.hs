module OnlyNature where

import Data.Maybe

-- As natural as any
-- competitive bodybuilder 
data Nat =
    Zero
  | Succ Nat
  deriving (Eq, Show)


-- >>> natToInteger Zero
-- 0
-- >>> natToInteger (Succ Zero)
-- 1
-- >>> natToInteger (Succ (Succ Zero)) 
-- 2
natToInteger :: Nat -> Integer 
natToInteger x = 
    case x of
        Zero   -> 0
        Succ y -> natToInteger y + 1 

-- >>> integerToNat 0
-- Just Zero
-- >>> integerToNat 1
-- Just (Succ Zero)
-- >>> integerToNat 2
-- Just (Succ (Succ Zero))
-- >>> integerToNat (-1)
-- Nothing
integerToNat :: Integer -> Maybe Nat 
integerToNat x
        | x == 0   = Just Zero
        | x > 0    = Just (Succ . fromJust $ integerToNat (x-1))
        |otherwise = Nothing
