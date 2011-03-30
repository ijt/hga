{-# LANGUAGE NoMonomorphismRestriction#-}

module Ga where

-- Ga means Geometric Algebra object
data Floating a => Ga a = S a
        | V [a]
        | Wedge (Ga a) (Ga a)
        | Dot (Ga a) (Ga a)
        -- Geometric (Clifford) Product
        -- a b = 1 / 2 (a . b + a ^ b)  for vectors
        | Gp (Ga a) (Ga a)
        | Plus (Ga a) (Ga a)
        | Minus (Ga a) (Ga a)
        | Div (Ga a) (Ga a)
        deriving (Show, Eq)

instance Floating a => Num (Ga a) where
  a + b = a `Plus` b
  a - b = a `Minus` b
  a * b = a `Gp` b

  abs (S x) = S $ abs x
  abs (V v) = S $ sqrt $ sum $ map (^2) v
  abs _ = undefined

  -- TODO(issac): Return signum for pseudoscalars.
  signum (S x) = S $ signum x
  signum x = undefined

  fromInteger i = S $ fromInteger i

instance Fractional a => Fractional (Ga a) where
  a / b = a `Div` b

a /\ b = a `Wedge` b
a $. b = a `Dot` b

simplifyOnce :: Floating a => Ga a -> Ga a
simplifyOnce (S 0 `Plus` x) = so x
simplifyOnce (x `Plus` S 0) = so x
simplifyOnce (S a `Plus` S b) = S $ a + b
simplifyOnce (S a `Minus` S b) = S $ a - b
simplifyOnce (S a `Gp` S b) = S $ a * b
simplifyOnce (S a `Div` S b) = S $ a / b
simplifyOnce (S 1 `Gp` b) = so b
simplifyOnce (a `Gp` S 1) = so a
simplifyOnce (a `Gp` S 0) = S 0
simplifyOnce (S 0 `Gp` b) = S 0
simplifyOnce (S 0 `Wedge` b) = S 0
simplifyOnce (a `Wedge` S 0) = S 0
simplifyOnce (a `Wedge` b) | a == b  = S 0
simplifyOnce (S 0 `Dot` b) = S 0
simplifyOnce (a `Dot` S 0) = S 0
-- Recurse if no patterns matched:
simplifyOnce (a `Wedge` b) = so a /\ so b
simplifyOnce (a `Dot` b) = so a `Dot` so b
simplifyOnce (a `Gp` b) = so a `Gp` so b
simplifyOnce (a `Plus` b) = so a `Plus` so b
simplifyOnce (a `Minus` b) = so a `Minus` so b
simplifyOnce (a `Div` b) = so a `Div` so b
-- Leave everything else alone:
simplifyOnce x = x

so = simplifyOnce

-- Ideally, this function should map all equivalent GA expressions
-- to a single, canonical GA expression.
simplify :: Floating a => Ga a -> Ga a
simplify = findFixedPoint simplifyOnce

-- TODO: Deal with possible oscillations due to floating point issues.
findFixedPoint :: Eq a => (a -> a) -> a -> a
findFixedPoint f x =
  if y == x then y else findFixedPoint f y
  where y = f x

--convertToGps :: Ga -> Ga                     
--convertToGps (S x) = S x                     
--convertToGps (V x) = V x                     
--convertToGps (a `Gp` b) = convertToGps a `Gp` convertToGps b
--convertToGps (V v `Wedge` b) = convertToGps a `Gp` convertToGps b

getKpart :: Floating a => Int -> Ga a -> Ga a
getKpart 0 (S x) = S x
getKpart _ (S x) = S 0
getKpart 1 (V x) = V x
getKpart _ (V x) = S 0
getKpart k (a `Wedge` b) =
    sum [getKpart i a /\ getKpart (k-i) b | i <- [1..k-1]]
    
-- TODO: Write the rest of getKpart.

