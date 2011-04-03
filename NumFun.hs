{-# LANGUAGE TypeSynonymInstances #-}

-- This module makes it possible to do arithmetic using functions
-- from Floats to Floats.

import Control.Applicative

-- Floating-point valued functions that take a single float as input.
type Flo2 = Float -> Float

-- HACK.
-- Show all Flo2s the same way to satisfy the Show requirement on the Num
-- typeclass.
instance Show Flo2 where
    show f = "<function R -> R>"

-- HACK.
-- Provide a non-useful definition of equality for Flo2s so we can
-- satisfy the Eq requirement for the Num typeclass.
-- Could this be avoided by using the mathematical prelude?
instance Eq Flo2 where
    f == g = undefined

-- This instance declaration lets us add, subtract and multiply numerical
-- functions.
instance Num Flo2 where
    f + g = \x -> f x + g x
    f - g = \x -> f x - g x
    f * g = \x -> f x * g x

    abs f = abs . f 

    fromInteger i = \x -> fromIntegral i

    signum f = undefined

-- This one lets us divide numerical functions.
instance Fractional Flo2 where
    f / g = \x -> f x / g x
    fromRational r = \x -> fromRational r

data Taylor = Taylor [Flo2]

instance Show Taylor where
    show t = "<Taylor expansion>"

-- Given a list of a function's derivatives starting with order 0, and a central
-- point, give back terms of a Taylor expansion.
taylor :: [Flo2] -> Float -> Taylor
taylor fs x0 = 
    Taylor [\x -> (x-x0)^k * dfk x0 / factorialf k | (k, dfk) <- zip [0..] fs]

evalTaylor :: Taylor -> Int -> Float -> Float
evalTaylor (Taylor fs) k x =
    sum $ take k $ fs <*> [x]

factorialf :: Integer -> Float
factorialf k = fromIntegral $ factorial k
 
factorial :: Integer -> Integer
factorial k = product [1..k]

