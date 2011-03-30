{-# LANGUAGE TypeSynonymInstances #-}

-- Floating-point valued functions that take a single float as input.
type Flo2 = Float -> Float

-- HACK.
-- Show all Flo2s the same way to satisfy the Show requirement on the Num
-- typeclass.
--instance Show Flo2 where
    --show f = "<function R -> R>"

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

