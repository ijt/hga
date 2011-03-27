{-# LANGUAGE TypeSynonymInstances#-}

type Flo2 = Float -> Float

instance Show Flo2 where
    show f = "<function R -> R>"

instance Eq Flo2 where
    f == g = undefined

-- This lets us add, subtract and multiply numerical functions.
instance Num Flo2 where
    f + g = \x -> f x + g x
    f - g = \x -> f x - g x
    f * g = \x -> f x * g x

    abs f = abs . f 

    fromInteger i = \x -> fromIntegral i

    signum f = undefined

-- This lets us divide numerical functions.
instance Fractional Flo2 where
    f / g = \x -> f x / g x
    fromRational r = \x -> fromRational r

