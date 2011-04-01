module BladeSum where

import Data.List

data NamedThing a = WithName { name :: String, contents :: a } deriving Show

-- Multivector
data Mv = BladeSum { mvTerms :: [Blade] }

instance Eq Mv where
    a == b = 
        mvTerms (mvNormalForm a) == mvTerms (mvNormalForm b)

instance Show Mv where
    show (BladeSum []) = "0"
    show a = stringJoin " + " $ map show $ mvTerms a

instance Fractional Mv where
    fromRational r = (fromRational r) `e` []

-- Scaled basis blade: the pseudoscalar for the space it spans.
-- These should always be kept in normal form.
data Blade = Blade {bScale :: Float, bIndices :: [Int]} deriving (Ord, Eq)

instance Show Blade where
    show (Blade s []) = show s
    show b = (show $ bScale b) ++ "e" ++ (show $ bIndices b)

-- Constructs a multivector from a scaled blade.
e :: Float -> [Int] -> Mv
s `e` indices = mvNormalForm $ BladeSum [Blade s indices]

instance Num Mv where
    a + b = mvNormalForm $ BladeSum (mvTerms a ++ mvTerms b)
    a - b = mvNormalForm $ BladeSum (mvTerms a ++ (map bladeNeg $ mvTerms b))

    -- Geometric (Clifford) product
    a * b = mvNormalForm $ BladeSum [bladeMul x y | x <- mvTerms a, y <- mvTerms b]

    fromInteger i = (fromIntegral i) `e` []

stringJoin :: String -> [String] -> String
stringJoin sep parts =
    concat $ intersperse sep parts

bladeNeg :: Blade -> Blade
bladeNeg b = Blade (- bScale b) (bIndices b)

bladeMul x y =
    bladeNormalForm $ Blade (bScale x * bScale y) (bIndices x ++ bIndices y)

bladeNonZero :: Blade -> Bool
bladeNonZero b = bScale b /= 0

mvNormalForm mv =
    BladeSum $ filter bladeNonZero $ combineLikeTerms $ sortByIndices $ map bladeNormalForm $ mvTerms mv

sortByIndices :: [Blade] -> [Blade]
sortByIndices bs = sortBy (\x y -> if bIndices x < bIndices y then LT else GT) bs

combineLikeTerms :: [Blade] -> [Blade]
combineLikeTerms [] = []
combineLikeTerms [x] = [x]
combineLikeTerms (x:y:rest) | (bIndices x == bIndices y) =
                                Blade (bScale x + bScale y) (bIndices x) : combineLikeTerms rest
                            | otherwise = x : combineLikeTerms (y:rest)

bladeNormalForm (Blade scale indices) =
    Blade scale' normalizedIndices
    where
        -- Sort the indices, take the sign from the permutation.
        (sortedIndices, sign) = signedSort indices 1
        scale' = (fromIntegral sign) * scale

        -- Remove even runs of the same index.
        normalizedIndices = removeDupPairs sortedIndices

-- Removes adjacent equal objects.
removeDupPairs :: Eq a => [a] -> [a]
removeDupPairs [] = []
removeDupPairs [x] = [x]
removeDupPairs (x:y:rest) | x == y = removeDupPairs rest
                          | otherwise = x : removeDupPairs (y:rest)

signedSort :: Ord a => [a] -> Int -> ([a], Int)
signedSort ls sgn =
    findFixedPoint (uncurry signedSortPass) (ls, sgn)

-- Single pass of bubble sort that keeps track of permutation sign.
signedSortPass :: Ord a => [a] -> Int -> ([a], Int)
signedSortPass [] sgn = ([], sgn)
signedSortPass [x] sgn = ([x], sgn)
signedSortPass (x:y:rest) sgn | y < x = let (rest', sgn') = signedSortPass (x:rest) (-1 * sgn)
                                        in (y:rest', sgn')
                              | otherwise = let (rest', sgn') = signedSortPass (y:rest) sgn
                                            in (x:rest', sgn')
 
-- Copied from Ga.hs
findFixedPoint :: Eq a => (a -> a) -> a -> a
findFixedPoint f x =
  if y == x then y else findFixedPoint f y
  where y = f x

-- The grade extraction operator
getGrade :: Int -> Mv -> Mv
getGrade k mv =
    BladeSum $ filter (`bIsOfGrade` k) (mvTerms mv)

dot :: Mv -> Mv -> Mv
dot a b =
    mvNormalForm $ BladeSum [x `bDot` y | x <- mvTerms a, y <- mvTerms b]

wedge :: Mv -> Mv -> Mv
wedge a b =
    mvNormalForm $ BladeSum [x `bWedge` y | x <- mvTerms a, y <- mvTerms b]

grade :: Blade -> Int
grade b = length $ bIndices b

bGetGrade :: Int -> Blade -> Blade
bGetGrade k b =
    if b `bIsOfGrade` k then b else Blade 0 []

bDot :: Blade -> Blade -> Blade
bDot x y =
    bladeNormalForm $ bGetGrade k xy
        where
            k = (abs (grade x) - (grade y))
            xy = bladeMul x y

bWedge :: Blade -> Blade -> Blade
bWedge x y =
    bladeNormalForm $ bGetGrade k xy
        where
            k = (abs (grade x) + (grade y))
            xy = bladeMul x y

bIsOfGrade :: Blade -> Int -> Bool
blade `bIsOfGrade` k =
    (length $ bIndices blade) == k

-- TESTS

-- Copied from Ga.hs
assertEqual :: (Eq a, Show a) => a -> a -> String -> IO ()
assertEqual expected actual msg =
    if actual /= expected
        then error $ msg ++ ": " ++ show expected ++ " /= " ++ show actual
        else putStrLn (msg ++ " passed.")

main = do
    -- Show
    assertEqual "0" (show (BladeSum [])) "Show an empty multivector"
    assertEqual "0" (show (0 :: Mv)) "Show 0"
    assertEqual "1.0" (show (1 `e` [])) "Show 1"
    assertEqual "1.5" (show (1.5 :: Mv)) "Show 1.5"
    assertEqual "0" (show (0 `e` [1])) "Show a zero-scaled vector"

    -- Construction
    assertEqual 1 (1`e`[]) "Scalar construction 1 == 1e1"

    -- Geometric product
    assertEqual 6 (2`e`[1] * 3`e`[1]) "Product of colinear vectors"
    assertEqual (-6`e`[1,2]) (2`e`[2] * 3`e`[1]) "Product of orthogonal vectors"
    assertEqual (6 - 6`e`[1,2]) ((2`e`[1] + 2`e`[2]) * 3`e`[1]) "Complex-like result"

    -- Grade extraction
    assertEqual 3 (getGrade 0 3) "Zero grade part of a scalar"
    assertEqual 0 (getGrade 0 (1 `e` [1])) "Zero grade part of a vector"
    assertEqual 0 (getGrade 1 3) "One-grade part of a scalar"
    assertEqual 3 (getGrade 0 (3 `e` [1,1])) "Grade extraction with annihilation"
    assertEqual 0 (getGrade 2 (3 `e` [1,1])) "Grade extraction with annihilation part 2"

    -- Dot product
    assertEqual 1 ((1`e`[1]) `dot` (1`e`[1])) "Dot product e1 . e1"
    assertEqual 12 ((4`e`[1] + 2`e`[2]) `dot` (3`e`[1])) "Dot product"

    -- Wedge product
    assertEqual 0 ((1`e`[1]) `wedge` (2`e`[1])) "Wedge of colinear vectors is 0"
    assertEqual (-2`e`[1,2]) ((1`e`[2]) `wedge` (2`e`[1])) "Wedge of orth vectors"
 
