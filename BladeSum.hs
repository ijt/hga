module BladeSum where

import Data.List

-- Multivector
data Mv = BladeSum { mvTerms :: [Blade] } deriving Show

instance Eq Mv where
    a == b = 
        mvTerms (mvNormalForm a) == mvTerms (mvNormalForm b)

-- Scaled basis blade: the pseudoscalar for the space it spans.
-- These are stupid. If you ask for equality, no normalization is done.
-- TODO(ijt): Make a BladeNormalForm type that guarantees goodness.
data Blade = Blade {bScale :: Float, bIndices :: [Int]} deriving (Show, Ord, Eq)

-- Constructs a multivector from a scaled blade.
e :: Float -> [Int] -> Mv
s `e` indices = BladeSum $ [Blade s indices]

instance Num Mv where
    a + b = mvNormalForm $ BladeSum (mvTerms a ++ mvTerms b)
    a - b = mvNormalForm $ BladeSum (mvTerms a ++ (map bladeNeg $ mvTerms b))
    a * b = mvNormalForm $ BladeSum [bladeMul x y | x <- mvTerms a, y <- mvTerms b]

    fromInteger i = (fromIntegral i) `e` []

bladeNeg :: Blade -> Blade
bladeNeg b = Blade (- bScale b) (bIndices b)

bladeMul x y =
    Blade (bScale x * bScale y) (bIndices x ++ bIndices y)

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

getGrade :: Int -> Mv -> Mv
getGrade k mv =
    BladeSum $ filter (`bIsOfGrade` k) (mvTerms mv)

bIsOfGrade :: Blade -> Int -> Bool
blade `bIsOfGrade` k =
    (length $ bIndices $ bladeNormalForm blade) == k

-- TESTS

-- Copied from Ga.hs
assertEqual :: (Eq a, Show a) => a -> a -> String -> IO ()
assertEqual expected actual msg =
    if actual /= expected
        then error $ msg ++ ": " ++ show actual ++ " /= " ++ show expected
        else putStrLn (msg ++ " passed.")

main = do
    assertEqual 1 (1`e`[]) "Scalar construction"
    assertEqual 6 (2`e`[1] * 3`e`[1]) "Product of colinear vectors"
    assertEqual (-6`e`[1,2]) (2`e`[2] * 3`e`[1]) "Product of orthogonal vectors"
    assertEqual (6 - 6`e`[1,2]) ((2`e`[1] + 2`e`[2]) * 3`e`[1]) "Complex-like result"
    --assertEqual 12 ((4`e`[1] + 2`e`[2]) `dot` 3`e`[1]) "Dot product"
    assertEqual 3 (getGrade 0 3) "Zero grade part of a scalar"
    assertEqual 0 (getGrade 0 (1 `e` [1])) "Zero grade part of a vector"
    assertEqual 0 (getGrade 1 3) "One-grade part of a scalar"
    assertEqual 3 (getGrade 0 (3 `e` [1,1])) "Grade extraction with annihilation"
    assertEqual 0 (getGrade 2 (3 `e` [1,1])) "Grade extraction with annihilation part 2"
 
