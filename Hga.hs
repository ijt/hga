module Hga where

import Data.List
import qualified Test.QuickCheck as QC

data NamedThing a = WithName { name :: String, contents :: a } deriving Show

-- Multivector
-- These should always be returned in normal form.
data Mv = BladeSum { mvTerms :: [Blade] }

instance Eq Mv where
    a == b = 
        mvTerms (mvNormalForm a) == mvTerms (mvNormalForm b)

instance Show Mv where
    show (BladeSum []) = "0"
    show a = stringJoin " + " $ map show $ mvTerms a

instance Fractional Mv where
    fromRational r = BladeSum [Blade (doubleFromRational r) []]
    recip x = mvNormalForm $ (scalar $ 1 / (mag x)^2) * mvRev x 
    a / b = mvNormalForm $ a * recip b

doubleFromRational :: Rational -> Double
doubleFromRational r = fromRational r

-- Scaled basis blade: the pseudoscalar for the space it spans.
-- These should always be returned in normal form.
data Blade = Blade {bScale :: Double, bIndices :: [Int]} deriving (Ord, Eq)

instance Show Blade where
    show (Blade s []) = show s
    show b = (show $ bScale b) ++ "`e`" ++ (show $ bIndices b)

-- Constructs a multivector from a scaled blade.
e :: Double -> [Int] -> Mv
s `e` indices = mvNormalForm $ BladeSum [Blade s indices]

-- Scalar constructor
scalar :: Double -> Mv
scalar x = x `e` []

-- Scalar extractor.
-- TESTME.
scalarPart :: Mv -> Double
scalarPart x = sum $ map bScale $ mvTerms $ getGrade 0 x

-- Vector constructor
vector :: [Double] -> Mv
vector x = mvNormalForm $ BladeSum [Blade xi [i] | (xi, i) <- zip x [1..]]

-- Vector extractor
-- TODO

instance Num Mv where
    a + b = mvNormalForm $ BladeSum (mvTerms a ++ mvTerms b)
    a - b = mvNormalForm $ BladeSum (mvTerms a ++ (map bladeNeg $ mvTerms b))

    -- Geometric (Clifford) product
    a * b = mvNormalForm $ BladeSum [bladeMul x y | x <- mvTerms a, y <- mvTerms b]

    fromInteger i = (fromIntegral i) `e` []

    abs x = (mag x) `e` []

    signum (BladeSum [Blade scale []]) = (signum scale) `e` []
    signum (BladeSum []) = scalar 0
    signum _ = undefined

mag :: Mv -> Double
mag mv = sqrt $ sum $ map bladeMag2 $ mvTerms mv

stringJoin :: String -> [String] -> String
stringJoin sep parts =
    concat $ intersperse sep parts

bladeNeg :: Blade -> Blade
bladeNeg b = Blade (- bScale b) (bIndices b)

bladeMul :: Blade -> Blade -> Blade
bladeMul x y =
    bladeNormalForm $ Blade (bScale x * bScale y) (bIndices x ++ bIndices y)

bladeNonZero :: Blade -> Bool
bladeNonZero b = bScale b /= 0

mvNormalForm :: Mv -> Mv
mvNormalForm mv =
    BladeSum $ filter bladeNonZero $ combineLikeTerms $ sortByIndices $ map bladeNormalForm $ mvTerms mv

sortByIndices :: [Blade] -> [Blade]
sortByIndices bs = sortBy (\x y -> if bIndices x < bIndices y then LT else GT) bs

combineLikeTerms :: [Blade] -> [Blade]
combineLikeTerms [] = []
combineLikeTerms [x] = [x]
combineLikeTerms (x:y:rest) | (bIndices x == bIndices y) =
                                combineLikeTerms $ (Blade (bScale x + bScale y) (bIndices x)) : rest
                            | otherwise = x : combineLikeTerms (y:rest)

bladeNormalForm :: Blade -> Blade
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
            k = (abs $ (grade x) - (grade y))
            xy = bladeMul x y

bWedge :: Blade -> Blade -> Blade
bWedge x y =
    bladeNormalForm $ bGetGrade k xy
        where
            k = (grade x) + (grade y)
            xy = bladeMul x y

bIsOfGrade :: Blade -> Int -> Bool
blade `bIsOfGrade` k =
    (length $ bIndices blade) == k

-- Imaginary-like element in the 1,2 plane
i :: Mv
i = 1`e`[1,2]

-- Multivector exponential
mvExp :: Mv -> Mv
-- TODO: Find a better way than "`e`[]"
mvExp x = sum [(1.0 / factorialf k)`e`[] * (x^k) | k <- [0..30]]

factorialf :: Integer -> Double
factorialf k = fromIntegral $ factorial k
 
factorial :: Integer -> Integer
factorial k = product [1..k]

bladeMag2 :: Blade -> Double
bladeMag2 b = (bScale b)^2

mvRev :: Mv -> Mv
mvRev a = mvNormalForm $ BladeSum $ map bReverse $ mvTerms a

bReverse :: Blade -> Blade
bReverse b = bladeNormalForm $ Blade (bScale b) (reverse $ bIndices b)

-- Approximate equality
tol :: Double
tol = 1e-5

(~=) :: Mv -> Mv -> Bool
a ~= b = (absDiff a b) <= tol

absDiff :: Mv -> Mv -> Double
absDiff a b = mag $ a - b

-- TESTS

assertEqual :: (Eq a, Show a) => a -> a -> String -> IO ()
assertEqual expected actual msg =
    if actual /= expected
        then error $ msg ++ ": " ++ show expected ++ " /= " ++ show actual
        else putStrLn (msg ++ " passed.")

assertAlmostEqual :: Mv -> Mv -> String -> IO ()
assertAlmostEqual expected actual msg =
    if expected ~= actual
        then putStrLn (msg ++ " passed.")
        else error $ msg ++ ": " ++ show expected ++ " /= " ++ show actual ++ " within tolerance " ++ show tol

g encodedIndices = BladeSum $ [Blade 1.0 indices]
    where indices = decode encodedIndices

decode :: Int -> [Int]
decode = reverse . revDecode
    where
        revDecode :: Int -> [Int]
        revDecode 0 = []
        revDecode n = n `mod` 10 : revDecode (n `div` 10)

test_hga :: IO ()
test_hga = do
    -- Show
    assertEqual "0" (show (BladeSum [])) "Show an empty multivector"
    assertEqual "0" (show (0 :: Mv)) "Show 0"
    assertEqual "1.0" (show (1 `e` [])) "Show 1"
    assertEqual "1.5" (show (1.5 :: Mv)) "Show 1.5"
    assertEqual "0" (show (0 `e` [1])) "Show a zero-scaled vector"

    -- Construction
    assertEqual 1 (1`e`[]) "Scalar construction 1 == 1e[]"

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

    -- Num typeclass
    assertEqual (-1) (signum $ scalar $ -1) "Signum of -1"
    assertEqual 0 (signum $ scalar 0) "Signum of 0"
    assertEqual 1 (signum $ scalar 1) "Signum of 1"

    -- Division
    assertEqual 1 (scalar 1 / scalar 1) "One over one"
    assertEqual 0 (scalar 0 / scalar 1) "Zero over one"

    -- Approximate equality
    putStrLn "Everything approximately equals itself."
    QC.quickCheck prop_selfApproxEqual

    -- Exponentiation
    putStrLn "Exponential:"
    QC.quickCheck prop_exponential

    -- Reverse
    assertEqual 1 (mvRev 1) "Reverse of a scalar is the same"
    assertEqual (1`e`[1]) (mvRev $ 1`e`[1]) "Reverse of a vector is the same"
    assertEqual (-1`e`[1,2]) (mvRev $ 1`e`[1,2]) "Reverse of a bivector is negated"
    assertEqual (-1`e`[1,2,3]) (mvRev $ 1`e`[1,2,3]) "Reverse of a trivector is negated"

    -- Laplace expansion
    putStrLn "Laplace expansion:"
    QC.quickCheck prop_laplaceExpansion

    -- Inverse
    putStrLn "vector inverse:"
    QC.quickCheck prop_vectorInverse

    -- Cross product
 
    -- Projection
    
    -- Rotation by spinors
   
    -- Conversion to and from complex numbers

    -- Easier construction of vectors. ga [1,2,3]

    -- Parsing of a little expression language

prop_vectorInverse coords = (sum $ map abs coords) /= 0 QC.==> (v / v) ~= 1
    where 
        v = vector coords 
        types = (coords :: [Double])

prop_selfApproxEqual x = scalar x ~= scalar x
    where types = x :: Double

laplaceLeft :: Mv -> Mv -> Mv -> Mv
laplaceLeft a b c = a `dot` (b `wedge` c)

laplaceRight :: Mv -> Mv -> Mv -> Mv
laplaceRight a b c = (a `dot` b) * c - (a `dot` c) * b

ll a b c = laplaceLeft (v a) (v b) (v c)
lr a b c = laplaceRight (v a) (v b) (v c)
v = vector

prop_laplaceExpansion la lb lc = laplaceLeft a b c ~= laplaceRight a b c
    where
        a = vector la
        b = vector lb
        c = vector lc
        types = (la::[Double], lb::[Double], lc::[Double])

prop_exponential x = abs (e1 - e2) < 1e-5
    where
        e1 = exp x
        e2 = scalarPart $ mvExp $ scalar x
        types = x :: Double

