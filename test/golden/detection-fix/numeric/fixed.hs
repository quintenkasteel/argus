{-# LANGUAGE OverloadedStrings #-}

module NumericTests where


-- Arithmetic simplifications

-- Adding zero (redundant)
addZeroTest :: Int -> Int
addZeroTest x = x + 0

addZeroLeft :: Int -> Int
addZeroLeft x = 0 + x

-- Subtracting zero (redundant)
subtractZeroTest :: Int -> Int
subtractZeroTest x = x - 0

-- Multiplying by one (redundant)
multiplyOneTest :: Int -> Int
multiplyOneTest x = x * 1

multiplyOneLeft :: Int -> Int
multiplyOneLeft x = 1 * x

-- Multiplying by zero (always zero)
multiplyZeroTest :: Int -> Int
multiplyZeroTest x = x * 0

multiplyZeroLeft :: Int -> Int
multiplyZeroLeft x = 0 * x

-- Dividing by one (redundant)
divideOneTest :: Double -> Double
divideOneTest x = x / 1

-- Double negation (cancels out)
doubleNegateTest :: Int -> Int
doubleNegateTest x = negate (negate x)

-- Subtracting from self (always zero)
subtractSelfTest :: Int -> Int
subtractSelfTest x = x - x

-- abs of negate is abs
absNegateTest :: Int -> Int
absNegateTest x = abs (negate x)

-- negate of abs
negateAbsTest :: Int -> Int
negateAbsTest x = negate (abs x)

-- Raising to power 1 (redundant)
powerOneTest :: Int -> Int
powerOneTest x = x ^ 1

-- Raising to power 0 (always 1)
powerZeroTest :: Int -> Int
powerZeroTest x = x ^ 0

-- Numeric conversions

-- fromIntegral to same type (redundant)
fromIntegralIdTest :: Int -> Int
fromIntegralIdTest x = fromIntegral (x :: Int) :: Int

-- realToFrac to same type (redundant)
realToFracIdTest :: Double -> Double
realToFracIdTest x = realToFrac (x :: Double) :: Double

-- toInteger . fromIntegral simplification
toIntegerFromIntegralTest :: Int -> Integer
toIntegerFromIntegralTest x = toInteger (fromIntegral x)

-- Conversion chain that could be simplified
nestedConversion :: Int -> Int
nestedConversion x = fromIntegral (fromIntegral x :: Integer)

-- Redundant round-trip conversion
roundTripConversion :: Double -> Double
roundTripConversion x = fromIntegral (floor x :: Int)

-- Integer arithmetic patterns

-- Multiple identity operations
multipleIdentities :: Int -> Int
multipleIdentities x = (x + 0) * 1 - 0

-- Nested redundant operations
nestedRedundant :: Integer -> Integer
nestedRedundant x = (x ^ 1) * 1

-- Complex arithmetic with identities
complexArithmetic :: Int -> Int -> Int
complexArithmetic x y = x * 1 + y - 0 + 0 * x

-- Power operations
powerPatterns :: Integer -> Integer
powerPatterns x = x ^ 0 + x ^ 1 * 1

-- Floating point patterns

-- Division by one
floatDivOne :: Double -> Double
floatDivOne x = x / 1.0

-- Multiplication patterns
floatMulOne :: Double -> Double
floatMulOne x = x * 1.0

-- Zero patterns
floatAddZero :: Double -> Double
floatAddZero x = x + 0.0

floatSubZero :: Double -> Double
floatSubZero x = x - 0.0

-- Comparison and equality

-- Comparing to zero
compareZero :: Int -> Bool
compareZero x = x > 0

-- Using compare for Ordering construction
manualOrdering :: Int -> Int -> Ordering
manualOrdering x y = if x < y then LT else if x > y then GT else EQ

-- compare with function application
compareWithFunc :: (a -> Int) -> a -> a -> Ordering
compareWithFunc f x y = compare (f x) (f y)

-- Absolute value comparisons
absCompareTest :: Int -> Int -> Bool
absCompareTest x y = abs x == abs y

-- Chain comparison
chainCompare :: Int -> Int -> Int -> Bool
chainCompare x y z = x <= y && y <= z

-- Numeric precision warnings

-- Float equality (dangerous)
floatEqualityTest :: Double -> Double -> Bool
floatEqualityTest x y = (x :: Double) == (y :: Double)

-- Division precision issue
divisionPrecisionTest :: Double -> Double -> Double
divisionPrecisionTest x y = x / y * y

-- Overflow/underflow patterns

-- Adding to maxBound (overflow)
overflowTest :: Int -> Int
overflowTest n = maxBound + n

-- Subtracting from minBound (underflow)
underflowTest :: Int -> Int
underflowTest n = minBound - n

-- Real-world patterns

-- Calculate percentage (with redundant operations)
calculatePercentage :: Double -> Double -> Double
calculatePercentage part total = (part / total) * 1 * 100

-- Distance calculation with identities
distance :: Double -> Double -> Double
distance x y = abs (x - y) + 0

-- Normalize value with redundant ops
normalizeValue :: Double -> Double -> Double
normalizeValue value maxVal = (value / maxVal) / 1

-- Price calculation with zeros
calculatePrice :: Double -> Int -> Double
calculatePrice price quantity = price * fromIntegral quantity + 0

-- Power calculation chain
powerChain :: Integer -> Integer
powerChain x = ((x ^ 1) ^ 1) * 1

-- Mixed conversion issues
mixedConversion :: Int -> Double
mixedConversion x = fromIntegral (fromIntegral x :: Integer)

-- Complex nested arithmetic
complexNested :: Int -> Int
complexNested x = negate (negate (x + 0))

-- Redundant absolute value
redundantAbs :: Int -> Int
redundantAbs x = abs (negate x) + 0 * 1

-- Identity function via arithmetic
identityViaArith :: Int -> Int
identityViaArith x = x - 0 + 0 * 999

-- Constant folding opportunities
constantFold :: Int -> Int
constantFold x = x + (5 - 5) * (3 + 0)

-- Double conversion pattern
doubleConvert :: Int -> Int
doubleConvert x = fromIntegral (toInteger x)

-- Fractional identity
fractionalId :: Double -> Double
fractionalId x = x / 1 + 0 - 0
