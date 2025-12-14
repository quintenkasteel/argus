{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module TypeAnnotationsExamples where

import Data.Proxy (Proxy(..))
import Data.Typeable (Typeable, typeRep)
import Text.Read (readMaybe)

--------------------------------------------------------------------------------
-- Missing top-level type signature
--------------------------------------------------------------------------------

-- Should warn: missing type signature
addNumbers x y = x + y

-- Good: has type signature
addNumbersTyped :: Int -> Int -> Int
addNumbersTyped x y = x + y

--------------------------------------------------------------------------------
-- Polymorphic numeric literals need annotation
--------------------------------------------------------------------------------

-- Should warn: ambiguous type variable
showNumber = show 42

-- Good: explicit type
showNumberTyped :: String
showNumberTyped = show (42 :: Int)

--------------------------------------------------------------------------------
-- Ambiguous read
--------------------------------------------------------------------------------

-- Should error: ambiguous type in read
parseValue s = readMaybe s

-- Good: explicit result type
parseValueTyped :: String -> Maybe Int
parseValueTyped = readMaybe

--------------------------------------------------------------------------------
-- Monomorphism restriction
--------------------------------------------------------------------------------

-- Should warn: defaulting due to monomorphism restriction
genericLength xs = fromIntegral (length xs)

-- Good: explicit polymorphic type
genericLengthTyped :: Num a => [b] -> a
genericLengthTyped xs = fromIntegral (length xs)

--------------------------------------------------------------------------------
-- TypeApplications preferred over Proxy
--------------------------------------------------------------------------------

-- Should suggest: use TypeApplications
showType :: forall a. Typeable a => Proxy a -> String
showType p = show (typeRep p)

-- Good: using TypeApplications
showTypeTA :: forall a. Typeable a => String
showTypeTA = show (typeRep @a Proxy)

--------------------------------------------------------------------------------
-- Missing forall with ScopedTypeVariables
--------------------------------------------------------------------------------

-- Should warn: type variable 'a' not in scope
-- sameType :: a -> a -> Bool  -- Without forall, 'a' creates new variable each time
-- sameType x y = True

-- Good: explicit forall
sameTypeForall :: forall a. a -> a -> Bool
sameTypeForall _ _ = True

--------------------------------------------------------------------------------
-- Return type annotation helpful
--------------------------------------------------------------------------------

-- Should suggest: add return type annotation
parseAndDouble s = do
  n <- readMaybe s
  pure (n * 2)

-- Good: clear return type
parseAndDoubleTyped :: String -> Maybe Int
parseAndDoubleTyped s = do
  n <- readMaybe s
  pure (n * 2)

--------------------------------------------------------------------------------
-- Local binding could use type annotation
--------------------------------------------------------------------------------

-- Should suggest: add type annotation to local binding
processData xs =
  let filtered = filter (> 0) xs
      total = sum filtered
  in total / fromIntegral (length filtered)

-- Good: local type annotations
processDataTyped :: [Double] -> Double
processDataTyped xs =
  let filtered :: [Double]
      filtered = filter (> 0) xs
      total :: Double
      total = sum filtered
  in total / fromIntegral (length filtered)

--------------------------------------------------------------------------------
-- Redundant type annotation
--------------------------------------------------------------------------------

-- Should warn: redundant type annotation
redundantAnnotation :: Int -> Int
redundantAnnotation x = (x :: Int) + 1

-- Good: no redundant annotation
noRedundant :: Int -> Int
noRedundant x = x + 1

--------------------------------------------------------------------------------
-- Type defaulting warning
--------------------------------------------------------------------------------

-- Should warn: type defaults to Integer
defaultedType = [1, 2, 3]

-- Good: explicit type
explicitType :: [Int]
explicitType = [1, 2, 3]

--------------------------------------------------------------------------------
-- Pattern signature
--------------------------------------------------------------------------------

-- Should suggest: use pattern signature for clarity
extractPair (x, y) = (y, x)

-- Good: pattern signature
extractPairTyped :: (a, b) -> (b, a)
extractPairTyped (x :: a, y :: b) = (y, x)

--------------------------------------------------------------------------------
-- Lambda needs annotation
--------------------------------------------------------------------------------

-- Should suggest: annotate lambda parameter
processWithLambda xs = map (\x -> x + 1) xs

-- Good: annotated lambda
processWithLambdaTyped :: [Int] -> [Int]
processWithLambdaTyped xs = map (\(x :: Int) -> x + 1) xs

--------------------------------------------------------------------------------
-- Record wildcard annotation
--------------------------------------------------------------------------------

data Config = Config
  { configTimeout :: Int
  , configRetries :: Int
  }

-- Should suggest: add type annotation with RecordWildCards
-- processConfig Config{..} = configTimeout + configRetries

-- Good: explicit deconstruction
processConfig :: Config -> Int
processConfig (Config timeout retries) = timeout + retries

--------------------------------------------------------------------------------
-- Type hole for discovery
--------------------------------------------------------------------------------

-- Note: type holes are useful for development but should be resolved
-- example = _ + 1  -- Shows needed type

-- Good: resolved type
exampleResolved :: Int -> Int
exampleResolved x = x + 1