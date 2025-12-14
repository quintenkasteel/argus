-- | Examples of duplicate/near-duplicate code that Argus should detect
module Duplicates where

import Data.Char (toUpper, toLower)

-- =============================================================================
-- Exact duplicates (copy-paste code)
-- =============================================================================

-- | Process user input - version 1
processUser1 :: String -> String -> Int -> String
processUser1 name email age =
  let cleanName = filter (/= ' ') name
      cleanEmail = map toLower email
      ageStr = if age > 0 then show age else "unknown"
  in cleanName ++ " <" ++ cleanEmail ++ "> (" ++ ageStr ++ ")"

-- | Process user input - version 2 (exact duplicate!)
processUser2 :: String -> String -> Int -> String
processUser2 name email age =
  let cleanName = filter (/= ' ') name
      cleanEmail = map toLower email
      ageStr = if age > 0 then show age else "unknown"
  in cleanName ++ " <" ++ cleanEmail ++ "> (" ++ ageStr ++ ")"

-- =============================================================================
-- Near duplicates (similar structure, minor differences)
-- =============================================================================

-- | Calculate area of rectangle
rectangleArea :: Double -> Double -> Double
rectangleArea width height = width * height

-- | Calculate area of parallelogram (same formula!)
parallelogramArea :: Double -> Double -> Double
parallelogramArea base height = base * height

-- | Format integer for display
formatInt :: Int -> String
formatInt n =
  if n < 0
  then "(" ++ show (abs n) ++ ")"
  else show n

-- | Format double for display (same pattern)
formatDouble :: Double -> String
formatDouble d =
  if d < 0
  then "(" ++ show (abs d) ++ ")"
  else show d

-- =============================================================================
-- Similar validation functions (could be unified)
-- =============================================================================

-- | Validate username
validateUsername :: String -> Either String String
validateUsername s
  | null s = Left "Username cannot be empty"
  | length s < 3 = Left "Username must be at least 3 characters"
  | length s > 50 = Left "Username cannot exceed 50 characters"
  | not (all isValidChar s) = Left "Username contains invalid characters"
  | otherwise = Right s
  where
    isValidChar c = c `elem` (['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "_-")

-- | Validate email (very similar structure)
validateEmail :: String -> Either String String
validateEmail s
  | null s = Left "Email cannot be empty"
  | length s < 5 = Left "Email must be at least 5 characters"
  | length s > 100 = Left "Email cannot exceed 100 characters"
  | '@' `notElem` s = Left "Email must contain @"
  | otherwise = Right s

-- | Validate password (similar structure again)
validatePassword :: String -> Either String String
validatePassword s
  | null s = Left "Password cannot be empty"
  | length s < 8 = Left "Password must be at least 8 characters"
  | length s > 100 = Left "Password cannot exceed 100 characters"
  | not (any (`elem` ['0'..'9']) s) = Left "Password must contain a digit"
  | otherwise = Right s

-- =============================================================================
-- Duplicate list processing patterns
-- =============================================================================

-- | Sum positive integers
sumPositive :: [Int] -> Int
sumPositive xs = sum $ filter (> 0) xs

-- | Count positive integers
countPositive :: [Int] -> Int
countPositive xs = length $ filter (> 0) xs

-- | Get positive integers
getPositive :: [Int] -> [Int]
getPositive xs = filter (> 0) xs

-- | Sum negative integers (same pattern)
sumNegative :: [Int] -> Int
sumNegative xs = sum $ filter (< 0) xs

-- | Count negative integers (same pattern)
countNegative :: [Int] -> Int
countNegative xs = length $ filter (< 0) xs

-- =============================================================================
-- Copy-paste with variable renaming only
-- =============================================================================

-- | Calculate total for items
calculateItemsTotal :: [(String, Double)] -> Double
calculateItemsTotal items =
  let prices = map snd items
      subtotal = sum prices
      tax = subtotal * 0.1
      total = subtotal + tax
  in total

-- | Calculate total for products (same logic, different names)
calculateProductsTotal :: [(String, Double)] -> Double
calculateProductsTotal products =
  let costs = map snd products
      subtotal = sum costs
      tax = subtotal * 0.1
      total = subtotal + tax
  in total

-- =============================================================================
-- For comparison: Properly abstracted versions
-- =============================================================================

-- | Generic formatter that handles both Int and Double
formatNumber :: (Ord a, Num a, Show a) => a -> String
formatNumber n
  | n < 0     = "(" ++ show (abs n) ++ ")"
  | otherwise = show n

-- | Generic validation helper
validateString :: String -> Int -> Int -> (String -> Bool) -> String -> Either String String
validateString fieldName minLen maxLen isValid s
  | null s = Left $ fieldName ++ " cannot be empty"
  | length s < minLen = Left $ fieldName ++ " must be at least " ++ show minLen ++ " characters"
  | length s > maxLen = Left $ fieldName ++ " cannot exceed " ++ show maxLen ++ " characters"
  | not (isValid s) = Left $ fieldName ++ " contains invalid characters"
  | otherwise = Right s

-- | Generic total calculator
calculateTotal :: Double -> [(a, Double)] -> Double
calculateTotal taxRate items =
  let subtotal = sum $ map snd items
      tax = subtotal * taxRate
  in subtotal + tax
