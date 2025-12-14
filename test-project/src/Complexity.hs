{-# LANGUAGE LambdaCase #-}
-- | Examples of high complexity functions that Argus should flag
module Complexity where

import Data.Maybe (fromMaybe)

-- =============================================================================
-- High Cyclomatic Complexity (many branches)
-- =============================================================================

-- | Function with many if-else branches - high cyclomatic complexity
classifyNumber :: Int -> String
classifyNumber n
  | n < -100    = "very negative"
  | n < -50     = "quite negative"
  | n < -10     = "somewhat negative"
  | n < 0       = "slightly negative"
  | n == 0      = "zero"
  | n < 10      = "slightly positive"
  | n < 50      = "somewhat positive"
  | n < 100     = "quite positive"
  | n < 500     = "large positive"
  | n < 1000    = "very large positive"
  | otherwise   = "huge"

-- | Complex pattern matching with guards
complexMatcher :: Maybe Int -> Maybe Int -> String -> Either String Int -> String
complexMatcher m1 m2 s e = case (m1, m2) of
  (Nothing, Nothing) -> case e of
    Left err -> "error: " ++ err
    Right n  -> if n > 0
                then if n > 100 then "big right" else "small right"
                else "negative right"
  (Just x, Nothing) -> case s of
    "" -> if x > 0 then "positive x, empty s" else "non-positive x, empty s"
    _  -> if length s > 10
          then if x > length s then "x bigger" else "s longer"
          else "short s"
  (Nothing, Just y) -> case e of
    Left _ -> if y > 50 then "y big, error" else "y small, error"
    Right n -> if y > n then "y wins" else "n wins"
  (Just x, Just y) -> case (x > 0, y > 0) of
    (True, True)   -> if x > y then "x larger positive" else "y larger positive"
    (True, False)  -> "x pos, y neg"
    (False, True)  -> "x neg, y pos"
    (False, False) -> if x < y then "x more negative" else "y more negative"

-- =============================================================================
-- High Cognitive Complexity (nested structures)
-- =============================================================================

-- | Deeply nested function - hard to understand
deeplyNested :: Int -> Int -> Int -> Maybe Int
deeplyNested x y z =
  if x > 0
  then if y > 0
       then if z > 0
            then if x + y + z > 100
                 then Just (x + y + z)
                 else if x * y * z > 1000
                      then Just (x * y * z)
                      else Nothing
            else if z < -10
                 then Just z
                 else Nothing
       else if y < -10
            then if z > 0
                 then Just (y + z)
                 else Nothing
            else Nothing
  else if x < -10
       then Just x
       else Nothing

-- | Multiple levels of case analysis
multipleCases :: Either String Int -> Maybe Bool -> [Int] -> String
multipleCases e m xs = case e of
  Left err -> case m of
    Nothing -> "error, no bool"
    Just b -> if b then "error but true" else "error and false"
  Right n -> case m of
    Nothing -> case xs of
      [] -> "right, no bool, empty"
      [x] -> if x == n then "match" else "no match"
      _ -> "right, no bool, many"
    Just b -> case xs of
      [] -> if b then "right, true, empty" else "right, false, empty"
      _ -> if b && n > 0
           then if length xs > 5 then "complex true big" else "complex true small"
           else if length xs > 5 then "complex false big" else "complex false small"

-- =============================================================================
-- Long Functions (too many lines/expressions)
-- =============================================================================

-- | Very long function that does too much
doEverything :: Int -> String -> [Int] -> IO ()
doEverything n s xs = do
  -- Part 1: Input validation
  let validated = if n < 0 then 0 else n
  let cleanedS = filter (/= ' ') s
  let filteredXs = filter (> 0) xs

  -- Part 2: Basic processing
  let doubled = validated * 2
  let reversed = reverse cleanedS
  let summed = sum filteredXs

  -- Part 3: More processing
  let combined = doubled + summed
  let formatted = reversed ++ ": " ++ show combined

  -- Part 4: Conditional logic
  if combined > 100
    then do
      putStrLn "Large result"
      putStrLn $ "Value: " ++ show combined
    else if combined > 50
      then do
        putStrLn "Medium result"
        putStrLn $ "Value: " ++ show combined
      else do
        putStrLn "Small result"
        putStrLn $ "Value: " ++ show combined

  -- Part 5: Final output
  putStrLn formatted
  putStrLn $ "Original n: " ++ show n
  putStrLn $ "Original s: " ++ s
  putStrLn $ "Original xs length: " ++ show (length xs)

  -- Part 6: Cleanup summary
  let summary = "Processed " ++ show (length xs) ++ " items"
  putStrLn summary
  putStrLn "Done"

-- =============================================================================
-- Complex Boolean Expressions
-- =============================================================================

-- | Complex boolean condition - hard to understand
complexCondition :: Int -> Int -> Int -> Bool -> Bool -> Bool
complexCondition a b c flag1 flag2 =
  (a > 0 && b > 0 && c > 0 && flag1) ||
  (a < 0 && b < 0 && c < 0 && flag2) ||
  (a > b && b > c && flag1 && flag2) ||
  (a == 0 && (b == 0 || c == 0) && (flag1 || flag2)) ||
  (not flag1 && not flag2 && a + b + c == 0)

-- | Nested boolean logic
nestedBooleans :: Maybe Int -> Maybe Int -> Bool -> Bool
nestedBooleans mx my flag = case mx of
  Nothing -> case my of
    Nothing -> flag
    Just y  -> (y > 0 && flag) || (y < 0 && not flag)
  Just x -> case my of
    Nothing -> (x > 0 && flag) || (x < 0 && not flag)
    Just y  -> (x > 0 && y > 0 && flag) ||
               (x < 0 && y < 0 && not flag) ||
               (x > y && flag) ||
               (x < y && not flag)

-- =============================================================================
-- Functions with too many parameters
-- =============================================================================

-- | Too many parameters - should probably use a record type
manyParams :: Int -> Int -> Int -> String -> String -> Bool -> Bool -> Maybe Int -> [Int] -> String
manyParams a b c s1 s2 f1 f2 m xs =
  let numPart = show (a + b + c)
      strPart = s1 ++ s2
      boolPart = if f1 && f2 then "both" else "not both"
      maybePart = fromMaybe 0 m
      listPart = sum xs
  in numPart ++ strPart ++ boolPart ++ show maybePart ++ show listPart

-- =============================================================================
-- For comparison: Well-structured alternatives
-- =============================================================================

-- | Data type instead of many parameters
data ProcessConfig = ProcessConfig
  { pcNumbers    :: (Int, Int, Int)
  , pcStrings    :: (String, String)
  , pcFlags      :: (Bool, Bool)
  , pcMaybeVal   :: Maybe Int
  , pcListVal    :: [Int]
  }

-- | Cleaner function with config
processWithConfig :: ProcessConfig -> String
processWithConfig cfg =
  let (a, b, c) = pcNumbers cfg
      (s1, s2)  = pcStrings cfg
      (f1, f2)  = pcFlags cfg
  in show (a + b + c) ++ s1 ++ s2 ++
     (if f1 && f2 then "both" else "not both") ++
     show (fromMaybe 0 (pcMaybeVal cfg)) ++
     show (sum (pcListVal cfg))
