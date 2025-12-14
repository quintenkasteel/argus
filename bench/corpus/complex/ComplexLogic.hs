{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}

-- | Complex module with high cyclomatic complexity for stress testing
module ComplexLogic where

import Data.List
import Data.Maybe
import Data.Text (Text)
import Data.Text qualified as T

-- | Complex decision tree with high cyclomatic complexity
complexDecision :: Int -> Int -> Int -> String -> Bool -> Maybe Int
complexDecision a b c mode strict
  | a < 0 && b < 0 && c < 0 = Nothing
  | a < 0 && b < 0 = Just c
  | a < 0 && c < 0 = Just b
  | b < 0 && c < 0 = Just a
  | a < 0 = Just (b + c)
  | b < 0 = Just (a + c)
  | c < 0 = Just (a + b)
  | mode == "sum" && strict = Just (a + b + c)
  | mode == "sum" = Just (a + b + c + 1)
  | mode == "product" && strict = Just (a * b * c)
  | mode == "product" = Just (a * b * c + 1)
  | mode == "max" && strict = Just (maximum [a, b, c])
  | mode == "max" = Just (maximum [a, b, c] + 1)
  | mode == "min" && strict = Just (minimum [a, b, c])
  | mode == "min" = Just (minimum [a, b, c] + 1)
  | mode == "avg" = Just ((a + b + c) `div` 3)
  | otherwise = Nothing

-- | Complex validation with many branches
validateInput :: Maybe Text -> Maybe Int -> Maybe Bool -> Either String (Text, Int, Bool)
validateInput mtext mint mbool = case mtext of
  Nothing -> Left "Missing text"
  Just t
    | T.null t -> Left "Empty text"
    | T.length t > 1000 -> Left "Text too long"
    | T.any (== '\0') t -> Left "Null character in text"
    | otherwise -> case mint of
        Nothing -> Left "Missing int"
        Just i
          | i < 0 -> Left "Negative int"
          | i > 1000000 -> Left "Int too large"
          | otherwise -> case mbool of
              Nothing -> Left "Missing bool"
              Just b -> Right (t, i, b)

-- | Complex state machine
data State = Start | Running | Paused | Stopped | Error
  deriving (Eq, Show)

data Event = StartE | PauseE | ResumeE | StopE | ErrorE | ResetE
  deriving (Eq, Show)

transition :: State -> Event -> Either String State
transition state event = case (state, event) of
  (Start, StartE) -> Right Running
  (Start, StopE) -> Right Stopped
  (Start, ErrorE) -> Right Error
  (Start, _) -> Left "Invalid event from Start"

  (Running, PauseE) -> Right Paused
  (Running, StopE) -> Right Stopped
  (Running, ErrorE) -> Right Error
  (Running, _) -> Left "Invalid event from Running"

  (Paused, ResumeE) -> Right Running
  (Paused, StopE) -> Right Stopped
  (Paused, ErrorE) -> Right Error
  (Paused, _) -> Left "Invalid event from Paused"

  (Stopped, ResetE) -> Right Start
  (Stopped, _) -> Left "Invalid event from Stopped"

  (Error, ResetE) -> Right Start
  (Error, _) -> Left "Invalid event from Error"

-- | Complex pattern matching
processValue :: Either (Maybe (Either Int String)) [Maybe Bool] -> String
processValue = \case
  Left Nothing -> "left-nothing"
  Left (Just (Left i))
    | i < 0 -> "left-just-left-negative"
    | i == 0 -> "left-just-left-zero"
    | i < 10 -> "left-just-left-small"
    | i < 100 -> "left-just-left-medium"
    | otherwise -> "left-just-left-large"
  Left (Just (Right s))
    | null s -> "left-just-right-empty"
    | length s < 5 -> "left-just-right-short"
    | otherwise -> "left-just-right-long"
  Right [] -> "right-empty"
  Right [Nothing] -> "right-single-nothing"
  Right [Just True] -> "right-single-true"
  Right [Just False] -> "right-single-false"
  Right bools
    | all isNothing bools -> "right-all-nothing"
    | all (== Just True) bools -> "right-all-true"
    | all (== Just False) bools -> "right-all-false"
    | any (== Just True) bools && any (== Just False) bools -> "right-mixed"
    | otherwise -> "right-partial"

-- | Complex recursion with guards
complexRecursion :: Int -> Int -> Int -> [Int] -> Int
complexRecursion limit acc step xs
  | limit <= 0 = acc
  | null xs = acc
  | step <= 0 = complexRecursion (limit - 1) acc 1 xs
  | step > length xs = complexRecursion limit acc (step `mod` length xs + 1) xs
  | otherwise =
      let x = head xs
          newAcc = if x > 0 then acc + x else acc
          newXs = tail xs ++ [x * step]
          newStep = if x `mod` 2 == 0 then step + 1 else step - 1
      in complexRecursion (limit - 1) newAcc (max 1 newStep) newXs

-- | Complex fold with multiple accumulators
complexFold :: [(Int, String, Bool)] -> (Int, Int, Int, [String])
complexFold = foldr go (0, 0, 0, [])
  where
    go (i, s, b) (total, trueCount, falseCount, strs)
      | i < 0 && b = (total + i, trueCount + 1, falseCount, s : strs)
      | i < 0 = (total + i, trueCount, falseCount + 1, strs)
      | i == 0 && b = (total, trueCount + 1, falseCount, s : strs)
      | i == 0 = (total, trueCount, falseCount + 1, s : strs)
      | b = (total + i, trueCount + 1, falseCount, s : strs)
      | otherwise = (total + i, trueCount, falseCount + 1, strs)

-- | Nested conditionals
nestedConditionals :: Int -> Int -> Int -> Int -> String
nestedConditionals a b c d
  | a > 0 =
      if b > 0
        then if c > 0
               then if d > 0
                      then "all positive"
                      else "d negative"
               else if d > 0
                      then "c negative, d positive"
                      else "c and d negative"
        else if c > 0
               then if d > 0
                      then "b negative only"
                      else "b and d negative"
               else "b and c negative"
  | a == 0 =
      if b == 0
        then if c == 0
               then if d == 0 then "all zero" else "only d"
               else "c and maybe d"
        else "b and maybe others"
  | otherwise = "a is negative"

-- | Many partial function uses
dangerousFunction :: [Int] -> [String] -> Int
dangerousFunction ints strs =
  let first = head ints
      second = ints !! 1
      third = ints !! 2
      lastInt = last ints
      firstStr = head strs
      lastStr = last strs
      strLen = length (head (words firstStr))
  in first + second + third + lastInt + strLen + length lastStr
