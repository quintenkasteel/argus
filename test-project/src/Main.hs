module Main where

import Lib (usedFunction)
import Utils (helperUsed)
import PartialFunctions (unsafeHead, safeHead)
import Performance (checkEmpty, getMinimum)
import SpaceLeaks (sumWithLazyFold)
import Complexity (classifyNumber)
import Imports (sortUnique)
import Pragmas (textValue, strictSum)
import Duplicates (processUser1)
import Architecture.Core (validateUser, User(..), calculateScore)

main :: IO ()
main = do
  -- Use some of the imported functions to demonstrate the project works
  putStrLn $ usedFunction "Hello"
  print $ helperUsed 42

  -- Demonstrate some functions (these have issues Argus should detect)
  let numbers = [3, 1, 4, 1, 5, 9, 2, 6]

  -- Partial function usage
  putStrLn $ "Unsafe head: " ++ show (unsafeHead numbers)
  putStrLn $ "Safe head: " ++ show (safeHead numbers)

  -- Performance issues
  putStrLn $ "Is empty (using length): " ++ show (checkEmpty numbers)
  putStrLn $ "Minimum (using sort): " ++ show (getMinimum numbers)

  -- Space leaks
  putStrLn $ "Sum (lazy fold): " ++ show (sumWithLazyFold numbers)

  -- Complexity
  putStrLn $ "Classify 42: " ++ classifyNumber 42

  -- Imports
  putStrLn $ "Sorted unique: " ++ show (sortUnique numbers)

  -- Pragmas
  putStrLn $ "Text value: " ++ show textValue
  putStrLn $ "Strict sum: " ++ show (strictSum numbers)

  -- Duplicates
  putStrLn $ "Process user: " ++ processUser1 "John Doe" "john@example.com" 30

  -- Architecture
  let user = User 1 "Test" 25
  putStrLn $ "Validate user: " ++ show (validateUser user)
