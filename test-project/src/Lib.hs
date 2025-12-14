module Lib
  ( usedFunction
  , unusedExportedFunction  -- This is exported but never used anywhere
  , internalUsed
  ) where

-- | This function is used by Main
usedFunction :: String -> String
usedFunction s = "Lib says: " ++ s ++ internalUsed

-- | This function is exported but never imported/used anywhere
unusedExportedFunction :: Int -> Int
unusedExportedFunction x = x * 2

-- | This is internal and used by usedFunction
internalUsed :: String
internalUsed = "!"

-- | This function is not exported and not used - truly dead code
unusedInternalFunction :: Bool -> Bool
unusedInternalFunction = not
