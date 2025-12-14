module Utils
  ( helperUsed
  , helperUnused  -- Exported but never used
  , AnotherUnused(..)  -- Exported type but never used
  ) where

-- | Used by Main
helperUsed :: Int -> Int
helperUsed = (+1)

-- | Exported but never imported anywhere
helperUnused :: String -> String
helperUnused = reverse

-- | A type that is exported but never used
data AnotherUnused = MkUnused Int String

-- | Internal unused function
privateUnused :: Double -> Double
privateUnused = (*2)
