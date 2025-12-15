{-# LANGUAGE OverloadedStrings #-}

module ImportsExamples where

-- Should warn: implicit Prelude import
import Data.List
import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Set as Set

-- Should warn: redundant import
import Data.List (sort, nub, sort)

-- Should warn: unused import
import Control.Monad (when, unless, forever)

-- Should warn: unqualified import of common clashing names
import Data.Text (Text, pack)

--------------------------------------------------------------------------------
-- Examples using the imports
--------------------------------------------------------------------------------

-- Uses sort from Data.List (implicit)
sortExample :: [Int] -> [Int]
sortExample = sort

-- Uses nub from Data.List
uniqueExample :: Eq a => [a] -> [a]
uniqueExample = nub

-- Uses fromMaybe from Data.Maybe
defaultExample :: Maybe Int -> Int
defaultExample = fromMaybe 0

-- Uses Map from Data.Map
mapExample :: Map String Int
mapExample = M.empty

-- Uses Set from Data.Set
setExample :: Set.Set Int
setExample = Set.empty

-- Uses 'when' from Control.Monad (unused: unless, forever)
conditionalAction :: Bool -> IO ()
conditionalAction b = when b $ putStrLn "active"

-- Uses Text from Data.Text
textExample :: Text
textExample = pack "hello"

--------------------------------------------------------------------------------
-- Should organize imports
--------------------------------------------------------------------------------

{-
Ideal import organization:
1. Explicit Prelude import (if using NoImplicitPrelude)
2. Base/standard library imports
3. External package imports
4. Local/project imports
-}