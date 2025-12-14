{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE BangPatterns #-}
-- | Examples of space leak patterns that Argus should detect
module SpaceLeaks where

import Data.List (foldl, foldl')
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.IO as LTIO
import Control.Monad.State.Lazy as LazyState
import Control.DeepSeq (NFData, force)
import System.IO (hGetContents, openFile, IOMode(..))

-- =============================================================================
-- Lazy foldl (classic space leak)
-- =============================================================================

-- | Uses lazy foldl - thunks build up, should use foldl'
sumWithLazyFold :: [Int] -> Int
sumWithLazyFold xs = foldl (+) 0 xs

-- | Product with lazy foldl
productWithLazyFold :: [Int] -> Int
productWithLazyFold xs = foldl (*) 1 xs

-- | Custom accumulator with lazy foldl - even worse
customLazyFold :: [Int] -> (Int, Int)
customLazyFold xs = foldl (\(s, p) x -> (s + x, p * x)) (0, 1) xs

-- | Nested lazy folds - memory explosion
nestedLazyFolds :: [[Int]] -> Int
nestedLazyFolds xss = foldl (+) 0 (map (foldl (+) 0) xss)

-- =============================================================================
-- Lazy IO (resource leaks)
-- =============================================================================

-- | Lazy file reading - file handle may not close promptly
lazyReadFile :: FilePath -> IO String
lazyReadFile path = readFile path

-- | Lazy ByteString reading
lazyByteStringRead :: FilePath -> IO LBS.ByteString
lazyByteStringRead = LBS.readFile

-- | Lazy Text reading
lazyTextRead :: FilePath -> IO LT.Text
lazyTextRead = LTIO.readFile

-- | hGetContents is lazy - classic resource leak
lazyHandleRead :: FilePath -> IO String
lazyHandleRead path = do
  h <- openFile path ReadMode
  hGetContents h  -- File handle kept open until string fully evaluated

-- =============================================================================
-- Non-strict record fields
-- =============================================================================

-- | Record with lazy fields - can accumulate thunks
data LazyRecord = LazyRecord
  { lrCount  :: Int      -- Should be strict: !Int
  , lrTotal  :: Double   -- Should be strict: !Double
  , lrName   :: String   -- String is already lazy, but field is too
  }

-- | Updating lazy record fields builds thunks
updateLazyRecord :: LazyRecord -> Int -> LazyRecord
updateLazyRecord r n = r { lrCount = lrCount r + n, lrTotal = lrTotal r + fromIntegral n }

-- | Another lazy record
data Stats = Stats
  { sSum     :: Int
  , sCount   :: Int
  , sAverage :: Double
  }

-- | Repeatedly updating stats - thunk accumulation
accumulateStats :: [Int] -> Stats
accumulateStats = foldl updateStats (Stats 0 0 0)
  where
    updateStats s x = s
      { sSum = sSum s + x
      , sCount = sCount s + 1
      , sAverage = fromIntegral (sSum s + x) / fromIntegral (sCount s + 1)
      }

-- =============================================================================
-- Lazy State monad
-- =============================================================================

-- | Using lazy State monad - thunks accumulate in state
lazyStateComputation :: LazyState.State Int Int
lazyStateComputation = do
  LazyState.modify (+1)
  LazyState.modify (+1)
  LazyState.modify (+1)
  LazyState.get

-- | Running lazy state many times
runLazyState :: Int -> Int
runLazyState n = LazyState.execState (replicateM_ n (LazyState.modify (+1))) 0
  where
    replicateM_ 0 _ = pure ()
    replicateM_ k m = m >> replicateM_ (k-1) m

-- =============================================================================
-- CAF (Constant Applicative Form) space leaks
-- =============================================================================

-- | Top-level CAF that holds onto large data
bigList :: [Int]
bigList = [1..1000000]  -- Retained for program lifetime

-- | CAF holding result of expensive computation
expensiveCAF :: Int
expensiveCAF = sum [1..1000000]  -- Computed once, retained forever

-- =============================================================================
-- Accumulator not strict
-- =============================================================================

-- | Manual fold with non-strict accumulator
manualSum :: [Int] -> Int
manualSum = go 0
  where
    go acc []     = acc
    go acc (x:xs) = go (acc + x) xs  -- (acc + x) not forced!

-- | Correct version with bang pattern for comparison
strictSum :: [Int] -> Int
strictSum = go 0
  where
    go !acc []     = acc
    go !acc (x:xs) = go (acc + x) xs

-- =============================================================================
-- Retaining references too long
-- =============================================================================

-- | Keeps reference to entire input while processing
processKeepingRef :: [Int] -> (Int, [Int])
processKeepingRef xs = (sum xs, xs)  -- xs kept alive for tuple

-- | Better: process and release
processThenRelease :: [Int] -> Int
processThenRelease xs =
  let !result = sum xs
  in result