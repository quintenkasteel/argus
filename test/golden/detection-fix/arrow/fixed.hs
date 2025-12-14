{-# LANGUAGE Arrows #-}
{-# LANGUAGE OverloadedStrings #-}

module ArrowExamples where

import Control.Arrow
import Control.Category (Category(..))
import Prelude hiding ((.), id)
import qualified Prelude

--------------------------------------------------------------------------------
-- Basic Arrow Identity and Composition
--------------------------------------------------------------------------------

-- arr id ==> id
arrIdExample :: Arrow a => a Int Int
arrIdExample = arr Prelude.id

arrIdInCompose :: Arrow a => a Int Int
arrIdInCompose = arr (+ 1) >>> arr Prelude.id

-- arr f >>> arr g ==> arr (g . f)
arrComposeExample :: Arrow a => a Int Int
arrComposeExample = arr (+ 1) >>> arr (* 2)

arrComposeChain :: Arrow a => a String Int
arrComposeChain = arr words >>> arr length

arrComposeMultiple :: Arrow a => a Int Bool
arrComposeMultiple = arr abs >>> arr even >>> arr not

--------------------------------------------------------------------------------
-- first and second with id
--------------------------------------------------------------------------------

-- first id ==> id
firstIdExample :: Arrow a => a (Int, String) (Int, String)
firstIdExample = first id

firstIdInPipeline :: Arrow a => a (Int, String) (Int, String)
firstIdInPipeline = first (arr (+ 1)) >>> first id

-- second id ==> id
secondIdExample :: Arrow a => a (String, Int) (String, Int)
secondIdExample = second id

secondIdInPipeline :: Arrow a => a (Int, Int) (Int, Int)
secondIdInPipeline = second id >>> arr swap
  where swap (a, b) = (b, a)

--------------------------------------------------------------------------------
-- first >>> second patterns
--------------------------------------------------------------------------------

-- first f >>> second g ==> f *** g
firstSecondExample :: Arrow a => a (Int, Int) (Int, Int)
firstSecondExample = first (arr (+ 1)) >>> second (arr (* 2))

firstSecondComplex :: Arrow a => a (String, String) (Int, Bool)
firstSecondComplex = first (arr length) >>> second (arr null)

-- second g >>> first f ==> f *** g
secondFirstExample :: Arrow a => a (Int, Int) (Int, Int)
secondFirstExample = second (arr (* 2)) >>> first (arr (+ 1))

secondFirstComplex :: Arrow a => a (String, Bool) (Int, String)
secondFirstComplex = second (arr show) >>> first (arr length)

--------------------------------------------------------------------------------
-- first >>> first and second >>> second patterns
--------------------------------------------------------------------------------

-- first f >>> first g ==> first (g . f)
firstFirstExample :: Arrow a => a (Int, String) (Int, String)
firstFirstExample = first (arr (+ 1)) >>> first (arr (* 2))

firstFirstChain :: Arrow a => a (String, Int) (Int, Int)
firstFirstChain = first (arr words) >>> first (arr length)

-- second f >>> second g ==> second (g . f)
secondSecondExample :: Arrow a => a (String, Int) (String, Int)
secondSecondExample = second (arr (+ 1)) >>> second (arr (* 2))

secondSecondChain :: Arrow a => a (Int, String) (Int, Int)
secondSecondChain = second (arr words) >>> second (arr length)

--------------------------------------------------------------------------------
-- Star (***) patterns
--------------------------------------------------------------------------------

-- id *** id ==> id
starIdExample :: Arrow a => a (Int, Int) (Int, Int)
starIdExample = id *** id

starIdComplex :: Arrow a => a (String, Bool) (String, Bool)
starIdComplex = arr id *** arr id

-- Mixed star patterns
starMixed1 :: Arrow a => a (Int, Int) (Int, Int)
starMixed1 = arr (+ 1) *** id

starMixed2 :: Arrow a => a (Int, Int) (Int, Int)
starMixed2 = id *** arr (* 2)

--------------------------------------------------------------------------------
-- Ampersand (&&&) patterns
--------------------------------------------------------------------------------

-- id &&& id ==> \x -> (x, x)
ampersandIdExample :: Arrow a => a Int (Int, Int)
ampersandIdExample = id &&& id

ampersandIdArr :: Arrow a => a String (String, String)
ampersandIdArr = arr id &&& arr id

-- Regular fanout patterns
ampersandExample :: Arrow a => a Int (Int, Int)
ampersandExample = arr (+ 1) &&& arr (* 2)

ampersandComplex :: Arrow a => a String (Int, Bool)
ampersandComplex = arr length &&& arr null

--------------------------------------------------------------------------------
-- Fanout simplifications
--------------------------------------------------------------------------------

-- f &&& g >>> arr fst ==> f
fanoutFstExample :: Arrow a => a Int Int
fanoutFstExample = arr (+ 1) &&& arr (* 2) >>> arr fst

fanoutFstComplex :: Arrow a => a String Int
fanoutFstComplex = arr length &&& arr reverse >>> arr fst

-- f &&& g >>> arr snd ==> g
fanoutSndExample :: Arrow a => a Int Int
fanoutSndExample = arr (+ 1) &&& arr (* 2) >>> arr snd

fanoutSndComplex :: Arrow a => a String String
fanoutSndComplex = arr reverse &&& arr (drop 1) >>> arr snd

--------------------------------------------------------------------------------
-- first/second with arr fst/snd
--------------------------------------------------------------------------------

-- first f >>> arr fst ==> f >>> arr fst
firstArrFstExample :: Arrow a => a (Int, String) Int
firstArrFstExample = first (arr (+ 1)) >>> arr fst

firstArrFstChain :: Arrow a => a (String, Bool) Int
firstArrFstChain = first (arr length) >>> arr fst

-- second f >>> arr snd ==> f >>> arr snd
secondArrSndExample :: Arrow a => a (String, Int) Int
secondArrSndExample = second (arr (+ 1)) >>> arr snd

secondArrSndChain :: Arrow a => a (Bool, String) Int
secondArrSndChain = second (arr length) >>> arr snd

--------------------------------------------------------------------------------
-- returnA patterns
--------------------------------------------------------------------------------

-- f >>> returnA ==> f
composeReturnAExample :: Arrow a => a Int Int
composeReturnAExample = arr (+ 1) >>> returnA

composeReturnAChain :: Arrow a => a String String
composeReturnAChain = arr reverse >>> arr (drop 1) >>> returnA

composeReturnAFirst :: Arrow a => a (Int, Int) (Int, Int)
composeReturnAFirst = first (arr (+ 1)) >>> returnA

--------------------------------------------------------------------------------
-- arr fst and arr snd patterns (informational)
--------------------------------------------------------------------------------

arrFstExample :: Arrow a => a (Int, String) Int
arrFstExample = arr fst

arrSndExample :: Arrow a => a (Int, String) String
arrSndExample = arr snd

arrFstComposed :: Arrow a => a (String, Bool) Int
arrFstComposed = arr fst >>> arr length

arrSndComposed :: Arrow a => a (Bool, String) Int
arrSndComposed = arr snd >>> arr length

--------------------------------------------------------------------------------
-- arr (const x) patterns (informational)
--------------------------------------------------------------------------------

arrConstExample :: Arrow a => a Int Int
arrConstExample = arr (const 42)

arrConstComposed :: Arrow a => a String String
arrConstComposed = arr length >>> arr (const "fixed")

arrConstInStar :: Arrow a => a (Int, String) (Int, String)
arrConstInStar = arr (const 0) *** arr (const "empty")

--------------------------------------------------------------------------------
-- Mixed complex patterns
--------------------------------------------------------------------------------

-- Multiple simplification opportunities
complexPattern1 :: Arrow a => a Int (Int, Int)
complexPattern1 = arr id >>> (id &&& id)

complexPattern2 :: Arrow a => a (Int, Int) Int
complexPattern2 = first id >>> second (arr (+ 1)) >>> arr fst

complexPattern3 :: Arrow a => a Int Int
complexPattern3 = arr (+ 1) >>> arr (* 2) >>> returnA

-- Nested first/second
nestedFirstSecond :: Arrow a => a ((Int, String), Bool) ((Int, String), Bool)
nestedFirstSecond = first (first (arr (+ 1))) >>> second id

-- Star and ampersand mix
starAmpersandMix :: Arrow a => a Int (Int, Int)
starAmpersandMix = (id &&& arr (* 2)) >>> (id *** arr (+ 1))

--------------------------------------------------------------------------------
-- ArrowChoice patterns (informational)
--------------------------------------------------------------------------------

leftExample :: ArrowChoice a => a (Either Int String) (Either Int String)
leftExample = left (arr (+ 1))

rightExample :: ArrowChoice a => a (Either Int String) (Either Int String)
rightExample = right (arr reverse)

plusExample :: ArrowChoice a => a (Either Int String) (Either Bool String)
plusExample = arr even +++ arr reverse

--------------------------------------------------------------------------------
-- Kleisli arrow patterns (informational)
--------------------------------------------------------------------------------

kleisliExample :: Kleisli Maybe Int Int
kleisliExample = Kleisli (\x -> if x > 0 then Just (x + 1) else Nothing)

kleisliCompose :: Kleisli Maybe Int Bool
kleisliCompose = Kleisli (\x -> Just (x * 2)) >>> Kleisli (\x -> Just (even x))

--------------------------------------------------------------------------------
-- ArrowLoop patterns (informational)
--------------------------------------------------------------------------------

-- Note: ArrowLoop is advanced - be careful with strictness
loopExample :: ArrowLoop a => a (Int, Int) (Int, Int)
loopExample = loop (arr (\((x, y), z) -> ((x + z, y + z), z + 1)))

--------------------------------------------------------------------------------
-- Helper functions for arrow examples
--------------------------------------------------------------------------------

processInt :: Arrow a => a Int Int
processInt = arr (+ 1) >>> arr (* 2)

processPair :: Arrow a => a (Int, Int) (Int, Int)
processPair = arr (+ 1) *** arr (* 2)

processFanout :: Arrow a => a Int (Int, Int)
processFanout = arr (+ 1) &&& arr (* 2)
