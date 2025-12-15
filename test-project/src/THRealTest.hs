{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

-- | REAL test for TH-aware unused detection
--
-- This module tests the actual Weeder caveat:
-- - onlyCalledFromTH is NEVER called directly in source code
-- - It is ONLY called from TH-generated code via $(generateCall)
-- - Argus should NOT flag it as unused because HIE files contain the TH-expanded reference
module THRealTest
  ( -- * The TH splice that generates the call
    runGenerated

    -- * Function that appears "unused" in source but is used via TH
    -- DO NOT call this directly - it tests TH detection
  , onlyCalledFromTH

    -- * Actually unused function (for comparison)
  , reallyUnused
  ) where

import THRealTest.TH (generateCall)

-- | This function is NEVER called directly in source code.
-- It should appear in HIE files because $(generateCall) expands to code that calls it.
--
-- If Argus flags this as unused, the TH detection is BROKEN.
onlyCalledFromTH :: Int -> Int
onlyCalledFromTH x = x * 2 + 42

-- | This function IS genuinely unused - never called anywhere.
-- Argus SHOULD flag this as unused.
reallyUnused :: Int -> Int
reallyUnused x = x - 1

-- | This function uses the TH splice.
-- When TH expands, it becomes: runGenerated = onlyCalledFromTH 10
-- The HIE file will show 'onlyCalledFromTH' as being referenced here.
runGenerated :: Int
runGenerated = $(generateCall)
