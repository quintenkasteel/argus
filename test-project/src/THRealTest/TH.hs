{-# LANGUAGE TemplateHaskell #-}

-- | TH splice definitions for THRealTest
module THRealTest.TH
  ( generateCall
  ) where

import Language.Haskell.TH

-- | TH function that generates code calling 'onlyCalledFromTH'
-- The generated code looks like: onlyCalledFromTH 10
generateCall :: Q Exp
generateCall = do
  -- Generate: onlyCalledFromTH 10
  let funcName = mkName "onlyCalledFromTH"
  pure $ AppE (VarE funcName) (LitE (IntegerL 10))
