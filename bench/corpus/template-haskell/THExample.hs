{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

-- | Module with Template Haskell usage for testing TH detection
module THExample where

import Language.Haskell.TH
import Language.Haskell.TH.Quote

-- | Example splice (would use TH)
-- Note: This file demonstrates TH patterns but doesn't actually compile
-- without proper TH infrastructure

-- | Generate getter functions for a record
-- generateGetter :: Name -> Q [Dec]
-- generateGetter name = do
--   info <- reify name
--   -- Generate getter implementations
--   pure []

-- | Quasiquoter example structure
exampleQuoter :: QuasiQuoter
exampleQuoter = QuasiQuoter
  { quoteExp = \_ -> pure (LitE (StringL "example"))
  , quotePat = \_ -> pure WildP
  , quoteType = \_ -> pure (ConT ''String)
  , quoteDec = \_ -> pure []
  }

-- | Example of what TH-generated code might look like
data Person = Person
  { personName :: String
  , personAge :: Int
  }

-- | Manual getter (what TH would generate)
getPersonName :: Person -> String
getPersonName = personName

-- | Manual getter (what TH would generate)
getPersonAge :: Person -> Int
getPersonAge = personAge

-- | Example function using head (partial function in TH context)
thRelatedFunc :: [a] -> a
thRelatedFunc = head

-- | Example of quoted name usage pattern
-- exampleQuotedName :: Name
-- exampleQuotedName = 'thRelatedFunc

-- | Another TH-style pattern
-- exampleTypeName :: Name
-- exampleTypeName = ''Person
