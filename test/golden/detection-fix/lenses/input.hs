{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}

module LensesExamples where

import Control.Lens
import Data.Text (Text)
import qualified Data.Text as T

--------------------------------------------------------------------------------
-- Manual getter/setter instead of lens
--------------------------------------------------------------------------------

data Person = Person
  { _personName :: Text
  , _personAge  :: Int
  }

-- Should suggest: use lens instead of manual getter
getPersonName :: Person -> Text
getPersonName p = _personName p

-- Should suggest: use lens instead of manual setter
setPersonName :: Text -> Person -> Person
setPersonName n p = p { _personName = n }

-- Good: using lens
makeLenses ''Person

personNameLens :: Lens' Person Text
personNameLens = lens _personName (\p n -> p { _personName = n })

--------------------------------------------------------------------------------
-- Nested record access
--------------------------------------------------------------------------------

data Address = Address
  { _addressStreet :: Text
  , _addressCity   :: Text
  }

data Company = Company
  { _companyName    :: Text
  , _companyAddress :: Address
  }

makeLenses ''Address
makeLenses ''Company

-- Should suggest: use lens composition
getCompanyCity :: Company -> Text
getCompanyCity c = _addressCity (_companyAddress c)

-- Good: lens composition
getCompanyCityLens :: Company -> Text
getCompanyCityLens c = c ^. companyAddress . addressCity

--------------------------------------------------------------------------------
-- Repeated field access
--------------------------------------------------------------------------------

-- Should suggest: use lens for repeated access
updatePerson :: Person -> Person
updatePerson p =
  let name = _personName p
      age = _personAge p
      newName = T.toUpper name
  in p { _personName = newName, _personAge = age + 1 }

-- Good: using lens operators
updatePersonLens :: Person -> Person
updatePersonLens = (personName %~ T.toUpper) . (personAge +~ 1)

--------------------------------------------------------------------------------
-- Prefer over instead of fmap with lens
--------------------------------------------------------------------------------

-- Should suggest: use 'over' instead of manual fmap
mapPersonNames :: [Person] -> [Person]
mapPersonNames ps = fmap (\p -> p { _personName = T.toUpper (_personName p) }) ps

-- Good: using over
mapPersonNamesLens :: [Person] -> [Person]
mapPersonNamesLens = over (mapped . personName) T.toUpper

--------------------------------------------------------------------------------
-- Use preview for Maybe lens
--------------------------------------------------------------------------------

data Config = Config
  { _configHost :: Maybe Text
  , _configPort :: Int
  }

makeLenses ''Config

-- Should suggest: use preview/^? instead of manual Maybe handling
getConfigHost :: Config -> Maybe Text
getConfigHost c = _configHost c

-- Good: using preview
getConfigHostLens :: Config -> Maybe Text
getConfigHostLens c = c ^? configHost . _Just

--------------------------------------------------------------------------------
-- Use at for Map access
--------------------------------------------------------------------------------

import qualified Data.Map as M

-- Should suggest: use 'at' lens for Map
lookupMap :: Ord k => k -> M.Map k v -> Maybe v
lookupMap k m = M.lookup k m

-- Good: using at
lookupMapLens :: Ord k => k -> M.Map k v -> Maybe v
lookupMapLens k m = m ^. at k

--------------------------------------------------------------------------------
-- Use ix for indexed access
--------------------------------------------------------------------------------

-- Should suggest: use 'ix' for safe indexed access
getListItem :: Int -> [a] -> Maybe a
getListItem i xs = if i < length xs then Just (xs !! i) else Nothing

-- Good: using ix
getListItemLens :: Int -> [a] -> Maybe a
getListItemLens i xs = xs ^? ix i

--------------------------------------------------------------------------------
-- Lens law violations
--------------------------------------------------------------------------------

-- Should warn: lens law violation (get-put)
badLens :: Lens' Person Text
badLens = lens (const "always this") (\p n -> p { _personName = n })

-- Good: follows lens laws
goodLens :: Lens' Person Text
goodLens = lens _personName (\p n -> p { _personName = n })

--------------------------------------------------------------------------------
-- Use makeLenses for boilerplate
--------------------------------------------------------------------------------

data Settings = Settings
  { _settingsTimeout  :: Int
  , _settingsRetries  :: Int
  , _settingsVerbose  :: Bool
  }

-- Should suggest: use makeLenses instead of manual definitions
settingsTimeout :: Lens' Settings Int
settingsTimeout = lens _settingsTimeout (\s t -> s { _settingsTimeout = t })

settingsRetries :: Lens' Settings Int
settingsRetries = lens _settingsRetries (\s r -> s { _settingsRetries = r })

settingsVerbose :: Lens' Settings Bool
settingsVerbose = lens _settingsVerbose (\s v -> s { _settingsVerbose = v })

-- Good: use makeLenses
-- makeLenses ''Settings

--------------------------------------------------------------------------------
-- Prism for sum types
--------------------------------------------------------------------------------

data Result a
  = Success a
  | Failure Text

-- Should suggest: use makePrisms for sum types
isSuccess :: Result a -> Bool
isSuccess (Success _) = True
isSuccess _ = False

getSuccess :: Result a -> Maybe a
getSuccess (Success a) = Just a
getSuccess _ = Nothing

-- Good: using prisms
makePrisms ''Result

isSuccessPrism :: Result a -> Bool
isSuccessPrism r = has _Success r
