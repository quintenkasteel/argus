{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}

module RecordPatterns where

import GHC.Generics (Generic)
import Data.Coerce (coerce)

--------------------------------------------------------------------------------
-- Basic Record Types
--------------------------------------------------------------------------------

-- Person record with multiple fields
data Person = Person
  { personName :: String
  , personAge :: Int
  , personEmail :: String
  , personPhone :: String
  }
  deriving (Show, Eq)

-- Address with strict fields
data Address = Address
  { street :: !String
  , city :: !String
  , zipCode :: !String
  }
  deriving (Show, Eq)

-- Config with UNPACK pragma
data Config = Config
  { configPort :: {-# UNPACK #-} !Int
  , configHost :: String
  , configDebug :: !Bool
  }
  deriving stock (Show, Eq)

-- User with different deriving strategies
data User = User
  { userName :: String
  , userId :: Int
  }
  deriving (Show, Eq, Generic)

-- Account with deriving newtype
newtype AccountId = AccountId Int
  deriving newtype (Show, Eq, Num)

-- Wrapper with deriving via
newtype Age = Age Int
  deriving (Show, Eq, Num) via Int

-- Newtype with record field
newtype Email = Email { getEmail :: String }
  deriving stock (Show, Eq)

--------------------------------------------------------------------------------
-- Record Pattern Matching - NamedFieldPuns Opportunities
--------------------------------------------------------------------------------

-- Should use NamedFieldPuns: Person { personName = personName }
extractName :: Person -> String
extractName Person { personName = personName } = personName

-- Should use NamedFieldPuns: Person { personAge = personAge }
extractAge :: Person -> Int
extractAge Person { personAge = personAge } = personAge

-- Should use NamedFieldPuns for single field
getStreet :: Address -> String
getStreet Address { street = street } = street

-- Should use NamedFieldPuns for single field
getCity :: Address -> String
getCity Address { city = city } = city

--------------------------------------------------------------------------------
-- Record Pattern Matching - RecordWildCards Opportunities
--------------------------------------------------------------------------------

-- Should use RecordWildCards: all fields bound with same names
formatPerson :: Person -> String
formatPerson Person { personName = personName, personAge = personAge, personEmail = personEmail, personPhone = personPhone } =
  personName ++ " (" ++ show personAge ++ ") - " ++ personEmail ++ " / " ++ personPhone

-- Should use RecordWildCards: multiple fields
formatAddress :: Address -> String
formatAddress Address { street = street, city = city, zipCode = zipCode } =
  street ++ ", " ++ city ++ " " ++ zipCode

-- Should use RecordWildCards: binding all fields
showUser :: User -> String
showUser User { userName = userName, userId = userId } =
  userName ++ " [ID: " ++ show userId ++ "]"

--------------------------------------------------------------------------------
-- Record Updates - Could Use Lenses
--------------------------------------------------------------------------------

-- Record update that modifies field based on current value
incrementAge :: Person -> Person
incrementAge p = p { personAge = personAge p + 1 }

-- Record update with function application
updateEmail :: (String -> String) -> Person -> Person
updateEmail f p = p { personEmail = f (personEmail p) }

-- Nested record update pattern
updatePort :: (Int -> Int) -> Config -> Config
updatePort f cfg = cfg { configPort = f (configPort cfg) }

-- Multiple field update with modifications
updatePerson :: String -> Person -> Person
updatePerson newName p = p { personName = newName, personAge = personAge p + 1 }

--------------------------------------------------------------------------------
-- Partial Record Fields (Dangerous Pattern)
--------------------------------------------------------------------------------

-- Partial record field - 'score' only in StudentRecord
data UserRecord
  = StudentRecord { recordName :: String, score :: Int }
  | TeacherRecord { recordName :: String, subject :: String }
  deriving (Show, Eq)

-- Partial record field - 'balance' only in PremiumAccount
data AccountType
  = FreeAccount { accountName :: String }
  | PremiumAccount { accountName :: String, balance :: Double }
  deriving (Show, Eq)

--------------------------------------------------------------------------------
-- Newtype Patterns
--------------------------------------------------------------------------------

-- Newtype wrapping (could use coerce)
wrapAccountId :: Int -> AccountId
wrapAccountId n = AccountId n

-- Newtype unwrapping (could use coerce)
unwrapAccountId :: AccountId -> Int
unwrapAccountId (AccountId n) = n

-- Newtype wrapping with pattern matching
createAge :: Int -> Age
createAge n = Age n

-- Newtype field accessor (redundant with automatic accessor)
extractEmail :: Email -> String
extractEmail e = getEmail e

-- Manual newtype conversion (could use coerce)
accountIdToInt :: AccountId -> Int
accountIdToInt (AccountId x) = x

-- Manual newtype wrapping in let binding
processAccountId :: Int -> String
processAccountId n =
  let AccountId aid = AccountId n
  in "Account: " ++ show aid

--------------------------------------------------------------------------------
-- Record Selectors and Field Access
--------------------------------------------------------------------------------

-- Direct record selector usage (may clash without DuplicateRecordFields)
nameSelector :: Person -> String
nameSelector = personName

-- Multiple selectors
ageSelector :: Person -> Int
ageSelector = personAge

-- Chained field access (could use OverloadedRecordDot)
getConfigPort :: Config -> Int
getConfigPort cfg = configPort cfg

-- Field access in function (could use OverloadedRecordDot)
isDebugMode :: Config -> Bool
isDebugMode cfg = configDebug cfg

--------------------------------------------------------------------------------
-- HasField Pattern (Modern GHC)
--------------------------------------------------------------------------------

-- Using getField with type application
getFieldName :: User -> String
getFieldName user = userName user

-- Field access that could benefit from HasField
getUserId :: User -> Int
getUserId user = userId user

--------------------------------------------------------------------------------
-- Lens-like Patterns (without actual lens library)
--------------------------------------------------------------------------------

-- Getter pattern (could be view with lenses)
getName :: Person -> String
getName person = personName person

-- Setter pattern (could be set with lenses)
setName :: String -> Person -> Person
setName newName person = person { personName = newName }

-- Modifier pattern (could be over with lenses)
modifyAge :: (Int -> Int) -> Person -> Person
modifyAge f person = person { personAge = f (personAge person) }

-- Nested access pattern
getDebugFlag :: Config -> Bool
getDebugFlag config = configDebug config

--------------------------------------------------------------------------------
-- Record Construction with Punning Opportunities
--------------------------------------------------------------------------------

-- Constructor with punning opportunity
makePerson :: String -> Int -> String -> String -> Person
makePerson personName personAge personEmail personPhone =
  Person { personName = personName, personAge = personAge, personEmail = personEmail, personPhone = personPhone }

-- Constructor with single field punning
makeAddress :: String -> String -> String -> Address
makeAddress street city zipCode =
  Address { street = street, city = city, zipCode = zipCode }

-- Constructor with some punning
makeUser :: String -> Int -> User
makeUser userName userId =
  User { userName = userName, userId = userId }

--------------------------------------------------------------------------------
-- Deriving Strategies (Good Examples)
--------------------------------------------------------------------------------

-- Type with multiple deriving clauses (should suggest strategies)
data Product = Product
  { productName :: String
  , productPrice :: Double
  }
  deriving (Show, Eq, Ord)

-- Newtype that could use deriving strategies
newtype ProductId = ProductId Int
  deriving (Show, Eq)

-- Type with Generic (could use anyclass)
data Order = Order
  { orderId :: Int
  , orderItems :: [String]
  }
  deriving (Show, Eq, Generic)

--------------------------------------------------------------------------------
-- Complex Real-World Examples
--------------------------------------------------------------------------------

-- Processing person with field extraction
processPerson :: Person -> String
processPerson Person { personName = personName, personAge = personAge } =
  if personAge >= 18
    then personName ++ " is an adult"
    else personName ++ " is a minor"

-- Updating nested configuration
updateConfig :: Config -> Config
updateConfig cfg = cfg { configPort = configPort cfg + 1000, configDebug = not (configDebug cfg) }

-- Pattern matching with partial extraction
getUserName :: UserRecord -> String
getUserName (StudentRecord { recordName = recordName }) = recordName
getUserName (TeacherRecord { recordName = recordName }) = recordName

-- Newtype conversion chain
convertAccountId :: Int -> String
convertAccountId n =
  let aid = AccountId n
      AccountId val = aid
  in show val