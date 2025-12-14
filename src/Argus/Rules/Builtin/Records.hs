{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Argus.Rules.Builtin.Records
-- Description : Record syntax and data type rules
-- Copyright   : (c) 2024
-- License     : MIT
-- Stability   : stable
--
-- Rules for record syntax, data type definitions, and related patterns.
-- Encourages modern record usage and avoids common pitfalls.
--
-- == Rule Categories
--
-- * __Record Syntax__: Record field access and update
-- * __Lenses__: Lens-related suggestions
-- * __Newtype__: Newtype wrapper patterns
-- * __Data Declaration__: Data type definition rules

module Argus.Rules.Builtin.Records
  ( -- * Rule Sets
    recordRules
  , recordSyntaxRules
  , lensRules
  , newtypeRules
  , dataDeclarationRules

    -- * Record Syntax
  , recordWildcards
  , recordPuns
  , recordUpdate
  , partialRecordField
  , recordSelector
  , duplicateRecordFields
  , hasFieldPattern
  , overloadedRecordDot

    -- * Lenses
  , getterToView
  , setterToSet
  , modifyToOver
  , composeLenses
  , lensFieldNaming
  , makeLensesHint

    -- * Newtype
  , newtypeCoerce
  , newtypeUnwrap
  , newtypeWrap
  , generalizedNewtype
  , newtypeDerivingVia
  , newtypeSingleField

    -- * Data Declaration
  , strictData
  , unpackPragma
  , derivingStrategies
  , stockDeriving
  , anyclassDeriving
  , newtypeDeriving
  , standaloneDerivingHint

    -- * Rule Count
  , recordRuleCount
  ) where

import Argus.Rules.DSL

--------------------------------------------------------------------------------
-- Rule Sets
--------------------------------------------------------------------------------

-- | All record-related rules.
recordRules :: [Rule]
recordRules = mconcat
  [ recordSyntaxRules
  , lensRules
  , newtypeRules
  , dataDeclarationRules
  ]

-- | Total count of record rules.
recordRuleCount :: Int
recordRuleCount = length recordRules

--------------------------------------------------------------------------------
-- Record Syntax Rules
--------------------------------------------------------------------------------

-- | Rules for record syntax usage.
recordSyntaxRules :: [Rule]
recordSyntaxRules =
  [ recordWildcards
  , recordPuns
  , recordUpdate
  , partialRecordField
  , recordSelector
  , duplicateRecordFields
  , hasFieldPattern
  , overloadedRecordDot
  ]

-- | Use RecordWildCards for binding all fields.
--
-- @
-- foo Rec { a = a, b = b, c = c }  ==>  foo Rec{..}
-- @
recordWildcards :: Rule
recordWildcards =
  rule "record-wildcards" $
    matchText "[A-Z][a-zA-Z]+ \\{ [a-z]+ = [a-z]+, [a-z]+ = [a-z]+, [a-z]+ = [a-z]+ \\}"
    & category Style
    & severity Suggestion
    & message "Consider RecordWildCards for binding all fields"
    & note "Requires {-# LANGUAGE RecordWildCards #-}"

-- | Use NamedFieldPuns for single field binding.
--
-- @
-- foo Rec { a = a }  ==>  foo Rec { a }
-- @
recordPuns :: Rule
recordPuns =
  rule "record-puns" $
    matchText "[A-Z][a-zA-Z]+ \\{ [a-z]+ = [a-z]+ \\}"
    & category Style
    & severity Suggestion
    & message "Consider NamedFieldPuns for field punning"
    & note "Requires {-# LANGUAGE NamedFieldPuns #-}"

-- | Record update syntax warning.
--
-- @
-- r { field = f (field r) }  -- Could use lens over
-- @
recordUpdate :: Rule
recordUpdate =
  rule "record-update" $
    matchText "[a-z]+ \\{ [a-z]+ = [a-z]+ \\([a-z]+ [a-z]+\\) \\}"
    & category Style
    & severity Info
    & message "Record update modifying a field - consider lenses"
    & safetyLevel ManualReview

-- | Partial record fields are dangerous.
--
-- @
-- data Foo = Bar { x :: Int } | Baz  -- x is partial!
-- @
partialRecordField :: Rule
partialRecordField =
  rule "partial-record-field" $
    matchText "data [A-Z][a-zA-Z]* = [A-Z][a-zA-Z]* \\{.+\\} \\| [A-Z]"
    & category Safety
    & severity Warning
    & message "Partial record field - not all constructors have this field"
    & note "Accessing this field on other constructors will throw"

-- | Record selector functions can clash.
--
-- @
-- field :: Record -> Int  -- May clash with other records
-- @
recordSelector :: Rule
recordSelector =
  rule "record-selector" $
    matchText "^[a-z][a-zA-Z]+ :: [A-Z][a-zA-Z]+ -> "
    & category Style
    & severity Info
    & message "Record selector may clash with other records"
    & note "Consider DuplicateRecordFields or prefixed field names"
    & safetyLevel ManualReview

-- | Use DuplicateRecordFields for shared field names.
--
-- @
-- {-# LANGUAGE DuplicateRecordFields #-}
-- @
duplicateRecordFields :: Rule
duplicateRecordFields =
  rule "duplicate-record-fields" $
    matchText "DuplicateRecordFields"
    & category Style
    & severity Info
    & message "DuplicateRecordFields enabled - ensure field access is unambiguous"
    & safetyLevel ManualReview

-- | Use HasField for generic field access.
--
-- @
-- getField @"name" record
-- @
hasFieldPattern :: Rule
hasFieldPattern =
  rule "hasField-pattern" $
    matchText "getField @\""
    & category Modernization
    & severity Info
    & message "Using HasField for generic field access"
    & safetyLevel ManualReview

-- | Use OverloadedRecordDot for field access.
--
-- @
-- record.field  -- Modern record dot syntax
-- @
overloadedRecordDot :: Rule
overloadedRecordDot =
  rule "overloaded-record-dot" $
    matchText "field [a-z]+"
    & category Style
    & severity Info
    & message "Consider OverloadedRecordDot for cleaner field access"
    & note "Requires GHC 9.2+ with {-# LANGUAGE OverloadedRecordDot #-}"
    & safetyLevel ManualReview

--------------------------------------------------------------------------------
-- Lens Rules
--------------------------------------------------------------------------------

-- | Rules for lens-style programming.
lensRules :: [Rule]
lensRules =
  [ getterToView
  , setterToSet
  , modifyToOver
  , composeLenses
  , lensFieldNaming
  , makeLensesHint
  ]

-- | Use view instead of getter application.
--
-- @
-- myLens record  ==>  view myLens record
-- @
getterToView :: Rule
getterToView =
  rule "getter-to-view" $
    matchText "\\^\\. [a-z]+"
    & category Style
    & severity Info
    & message "Using view operator (^.) for lens access"
    & safetyLevel ManualReview

-- | Use set instead of manual setting.
--
-- @
-- r { _field = x }  ==>  set field x r
-- @
setterToSet :: Rule
setterToSet =
  rule "setter-to-set" $
    matchText "\\.~ "
    & category Style
    & severity Info
    & message "Using set operator (.~) for lens setting"
    & safetyLevel ManualReview

-- | Use over instead of get-modify-set.
--
-- @
-- r { _field = f (_field r) }  ==>  over field f r
-- @
modifyToOver :: Rule
modifyToOver =
  rule "modify-to-over" $
    matchText "%~ "
    & category Style
    & severity Info
    & message "Using over operator (%~) for lens modification"
    & safetyLevel ManualReview

-- | Compose lenses instead of nesting.
--
-- @
-- view a . view b  ==>  view (b . a)
-- @
composeLenses :: Rule
composeLenses =
  rule "compose-lenses" $
    match ("view _a . view _b" ==> "view (_b . _a)")
    & category Style
    & severity Suggestion
    & message "Compose lenses instead of composing views"

-- | Lens field naming convention.
--
-- @
-- _fieldName  -- Lens naming convention
-- @
lensFieldNaming :: Rule
lensFieldNaming =
  rule "lens-field-naming" $
    matchText "^_[a-z][a-zA-Z]+ ::"
    & category Style
    & severity Info
    & message "Using underscore prefix convention for lens fields"
    & safetyLevel ManualReview

-- | Suggest makeLenses TH.
--
-- @
-- data Record = Record { _field :: Int }
-- -- Consider: makeLenses ''Record
-- @
makeLensesHint :: Rule
makeLensesHint =
  rule "makeLenses-hint" $
    matchText "data [A-Z][a-zA-Z]+ = [A-Z][a-zA-Z]+ \\{ _"
    & category Style
    & severity Info
    & message "Fields with underscore prefix - consider using makeLenses"
    & note "import Control.Lens.TH (makeLenses)"
    & safetyLevel ManualReview

--------------------------------------------------------------------------------
-- Newtype Rules
--------------------------------------------------------------------------------

-- | Rules for newtype usage.
newtypeRules :: [Rule]
newtypeRules =
  [ newtypeCoerce
  , newtypeUnwrap
  , newtypeWrap
  , generalizedNewtype
  , newtypeDerivingVia
  , newtypeSingleField
  ]

-- | Use coerce for newtype conversion.
--
-- @
-- MyNewtype x  -- Consider coerce for zero-cost conversion
-- @
newtypeCoerce :: Rule
newtypeCoerce =
  rule "newtype-coerce" $
    matchText "newtype [A-Z][a-zA-Z]+ = [A-Z][a-zA-Z]+ [A-Z][a-zA-Z]+"
    & category Performance
    & severity Info
    & message "Newtypes can use coerce for zero-cost conversion"
    & note "import Data.Coerce (coerce)"
    & safetyLevel ManualReview

-- | Unwrapping newtype pattern.
--
-- @
-- let MyNewtype x = myNewtype  -- Unwrapping
-- @
newtypeUnwrap :: Rule
newtypeUnwrap =
  rule "newtype-unwrap" $
    matchText "let [A-Z][a-zA-Z]+ [a-z]+ ="
    & category Style
    & severity Info
    & message "Newtype unwrapping - consider coerce or a getter"
    & safetyLevel ManualReview

-- | Wrapping in newtype.
--
-- @
-- MyNewtype value  -- Wrapping in newtype
-- @
newtypeWrap :: Rule
newtypeWrap =
  rule "newtype-wrap" $
    matchText "^[A-Z][a-zA-Z]+ [a-z]+$"
    & category Style
    & severity Info
    & message "Newtype wrapping - consider coerce for multiple conversions"
    & safetyLevel ManualReview

-- | GeneralizedNewtypeDeriving suggestion.
--
-- @
-- newtype Age = Age Int
-- instance Num Age where ...  -- Use GeneralizedNewtypeDeriving
-- @
generalizedNewtype :: Rule
generalizedNewtype =
  rule "generalized-newtype" $
    matchText "instance .+ [A-Z][a-zA-Z]+ where"
    & category Style
    & severity Info
    & message "Consider GeneralizedNewtypeDeriving for newtype instances"
    & note "Requires {-# LANGUAGE GeneralizedNewtypeDeriving #-}"
    & safetyLevel ManualReview

-- | DerivingVia for newtype.
--
-- @
-- newtype Age = Age Int deriving (Num) via Int
-- @
newtypeDerivingVia :: Rule
newtypeDerivingVia =
  rule "newtype-deriving-via" $
    matchText "deriving .+ via "
    & category Modernization
    & severity Info
    & message "Using DerivingVia for newtype instances"
    & safetyLevel ManualReview

-- | Newtype with single field.
--
-- @
-- newtype Wrapper a = Wrapper { unwrap :: a }
-- @
newtypeSingleField :: Rule
newtypeSingleField =
  rule "newtype-single-field" $
    matchText "newtype [A-Z][a-zA-Z]+ [a-z]* = [A-Z][a-zA-Z]+ \\{ [a-z]+"
    & category Style
    & severity Info
    & message "Newtype with record field - provides unwrapper function"
    & safetyLevel ManualReview

--------------------------------------------------------------------------------
-- Data Declaration Rules
--------------------------------------------------------------------------------

-- | Rules for data type declarations.
dataDeclarationRules :: [Rule]
dataDeclarationRules =
  [ strictData
  , unpackPragma
  , derivingStrategies
  , stockDeriving
  , anyclassDeriving
  , newtypeDeriving
  , standaloneDerivingHint
  ]

-- | Strict data fields.
--
-- @
-- data Foo = Foo !Int !String  -- Strict fields
-- @
strictData :: Rule
strictData =
  rule "strict-data" $
    matchText "data [A-Z][a-zA-Z]+ = [A-Z][a-zA-Z]+ !"
    & category Performance
    & severity Info
    & message "Using strict fields - good for avoiding space leaks"
    & safetyLevel ManualReview

-- | UNPACK pragma for primitives.
--
-- @
-- data Foo = Foo {-# UNPACK #-} !Int
-- @
unpackPragma :: Rule
unpackPragma =
  rule "unpack-pragma" $
    matchText "\\{-# UNPACK #-\\}"
    & category Performance
    & severity Info
    & message "Using UNPACK pragma for primitive unpacking"
    & safetyLevel ManualReview

-- | Use DerivingStrategies.
--
-- @
-- deriving stock (Show, Eq)
-- @
derivingStrategies :: Rule
derivingStrategies =
  rule "deriving-strategies" $
    matchText "deriving \\("
    & category Style
    & severity Info
    & message "Consider DerivingStrategies for explicit deriving method"
    & note "Use 'deriving stock', 'deriving anyclass', 'deriving newtype', or 'deriving via'"
    & safetyLevel ManualReview

-- | Stock deriving.
--
-- @
-- deriving stock (Show, Eq, Ord)
-- @
stockDeriving :: Rule
stockDeriving =
  rule "stock-deriving" $
    matchText "deriving stock"
    & category Style
    & severity Info
    & message "Using stock deriving strategy"
    & safetyLevel ManualReview

-- | Anyclass deriving.
--
-- @
-- deriving anyclass (ToJSON, FromJSON)
-- @
anyclassDeriving :: Rule
anyclassDeriving =
  rule "anyclass-deriving" $
    matchText "deriving anyclass"
    & category Style
    & severity Info
    & message "Using anyclass deriving - requires default methods"
    & safetyLevel ManualReview

-- | Newtype deriving.
--
-- @
-- deriving newtype (Num, Show)
-- @
newtypeDeriving :: Rule
newtypeDeriving =
  rule "newtype-deriving" $
    matchText "deriving newtype"
    & category Style
    & severity Info
    & message "Using newtype deriving strategy"
    & safetyLevel ManualReview

-- | Standalone deriving hint.
--
-- @
-- deriving instance Show Foo
-- @
standaloneDerivingHint :: Rule
standaloneDerivingHint =
  rule "standalone-deriving" $
    matchText "^deriving instance "
    & category Style
    & severity Info
    & message "Using standalone deriving"
    & note "Useful for GADTs or when deriving clause won't work"
    & safetyLevel ManualReview
