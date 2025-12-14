{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Argus.Rules.Builtin.Lenses
-- Description : Lens and optics rules
-- Copyright   : (c) 2024
-- License     : MIT
-- Stability   : stable
--
-- Rules for lens, optics, getters, setters, and related
-- functional references patterns.
--
-- == Rule Categories
--
-- * __Lens__: Lens operations
-- * __Prism__: Prism and traversal
-- * __Getter__: Getter patterns
-- * __Setter__: Setter and modifier patterns
-- * __TH__: Template Haskell lens generation

module Argus.Rules.Builtin.Lenses
  ( -- * Rule Sets
    opticsRules
  , lensOpRules
  , prismOpRules
  , getterOpRules
  , setterOpRules
  , lensTHRules

    -- * Lens Operation Rules
  , viewOp
  , overOp
  , setOp
  , composeOp
  , atOp
  , ixOp
  , nestedLens
  , lensIdentity

    -- * Prism Rules
  , previewOp
  , reviewOp
  , prismMatch
  , traversalOp
  , eachOp
  , traversedOp
  , filteredOp
  , takingOp

    -- * Getter Rules
  , toGetter
  , viewsOp
  , getterFold
  , toListOfOp
  , previewJust
  , hasOp

    -- * Setter Rules
  , setterModify
  , mappedOp
  , settingOp
  , argumentsOp
  , assignOp
  , modifyingOp

    -- * TH Rules
  , makeLenses
  , makeClassy
  , makePrisms
  , makeFields
  , underscoreFields
  , camelCaseFields

    -- * Rule Count
  , lensRuleCount
  ) where

import Argus.Rules.DSL

--------------------------------------------------------------------------------
-- Rule Sets
--------------------------------------------------------------------------------

-- | All lens-related rules.
opticsRules :: [Rule]
opticsRules = mconcat
  [ lensOpRules
  , prismOpRules
  , getterOpRules
  , setterOpRules
  , lensTHRules
  ]

-- | Total count of lens rules.
lensRuleCount :: Int
lensRuleCount = length opticsRules

--------------------------------------------------------------------------------
-- Lens Operation Rules
--------------------------------------------------------------------------------

-- | Rules for lens operations.
lensOpRules :: [Rule]
lensOpRules =
  [ viewOp
  , overOp
  , setOp
  , composeOp
  , atOp
  , ixOp
  , nestedLens
  , lensIdentity
  ]

-- | view operation.
--
-- @
-- view _field record  -- Get field value
-- @
viewOp :: Rule
viewOp =
  rule "view-op" $
    match ("view _lens _s" ==> "view _lens _s")
    & category Style
    & severity Info
    & message "Using view to access through lens"
    & safetyLevel ManualReview

-- | over operation.
--
-- @
-- over _field (+1) record  -- Modify through lens
-- @
overOp :: Rule
overOp =
  rule "over-op" $
    match ("over _lens _f _s" ==> "over _lens _f _s")
    & category Style
    & severity Info
    & message "Using over to modify through lens"
    & safetyLevel ManualReview

-- | set operation.
--
-- @
-- set _field value record  -- Set through lens
-- @
setOp :: Rule
setOp =
  rule "set-op" $
    match ("set _lens _v _s" ==> "set _lens _v _s")
    & category Style
    & severity Info
    & message "Using set to update through lens"
    & safetyLevel ManualReview

-- | Lens composition with (.).
--
-- @
-- _outer . _inner  -- Compose lenses
-- @
composeOp :: Rule
composeOp =
  rule "lens-compose" $
    matchText "_[a-z]+\\s*\\.\\s*_[a-z]+"
    & category Style
    & severity Info
    & message "Composing lenses - accessing nested structure"
    & safetyLevel ManualReview

-- | at for Map-like structures.
--
-- @
-- at key  -- Lens into Map
-- @
atOp :: Rule
atOp =
  rule "at-op" $
    matchText "\\bat\\s+[\"'][^\"']+[\"']|\\bat\\s+[a-z]+"
    & category Style
    & severity Info
    & message "Using at for Map/Set access"
    & safetyLevel ManualReview

-- | ix for indexed structures.
--
-- @
-- ix n  -- Traversal at index
-- @
ixOp :: Rule
ixOp =
  rule "ix-op" $
    matchText "\\bix\\s+[0-9]+"
    & category Style
    & severity Info
    & message "Using ix for indexed access - returns Traversal"
    & safetyLevel ManualReview

-- | Deeply nested lens.
--
-- @
-- _a . _b . _c . _d  -- Many levels deep
-- @
nestedLens :: Rule
nestedLens =
  rule "nested-lens" $
    matchText "_[a-z]+\\s*\\.\\s*_[a-z]+\\s*\\.\\s*_[a-z]+\\s*\\.\\s*_[a-z]+"
    & category Style
    & severity Suggestion
    & message "Deeply nested lens access - consider intermediate records"

-- | Identity lens.
--
-- @
-- over id f  ==>  f
-- @
lensIdentity :: Rule
lensIdentity =
  rule "lens-identity" $
    match ("over id _f" ==> "_f")
    & category Style
    & severity Suggestion
    & message "over id is just function application"

--------------------------------------------------------------------------------
-- Prism Rules
--------------------------------------------------------------------------------

-- | Rules for prism operations.
prismOpRules :: [Rule]
prismOpRules =
  [ previewOp
  , reviewOp
  , prismMatch
  , traversalOp
  , eachOp
  , traversedOp
  , filteredOp
  , takingOp
  ]

-- | preview operation.
--
-- @
-- preview _Left either  -- Maybe get Left
-- @
previewOp :: Rule
previewOp =
  rule "preview-op" $
    match ("preview _prism _s" ==> "preview _prism _s")
    & category Style
    & severity Info
    & message "Using preview - returns Maybe for prism"
    & safetyLevel ManualReview

-- | review operation.
--
-- @
-- review _Left value  -- Inject into Left
-- @
reviewOp :: Rule
reviewOp =
  rule "review-op" $
    match ("review _prism _v" ==> "review _prism _v")
    & category Style
    & severity Info
    & message "Using review to construct through prism"
    & safetyLevel ManualReview

-- | Prism pattern match.
--
-- @
-- _Left  -- Prism for Left
-- @
prismMatch :: Rule
prismMatch =
  rule "prism-match" $
    matchText "_Left|_Right|_Just|_Nothing|_Cons|_Nil"
    & category Style
    & severity Info
    & message "Using standard prism"
    & safetyLevel ManualReview

-- | Traversal usage.
--
-- @
-- toListOf traversal s  -- All matches
-- @
traversalOp :: Rule
traversalOp =
  rule "traversal-op" $
    match ("toListOf _trav _s" ==> "toListOf _trav _s")
    & category Style
    & severity Info
    & message "Using traversal - may return multiple results"
    & safetyLevel ManualReview

-- | each traversal.
--
-- @
-- each  -- Traverse all elements
-- @
eachOp :: Rule
eachOp =
  rule "each-op" $
    matchText "\\beach\\b"
    & category Style
    & severity Info
    & message "Using each to traverse all elements"
    & safetyLevel ManualReview

-- | traversed for Traversable.
--
-- @
-- traversed  -- Traverse Traversable
-- @
traversedOp :: Rule
traversedOp =
  rule "traversed-op" $
    matchText "\\btraversed\\b"
    & category Style
    & severity Info
    & message "Using traversed for Traversable structures"
    & safetyLevel ManualReview

-- | filtered traversal.
--
-- @
-- filtered predicate  -- Only matching elements
-- @
filteredOp :: Rule
filteredOp =
  rule "filtered-op" $
    match ("filtered _pred" ==> "filtered _pred")
    & category Style
    & severity Info
    & message "Using filtered to select elements"
    & safetyLevel ManualReview

-- | taking/dropping traversal.
--
-- @
-- taking n traversal  -- First n elements
-- @
takingOp :: Rule
takingOp =
  rule "taking-op" $
    matchText "\\btaking\\b|\\bdropping\\b"
    & category Style
    & severity Info
    & message "Using taking/dropping to limit traversal"
    & safetyLevel ManualReview

--------------------------------------------------------------------------------
-- Getter Rules
--------------------------------------------------------------------------------

-- | Rules for getter operations.
getterOpRules :: [Rule]
getterOpRules =
  [ toGetter
  , viewsOp
  , getterFold
  , toListOfOp
  , previewJust
  , hasOp
  ]

-- | to getter.
--
-- @
-- to f  -- Make getter from function
-- @
toGetter :: Rule
toGetter =
  rule "to-getter" $
    matchText "\\bto\\s+[a-z]+"
    & category Style
    & severity Info
    & message "Using to to create getter from function"
    & safetyLevel ManualReview

-- | views operation.
--
-- @
-- views _field f record  -- Apply function to viewed value
-- @
viewsOp :: Rule
viewsOp =
  rule "views-op" $
    matchText "\\bviews\\b"
    & category Style
    & severity Info
    & message "Using views to apply function after viewing"
    & safetyLevel ManualReview

-- | Getter as fold.
--
-- @
-- folded  -- Turn Foldable into Fold
-- @
getterFold :: Rule
getterFold =
  rule "getter-fold" $
    matchText "\\bfolded\\b"
    & category Style
    & severity Info
    & message "Using folded to create Fold from Foldable"
    & safetyLevel ManualReview

-- | toListOf operation.
--
-- @
-- toListOf traversal s  -- Collect all targets
-- @
toListOfOp :: Rule
toListOfOp =
  rule "toListOf-op" $
    matchText "\\btoListOf\\b"
    & category Style
    & severity Info
    & message "Using toListOf to collect traversal targets"
    & safetyLevel ManualReview

-- | preview with fromJust.
--
-- @
-- fromJust (preview prism s)  -- Dangerous!
-- @
previewJust :: Rule
previewJust =
  rule "preview-just" $
    matchText "fromJust.*preview|preview.*fromJust"
    & category Safety
    & severity Warning
    & message "Using fromJust on preview - handle Nothing case instead"

-- | has predicate.
--
-- @
-- has _field record  -- Check if target exists
-- @
hasOp :: Rule
hasOp =
  rule "has-op" $
    matchText "\\bhas\\s+_"
    & category Style
    & severity Info
    & message "Using has to check for target existence"
    & safetyLevel ManualReview

--------------------------------------------------------------------------------
-- Setter Rules
--------------------------------------------------------------------------------

-- | Rules for setter operations.
setterOpRules :: [Rule]
setterOpRules =
  [ setterModify
  , mappedOp
  , settingOp
  , argumentsOp
  , assignOp
  , modifyingOp
  ]

-- | Setter modify pattern.
--
-- @
-- %~ f  -- Modify operator
-- @
setterModify :: Rule
setterModify =
  rule "setter-modify" $
    matchText "%~|\\.\\.~|\\.~"
    & category Style
    & severity Info
    & message "Using lens modifier operator"
    & safetyLevel ManualReview

-- | mapped setter.
--
-- @
-- mapped  -- Functor setter
-- @
mappedOp :: Rule
mappedOp =
  rule "mapped-op" $
    matchText "\\bmapped\\b"
    & category Style
    & severity Info
    & message "Using mapped for Functor modification"
    & safetyLevel ManualReview

-- | setting function.
--
-- @
-- setting f  -- Make setter from function
-- @
settingOp :: Rule
settingOp =
  rule "setting-op" $
    matchText "\\bsetting\\b"
    & category Style
    & severity Info
    & message "Using setting to create Setter"
    & safetyLevel ManualReview

-- | Argument setter.
--
-- @
-- argument  -- Setter for function arguments
-- @
argumentsOp :: Rule
argumentsOp =
  rule "argument-op" $
    matchText "\\bargument\\b"
    & category Style
    & severity Info
    & message "Using argument setter for contravariant position"
    & safetyLevel ManualReview

-- | assign in State.
--
-- @
-- _field .= value  -- Stateful assignment
-- @
assignOp :: Rule
assignOp =
  rule "assign-op" $
    matchText "\\.="
    & category Style
    & severity Info
    & message "Using lens assignment in State"
    & safetyLevel ManualReview

-- | modifying in State.
--
-- @
-- _field %= f  -- Stateful modification
-- @
modifyingOp :: Rule
modifyingOp =
  rule "modifying-op" $
    matchText "%="
    & category Style
    & severity Info
    & message "Using lens modification in State"
    & safetyLevel ManualReview

--------------------------------------------------------------------------------
-- Template Haskell Rules
--------------------------------------------------------------------------------

-- | Rules for lens TH generation.
lensTHRules :: [Rule]
lensTHRules =
  [ makeLenses
  , makeClassy
  , makePrisms
  , makeFields
  , underscoreFields
  , camelCaseFields
  ]

-- | makeLenses TH.
--
-- @
-- makeLenses ''MyType
-- @
makeLenses :: Rule
makeLenses =
  rule "makeLenses" $
    matchText "makeLenses\\s+''[A-Z]"
    & category Style
    & severity Info
    & message "Generating lenses with makeLenses"
    & safetyLevel ManualReview

-- | makeClassy TH.
--
-- @
-- makeClassy ''MyType
-- @
makeClassy :: Rule
makeClassy =
  rule "makeClassy" $
    matchText "makeClassy\\s+''[A-Z]"
    & category Style
    & severity Info
    & message "Generating classy lenses with makeClassy"
    & safetyLevel ManualReview

-- | makePrisms TH.
--
-- @
-- makePrisms ''MySum
-- @
makePrisms :: Rule
makePrisms =
  rule "makePrisms" $
    matchText "makePrisms\\s+''[A-Z]"
    & category Style
    & severity Info
    & message "Generating prisms with makePrisms"
    & safetyLevel ManualReview

-- | makeFields TH.
--
-- @
-- makeFields ''MyRecord
-- @
makeFields :: Rule
makeFields =
  rule "makeFields" $
    matchText "makeFields\\s+''[A-Z]"
    & category Style
    & severity Info
    & message "Generating HasField instances with makeFields"
    & safetyLevel ManualReview

-- | Underscore field naming.
--
-- @
-- data T = T { _field :: Int }  -- Underscore prefix
-- @
underscoreFields :: Rule
underscoreFields =
  rule "underscore-fields" $
    matchText "\\{\\s*_[a-z]+\\s*::"
    & category Style
    & severity Info
    & message "Underscore-prefixed field - compatible with makeLenses"
    & safetyLevel ManualReview

-- | CamelCase field naming.
--
-- @
-- data Person = Person { personName :: String }  -- Prefix naming
-- @
camelCaseFields :: Rule
camelCaseFields =
  rule "camelCase-fields" $
    matchText "data [A-Z][a-z]+.*\\{\\s*[a-z]+[A-Z]"
    & category Style
    & severity Info
    & message "CamelCase field naming - compatible with makeFields"
    & safetyLevel ManualReview
