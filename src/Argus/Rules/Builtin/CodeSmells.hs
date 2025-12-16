{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Argus.Rules.Builtin.CodeSmells
-- Description : Code smell detection for architectural quality
-- Copyright   : (c) 2024
-- License     : MIT
-- Stability   : stable
--
-- Rules for detecting code smells that indicate potential design problems.
-- These rules catch issues that aren't bugs but suggest refactoring
-- opportunities for better maintainability.
--
-- == Rule Categories
--
-- * __Bloaters__: Long functions, parameter lists, complex expressions
-- * __Couplers__: Feature envy, inappropriate intimacy
-- * __Dispensables__: Dead code, redundant comments, speculative generality
-- * __Abusers__: Typeclass abuse, primitive obsession
--
-- == References
--
-- * <https://refactoring.guru/refactoring/smells Code Smells>
-- * <https://martinfowler.com/bliki/CodeSmell.html Martin Fowler on Code Smells>

module Argus.Rules.Builtin.CodeSmells
  ( -- * Rule Sets
    codeSmellRules
  , bloaterRules
  , couplerRules
  , dispensableRules
  , abuserRules

    -- * Bloaters
  , longFunction
  , longParameterList
  , deepNesting
  , largeCase
  , complexGuards
  , godModule
  , primitiveObsession

    -- * Couplers
  , featureEnvy
  , messageChain
  , middleMan
  , excessiveImports
  , circularDependency

    -- * Dispensables
  , deadCode
  , redundantComment
  , speculativeGenerality
  , lazyClass
  , duplicateCode

    -- * Abusers
  , typeclassAbuse
  , partialInstance
  , orphanInstance
  , overlappingInstances
  , undecidableInstances

    -- * Rule Count
  , codeSmellRuleCount
  ) where

import Argus.Rules.DSL hiding (Complexity)
import Argus.Rules.Types (Category(Complexity))

--------------------------------------------------------------------------------
-- Rule Sets
--------------------------------------------------------------------------------

-- | All code smell detection rules.
codeSmellRules :: [Rule]
codeSmellRules = mconcat
  [ bloaterRules
  , couplerRules
  , dispensableRules
  , abuserRules
  ]

-- | Total count of code smell rules.
codeSmellRuleCount :: Int
codeSmellRuleCount = length codeSmellRules

--------------------------------------------------------------------------------
-- Bloater Rules
--------------------------------------------------------------------------------

-- | Rules for detecting bloated code structures.
bloaterRules :: [Rule]
bloaterRules =
  [ longFunction
  , longParameterList
  , deepNesting
  , largeCase
  , complexGuards
  , godModule
  , primitiveObsession
  ]

-- | Function is too long (>50 lines).
--
-- @
-- foo = do
--   -- 100 lines of code
--   ...
-- @
longFunction :: Rule
longFunction =
  rule "long-function" $
    matchText "^[a-z][a-zA-Z0-9_']* "
    & category Complexity
    & severity Warning
    & message "Function may be too long - consider splitting"
    & note "Functions over 50 lines are harder to understand and test"
    & safetyLevel ManualReview

-- | Too many parameters (>5).
--
-- @
-- foo a b c d e f g = ...  -- Too many parameters!
-- @
longParameterList :: Rule
longParameterList =
  rule "long-parameter-list" $
    matchText "^[a-z][a-zA-Z0-9_']* ([a-z_]+ ){5,}"
    & category Complexity
    & severity Warning
    & message "Too many parameters - consider using a record type"
    & note "Functions with >5 parameters are hard to call correctly"

-- | Deeply nested expressions (>4 levels).
--
-- @
-- foo = case x of
--   A -> case y of
--     B -> case z of
--       C -> case w of  -- Too deep!
-- @
deepNesting :: Rule
deepNesting =
  rule "deep-nesting" $
    matchText "case .+ of.+case .+ of.+case .+ of.+case .+ of"
    & category Complexity
    & severity Warning
    & message "Deeply nested control flow - consider refactoring"
    & note "Deep nesting (>4 levels) hurts readability"

-- | Case expression with too many branches (>10).
--
-- @
-- foo x = case x of
--   A -> ...
--   B -> ...
--   -- ... 15 more cases
-- @
largeCase :: Rule
largeCase =
  rule "large-case" $
    matchText "case .+ of"
    & category Complexity
    & severity Info
    & message "Large case expression - consider using a lookup table or typeclass"
    & note "Case expressions with >10 branches may indicate missing abstraction"
    & safetyLevel ManualReview

-- | Complex guard conditions.
--
-- @
-- foo x
--   | cond1 x && cond2 x && cond3 x && cond4 x = ...
-- @
complexGuards :: Rule
complexGuards =
  rule "complex-guards" $
    matchText "\\| .+ && .+ && .+ &&"
    & category Complexity
    & severity Warning
    & message "Complex guard condition - consider extracting a named predicate"
    & note "Guards with >3 conditions should be named for clarity"

-- | Module is too large (god module).
--
-- @
-- module Everything where  -- 2000+ lines
-- @
godModule :: Rule
godModule =
  rule "god-module" $
    matchText "^module"
    & category Complexity
    & severity Warning
    & message "Module may be too large - consider splitting"
    & note "Modules over 500 lines often have too many responsibilities"
    & safetyLevel ManualReview

-- | Using primitives instead of domain types.
--
-- @
-- sendEmail :: String -> String -> String -> IO ()  -- email, subject, body
-- -- Should be: sendEmail :: Email -> Subject -> Body -> IO ()
-- @
primitiveObsession :: Rule
primitiveObsession =
  rule "primitive-obsession" $
    matchText ":: String -> String -> String ->"
    & category Style
    & severity Info
    & message "Multiple String parameters - consider domain types"
    & note "Three+ String/Int parameters often indicate missing newtypes"
    & safetyLevel ManualReview

--------------------------------------------------------------------------------
-- Coupler Rules
--------------------------------------------------------------------------------

-- | Rules for detecting excessive coupling.
couplerRules :: [Rule]
couplerRules =
  [ featureEnvy
  , messageChain
  , middleMan
  , excessiveImports
  , circularDependency
  ]

-- | Function uses another module's data excessively.
--
-- @
-- import qualified Data.Map as M
-- foo x = M.size (M.filter p (M.map f (M.keys (userData x))))
-- -- Should be a function in the module defining userData
-- @
featureEnvy :: Rule
featureEnvy =
  rule "feature-envy" $
    matchText "[A-Z]\\.[a-z]+ \\([A-Z]\\.[a-z]+ \\([A-Z]\\.[a-z]+"
    & category Complexity
    & severity Info
    & message "Feature envy - function uses another module's data excessively"
    & note "Consider moving this logic to the module that owns the data"
    & safetyLevel ManualReview

-- | Long chain of field/method access.
--
-- @
-- foo = a.b.c.d.e  -- Too many dots
-- @
messageChain :: Rule
messageChain =
  rule "message-chain" $
    matchText "\\.[a-z]+\\.[a-z]+\\.[a-z]+\\.[a-z]+"
    & category Complexity
    & severity Warning
    & message "Long accessor chain - violates Law of Demeter"
    & note "Consider adding a helper function to reduce coupling"

-- | Function just delegates to another.
--
-- @
-- foo x = bar x  -- Middle man, just call bar directly
-- @
middleMan :: Rule
middleMan =
  rule "middle-man" $
    match (("_f _x = _g _x" ==> "_g") `where_` isVariable "_x")
    & category Redundant
    & severity Info
    & message "Middle man - function just delegates to another"
    & note "Consider removing this wrapper or adding meaningful logic"

-- | Too many imports from one module.
--
-- @
-- import Data.Map (Map, empty, insert, lookup, delete, filter, map, ...)
-- @
excessiveImports :: Rule
excessiveImports =
  rule "excessive-imports" $
    matchText "import .+ \\(.+,.+,.+,.+,.+,.+,.+,.+\\)"
    & category Style
    & severity Info
    & message "Many imports from single module - consider qualified import"
    & note "More than 8 imports from one module suggests qualified import"

-- | Circular module dependency indicator.
--
-- @
-- {-# SOURCE #-} import  -- Indicates circular dependency
-- @
circularDependency :: Rule
circularDependency =
  rule "circular-dependency" $
    matchText "\\{-# SOURCE #-\\}"
    & category Complexity
    & severity Warning
    & message "Circular module dependency detected"
    & note "SOURCE pragma indicates circular dependency - consider refactoring"

--------------------------------------------------------------------------------
-- Dispensable Rules
--------------------------------------------------------------------------------

-- | Rules for detecting dispensable code elements.
dispensableRules :: [Rule]
dispensableRules =
  [ deadCode
  , redundantComment
  , speculativeGenerality
  , lazyClass
  , duplicateCode
  ]

-- | Dead code that's never used.
--
-- @
-- _unused x = x + 1  -- Leading underscore suggests unused
-- @
deadCode :: Rule
deadCode =
  rule "dead-code" $
    matchText "^_[a-z][a-zA-Z0-9_']* "
    & category Redundant
    & severity Warning
    & message "Potentially dead code (underscore prefix)"
    & note "Leading underscore in binding name suggests it may be unused"

-- | Redundant comment stating the obvious.
--
-- @
-- -- Increments x
-- increment x = x + 1
-- @
redundantComment :: Rule
redundantComment =
  rule "redundant-comment" $
    matchText "-- [A-Z][a-z]+ [a-z]+$"
    & category Documentation
    & severity Info
    & message "Comment may be redundant if it just restates the code"
    & note "Comments should explain why, not what"
    & safetyLevel ManualReview

-- | Over-generalized code that's only used once.
--
-- @
-- -- Type parameter \'a\' but only ever used with Int
-- foo :: forall a. Num a => a -> a
-- foo x = x + 1  -- Only called as foo (1 :: Int)
-- @
speculativeGenerality :: Rule
speculativeGenerality =
  rule "speculative-generality" $
    matchText "forall [a-z]+\\. .+ => [a-z]+ -> [a-z]+$"
    & category Style
    & severity Info
    & message "Possibly over-generalized - is polymorphism needed?"
    & note "Consider concrete types if only used with one type"
    & safetyLevel ManualReview

-- | Module/type with very little functionality.
--
-- @
-- newtype Wrapper = Wrapper Int
-- unwrap (Wrapper x) = x
-- -- That's it, no other functions
-- @
lazyClass :: Rule
lazyClass =
  rule "lazy-class" $
    matchText "^newtype [A-Z][a-zA-Z]* = [A-Z][a-zA-Z]* [A-Z]"
    & category Style
    & severity Info
    & message "Small newtype - ensure it provides meaningful abstraction"
    & note "Newtypes should provide more than just wrapping"
    & safetyLevel ManualReview

-- | Duplicate code patterns.
--
-- @
-- foo = someComplexExpression ...
-- bar = someComplexExpression ...  -- Same as foo!
-- @
duplicateCode :: Rule
duplicateCode =
  rule "duplicate-code" $
    matchText "= [a-z]+ [a-z]+ [a-z]+ [a-z]+ [a-z]+"
    & category Redundant
    & severity Info
    & message "Potential code duplication detected"
    & note "Similar expressions may indicate opportunity for extraction"
    & safetyLevel ManualReview

--------------------------------------------------------------------------------
-- Abuser Rules
--------------------------------------------------------------------------------

-- | Rules for detecting abstraction abuse.
abuserRules :: [Rule]
abuserRules =
  [ typeclassAbuse
  , partialInstance
  , orphanInstance
  , overlappingInstances
  , undecidableInstances
  ]

-- | Typeclass with too many methods.
--
-- @
-- class HasEverything a where
--   method1 :: ...
--   method2 :: ...
--   -- ... 20 more methods
-- @
typeclassAbuse :: Rule
typeclassAbuse =
  rule "typeclass-abuse" $
    matchText "^class .+ where"
    & category Complexity
    & severity Info
    & message "Large typeclass - consider splitting into smaller classes"
    & note "Typeclasses with many methods may violate single responsibility"
    & safetyLevel ManualReview

-- | Instance with undefined methods.
--
-- @
-- instance Foo Bar where
--   method1 = undefined  -- Partial implementation!
-- @
partialInstance :: Rule
partialInstance =
  rule "partial-instance" $
    matchText "= undefined$"
    & category Safety
    & severity Error
    & message "Partial typeclass instance - undefined method"
    & note "Instance methods should never be undefined"

-- | Orphan instance definition.
--
-- @
-- -- In module C:
-- instance A.Foo B.Bar where ...  -- Neither A.Foo nor B.Bar defined here
-- @
orphanInstance :: Rule
orphanInstance =
  rule "orphan-instance" $
    matchText "\\{-# OPTIONS_GHC -Wno-orphans #-\\}"
    & category Style
    & severity Warning
    & message "Orphan instance detected (orphan warning disabled)"
    & note "Orphan instances can cause coherence problems"

-- | OverlappingInstances is often a design smell.
--
-- @
-- {-# LANGUAGE OverlappingInstances #-}
-- @
overlappingInstances :: Rule
overlappingInstances =
  rule "overlapping-instances" $
    matchText "OverlappingInstances"
    & category Style
    & severity Warning
    & message "OverlappingInstances may indicate design issue"
    & note "Consider redesigning to avoid instance overlap"

-- | UndecidableInstances may cause infinite loops.
--
-- @
-- {-# LANGUAGE UndecidableInstances #-}
-- @
undecidableInstances :: Rule
undecidableInstances =
  rule "undecidable-instances" $
    matchText "UndecidableInstances"
    & category Safety
    & severity Warning
    & message "UndecidableInstances can cause compilation to loop"
    & note "Ensure instances terminate or use type family approach"
