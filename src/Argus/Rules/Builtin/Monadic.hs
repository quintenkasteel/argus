{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Argus.Rules.Builtin.Monadic
-- Description : Monad and Applicative modernization rules
-- Copyright   : (c) 2024
-- License     : MIT
-- Stability   : stable
--
-- Comprehensive rules for modernizing monadic code to use current best
-- practices. These rules encourage Applicative style, modern traversal
-- functions, and proper use of do-notation.
--
-- == Rule Categories
--
-- * __Applicative Modernization__: Monad → Applicative where possible
-- * __Traversal Modernization__: mapM → traverse, forM → for
-- * __Do-Notation__: Simplify and modernize do blocks
-- * __Monad Transformers__: Best practices for transformer stacks
-- * __Bind Simplification__: Simplify (>>=) and (>>) patterns
--
-- == References
--
-- * <https://wiki.haskell.org/Applicative_functor Applicative Functors>
-- * <https://hackage.haskell.org/package/base/docs/Data-Traversable.html Traversable>
-- * <https://wiki.haskell.org/All_About_Monads Monads>

module Argus.Rules.Builtin.Monadic
  ( -- * Rule Sets
    monadicRules
  , applicativeModernRules
  , traversalRules
  , doNotationRules
  , transformerRules
  , bindRules

    -- * Applicative Modernization
  , returnToPure
  , liftMToFmap
  , liftM2ToLiftA2
  , apToApplicative
  , sequenceAp
  , monadApplicativeUpgrade

    -- * Traversal Modernization
  , mapMToTraverse
  , mapM_ToTraverse_
  , forMToFor
  , forM_ToFor_
  , sequenceToSequenceA
  , sequence_ToSequenceA_
  , replicateMToReplicateA

    -- * Do-Notation
  , redundantDoReturn
  , redundantDoBind
  , redundantDoLet
  , singleStatementDo
  , unnecessaryDo
  , letInDo
  , lastStatementBind

    -- * Monad Transformers
  , liftToMonadIO
  , askToReader
  , getToState
  , tellToWriter
  , throwToExcept
  , mtlStyle

    -- * Bind Simplification
  , bindReturn
  , bindPure
  , bindIgnore
  , voidBind
  , joinBind
  , kleisliCompose

    -- * Rule Count
  , monadicRuleCount
  ) where

import Argus.Rules.DSL

--------------------------------------------------------------------------------
-- Rule Sets
--------------------------------------------------------------------------------

-- | All monadic modernization rules.
monadicRules :: [Rule]
monadicRules = mconcat
  [ applicativeModernRules
  , traversalRules
  , doNotationRules
  , transformerRules
  , bindRules
  ]

-- | Total count of monadic rules.
monadicRuleCount :: Int
monadicRuleCount = length monadicRules

--------------------------------------------------------------------------------
-- Applicative Modernization Rules
--------------------------------------------------------------------------------

-- | Rules for modernizing to Applicative style.
applicativeModernRules :: [Rule]
applicativeModernRules =
  [ returnToPure
  , liftMToFmap
  , liftM2ToLiftA2
  , apToApplicative
  , sequenceAp
  , monadApplicativeUpgrade
  ]

-- | Replace return with pure.
--
-- @
-- return x  ==>  pure x
-- @
returnToPure :: Rule
returnToPure =
  rule "return-to-pure" $
    match ("return _x" ==> "pure _x")
    & category Modernization
    & severity Suggestion
    & message "Use 'pure' instead of 'return'"
    & note "pure is the Applicative version, works in more contexts"

-- | Replace liftM with fmap.
--
-- @
-- liftM f m  ==>  fmap f m
-- @
liftMToFmap :: Rule
liftMToFmap =
  rule "liftm-to-fmap" $
    match ("liftM _f _m" ==> "fmap _f _m")
    & category Modernization
    & severity Suggestion
    & message "Use 'fmap' instead of 'liftM'"
    & note "fmap is more general (Functor vs Monad)"

-- | Replace liftM2 with liftA2.
--
-- @
-- liftM2 f m1 m2  ==>  liftA2 f m1 m2
-- @
liftM2ToLiftA2 :: Rule
liftM2ToLiftA2 =
  rule "liftm2-to-lifta2" $
    match ("liftM2 _f _m1 _m2" ==> "liftA2 _f _m1 _m2")
    & category Modernization
    & severity Suggestion
    & message "Use 'liftA2' instead of 'liftM2'"

-- | Replace ap with <*>.
--
-- @
-- f `ap` x  ==>  f <*> x
-- @
apToApplicative :: Rule
apToApplicative =
  rule "ap-to-applicative" $
    match ("_f `ap` _x" ==> "_f <*> _x")
    & category Modernization
    & severity Suggestion
    & message "Use '<*>' instead of 'ap'"

-- | Applicative sequence pattern.
--
-- @
-- do { x <- mx; y <- my; return (f x y) }  ==>  f <$> mx <*> my
-- @
sequenceAp :: Rule
sequenceAp =
  rule "sequence-ap" $
    match ("do { _x <- _mx; _y <- _my; return (_f _x _y) }"
           ==> "_f <$> _mx <*> _my")
    & category Modernization
    & severity Suggestion
    & message "Use Applicative style instead of do-notation"
    & note "When bindings are independent, Applicative is simpler"

-- | General Monad to Applicative suggestion.
--
-- @
-- do { x <- action1; action2 x }
-- -- If action2 doesn't use x in its effects, can use Applicative
-- @
monadApplicativeUpgrade :: Rule
monadApplicativeUpgrade =
  rule "monad-applicative-upgrade" $
    match ("_m >>= \\_ -> _n" ==> "_m *> _n")
    & category Modernization
    & severity Suggestion
    & message "Use '*>' when ignoring result"
    & note "m >>= \\_ -> n is equivalent to m *> n"

--------------------------------------------------------------------------------
-- Traversal Modernization Rules
--------------------------------------------------------------------------------

-- | Rules for modernizing traversal functions.
traversalRules :: [Rule]
traversalRules =
  [ mapMToTraverse
  , mapM_ToTraverse_
  , forMToFor
  , forM_ToFor_
  , sequenceToSequenceA
  , sequence_ToSequenceA_
  , replicateMToReplicateA
  ]

-- | Replace mapM with traverse.
--
-- @
-- mapM f xs  ==>  traverse f xs
-- @
mapMToTraverse :: Rule
mapMToTraverse =
  rule "mapm-to-traverse" $
    match ("mapM _f _xs" ==> "traverse _f _xs")
    & category Modernization
    & severity Suggestion
    & message "Use 'traverse' instead of 'mapM'"
    & note "traverse is the Applicative version, more general"

-- | Replace mapM_ with traverse_.
--
-- @
-- mapM_ f xs  ==>  traverse_ f xs
-- @
mapM_ToTraverse_ :: Rule
mapM_ToTraverse_ =
  rule "mapm_-to-traverse_" $
    match ("mapM_ _f _xs" ==> "traverse_ _f _xs")
    & category Modernization
    & severity Suggestion
    & message "Use 'traverse_' instead of 'mapM_'"

-- | Replace forM with for.
--
-- @
-- forM xs f  ==>  for xs f
-- @
forMToFor :: Rule
forMToFor =
  rule "form-to-for" $
    match ("forM _xs _f" ==> "for _xs _f")
    & category Modernization
    & severity Suggestion
    & message "Use 'for' instead of 'forM'"

-- | Replace forM_ with for_.
--
-- @
-- forM_ xs f  ==>  for_ xs f
-- @
forM_ToFor_ :: Rule
forM_ToFor_ =
  rule "form_-to-for_" $
    match ("forM_ _xs _f" ==> "for_ _xs _f")
    & category Modernization
    & severity Suggestion
    & message "Use 'for_' instead of 'forM_'"

-- | Replace sequence with sequenceA.
--
-- @
-- sequence actions  ==>  sequenceA actions
-- @
sequenceToSequenceA :: Rule
sequenceToSequenceA =
  rule "sequence-to-sequencea" $
    match ("sequence _xs" ==> "sequenceA _xs")
    & category Modernization
    & severity Suggestion
    & message "Use 'sequenceA' instead of 'sequence'"
    & note "sequenceA works with any Applicative, not just Monad"

-- | Replace sequence_ with sequenceA_.
--
-- @
-- sequence_ actions  ==>  sequenceA_ actions
-- @
sequence_ToSequenceA_ :: Rule
sequence_ToSequenceA_ =
  rule "sequence_-to-sequencea_" $
    match ("sequence_ _xs" ==> "sequenceA_ _xs")
    & category Modernization
    & severity Suggestion
    & message "Use 'sequenceA_' instead of 'sequence_'"

-- | Replace replicateM with replicateA (custom or comment).
--
-- @
-- replicateM n action  -- Consider if Applicative suffices
-- @
replicateMToReplicateA :: Rule
replicateMToReplicateA =
  rule "replicatem-to-replicatea" $
    match ("replicateM _n _m" ==> "replicateM _n _m")
    & category Modernization
    & severity Info
    & message "replicateM requires Monad - ensure this is necessary"
    & note "If effects are independent, consider sequenceA . replicate"
    & safetyLevel ManualReview

--------------------------------------------------------------------------------
-- Do-Notation Rules
--------------------------------------------------------------------------------

-- | Rules for simplifying do-notation.
doNotationRules :: [Rule]
doNotationRules =
  [ redundantDoReturn
  , redundantDoBind
  , redundantDoLet
  , singleStatementDo
  , unnecessaryDo
  , letInDo
  , lastStatementBind
  ]

-- | Redundant do with just return.
--
-- @
-- do { return x }  ==>  pure x
-- @
redundantDoReturn :: Rule
redundantDoReturn =
  rule "redundant-do-return" $
    match ("do { return _x }" ==> "pure _x")
    & category Style
    & severity Suggestion
    & message "Redundant do-notation around return"

-- | Redundant bind that just returns.
--
-- @
-- do { x <- m; return x }  ==>  m
-- @
redundantDoBind :: Rule
redundantDoBind =
  rule "redundant-do-bind" $
    match ("do { _x <- _m; return _x }" ==> "_m")
    & category Style
    & severity Suggestion
    & message "Redundant bind and return - just use the action"

-- | Redundant let in do.
--
-- @
-- do { let x = y; return x }  ==>  pure y
-- @
redundantDoLet :: Rule
redundantDoLet =
  rule "redundant-do-let" $
    match ("do { let _x = _y; return _x }" ==> "pure _y")
    & category Style
    & severity Suggestion
    & message "Redundant let in do-notation"

-- | Single statement do-notation.
--
-- @
-- do { action }  ==>  action
-- @
singleStatementDo :: Rule
singleStatementDo =
  rule "single-statement-do" $
    match ("do { _m }" ==> "_m")
    & category Style
    & severity Suggestion
    & message "Redundant do-notation with single statement"

-- | Unnecessary do when expression is simpler.
--
-- @
-- do { m1; m2 }  ==>  m1 >> m2  (sometimes clearer)
-- @
unnecessaryDo :: Rule
unnecessaryDo =
  rule "unnecessary-do" $
    match ("do { _m1; _m2 }" ==> "_m1 >> _m2")
    & category Style
    & severity Info
    & message "Two-statement do can use (>>)"
    & safetyLevel ManualReview

-- | Let binding in do can use <-.
--
-- @
-- do { let x = pure y; ... }  -- Prefer <- for monadic values
-- @
letInDo :: Rule
letInDo =
  rule "let-in-do" $
    match ("do { let _x = pure _y; _rest }" ==> "do { _x <- pure _y; _rest }")
    & category Style
    & severity Info
    & message "Consider <- instead of let for monadic values"
    & safetyLevel ManualReview

-- | Last statement shouldn't be a bind.
--
-- @
-- do { ...; x <- action }  -- x is unused!
-- @
lastStatementBind :: Rule
lastStatementBind =
  rule "last-statement-bind" $
    matchText "; [a-z]+ <- [a-z]+[^;]*$"
    & category Style
    & severity Warning
    & message "Last statement in do-block binds unused variable"
    & note "Either use the bound value or use >> instead of <-"

--------------------------------------------------------------------------------
-- Monad Transformer Rules
--------------------------------------------------------------------------------

-- | Rules for monad transformer best practices.
transformerRules :: [Rule]
transformerRules =
  [ liftToMonadIO
  , askToReader
  , getToState
  , tellToWriter
  , throwToExcept
  , mtlStyle
  ]

-- | Use liftIO instead of multiple lifts.
--
-- @
-- lift (lift (lift action))  ==>  liftIO action
-- @
liftToMonadIO :: Rule
liftToMonadIO =
  rule "lift-to-monadio" $
    match ("lift (lift _action)" ==> "liftIO _action")
    & category Style
    & severity Suggestion
    & message "Use 'liftIO' instead of nested 'lift'"
    & note "liftIO works through any transformer stack"

-- | Use ask from MonadReader.
--
-- @
-- lift ask  ==>  ask  (with MonadReader constraint)
-- @
askToReader :: Rule
askToReader =
  rule "ask-to-reader" $
    match ("lift ask" ==> "ask")
    & category Style
    & severity Suggestion
    & message "Use mtl-style 'ask' without lift"
    & note "MonadReader provides ask at any transformer level"

-- | Use get from MonadState.
--
-- @
-- lift get  ==>  get  (with MonadState constraint)
-- @
getToState :: Rule
getToState =
  rule "get-to-state" $
    match ("lift get" ==> "get")
    & category Style
    & severity Suggestion
    & message "Use mtl-style 'get' without lift"

-- | Use tell from MonadWriter.
--
-- @
-- lift (tell w)  ==>  tell w  (with MonadWriter constraint)
-- @
tellToWriter :: Rule
tellToWriter =
  rule "tell-to-writer" $
    match ("lift (tell _w)" ==> "tell _w")
    & category Style
    & severity Suggestion
    & message "Use mtl-style 'tell' without lift"

-- | Use throwError from MonadError.
--
-- @
-- lift (throwError e)  ==>  throwError e
-- @
throwToExcept :: Rule
throwToExcept =
  rule "throw-to-except" $
    match ("lift (throwError _e)" ==> "throwError _e")
    & category Style
    & severity Suggestion
    & message "Use mtl-style 'throwError' without lift"

-- | Suggest mtl-style over explicit transformer.
--
-- @
-- ReaderT Config IO a  -- Consider: MonadReader Config m => m a
-- @
mtlStyle :: Rule
mtlStyle =
  rule "mtl-style" $
    matchText ":: ReaderT .+ IO"
    & category Style
    & severity Info
    & message "Consider mtl-style constraint instead of concrete transformer"
    & note "MonadReader r m => m a is more flexible than ReaderT r IO a"
    & safetyLevel ManualReview

--------------------------------------------------------------------------------
-- Bind Simplification Rules
--------------------------------------------------------------------------------

-- | Rules for simplifying bind operations.
bindRules :: [Rule]
bindRules =
  [ bindReturn
  , bindPure
  , bindIgnore
  , voidBind
  , joinBind
  , kleisliCompose
  ]

-- | Bind followed by return.
--
-- @
-- m >>= \\x -> return (f x)  ==>  fmap f m
-- @
bindReturn :: Rule
bindReturn =
  rule "bind-return" $
    match ("_m >>= \\x -> return (_f x)" ==> "fmap _f _m")
    & category Style
    & severity Suggestion
    & message "m >>= return . f  ==>  fmap f m"

-- | Bind followed by pure.
--
-- @
-- m >>= \\x -> pure (f x)  ==>  fmap f m
-- @
bindPure :: Rule
bindPure =
  rule "bind-pure" $
    match ("_m >>= \\x -> pure (_f x)" ==> "fmap _f _m")
    & category Style
    & severity Suggestion
    & message "m >>= pure . f  ==>  fmap f m"

-- | Bind that ignores result.
--
-- @
-- m >>= \\_ -> n  ==>  m >> n
-- @
bindIgnore :: Rule
bindIgnore =
  rule "bind-ignore" $
    match ("_m >>= \\_ -> _n" ==> "_m >> _n")
    & category Style
    & severity Suggestion
    & message "Use '>>' when ignoring the bound value"

-- | Use void instead of >> return ().
--
-- @
-- m >> return ()  ==>  void m
-- @
voidBind :: Rule
voidBind =
  rule "void-bind" $
    match ("_m >> return ()" ==> "void _m")
    & category Style
    & severity Suggestion
    & message "Use 'void' instead of '>> return ()'"

-- | Use join instead of bind with id.
--
-- @
-- m >>= id  ==>  join m
-- @
joinBind :: Rule
joinBind =
  rule "join-bind" $
    match ("_m >>= id" ==> "join _m")
    & category Style
    & severity Suggestion
    & message "Use 'join' instead of '>>= id'"

-- | Suggest Kleisli composition.
--
-- @
-- \\x -> f x >>= g  ==>  f >=> g
-- @
kleisliCompose :: Rule
kleisliCompose =
  rule "kleisli-compose" $
    match ("\\x -> _f x >>= _g" ==> "_f >=> _g")
    & category Style
    & severity Suggestion
    & message "Use Kleisli composition (>=>)"
    & note "f >=> g = \\x -> f x >>= g"
