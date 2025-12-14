{-# LANGUAGE ImportQualifiedPost #-}

module MonadicPatterns where

import Control.Monad (liftM, liftM2, mapM, mapM_, forM, forM_, sequence, sequence_, ap, join, (>=>))
import Data.Traversable (traverse, for, sequenceA)
import Data.Foldable (traverse_, for_, sequenceA_)
import Data.Functor (void, (<$>))
import Control.Applicative (liftA2, (<*>), (*>))

--------------------------------------------------------------------------------
-- Bind Simplification Patterns
--------------------------------------------------------------------------------

-- Should simplify: return x >>= f  ==>  f x
bindWithReturn :: Monad m => (a -> m b) -> a -> m b
bindWithReturn f x = pure x >>= f

-- Should simplify: m >>= return  ==>  m
bindReturn :: Monad m => m a -> m a
bindReturn m = m >>= return

-- Should simplify: m >>= \x -> return (f x)  ==>  fmap f m
bindReturnFunc :: Monad m => (a -> b) -> m a -> m b
bindReturnFunc f m = m >>= \x -> pure (f x)

-- Should simplify: m >>= \x -> pure (f x)  ==>  fmap f m
bindPureFunc :: Monad m => (a -> b) -> m a -> m b
bindPureFunc f m = m >>= \x -> pure (f x)

-- Should simplify: m >>= \_ -> n  ==>  m >> n
bindIgnoreResult :: Monad m => m a -> m b -> m b
bindIgnoreResult m n = m >>= \_ -> n

-- Should simplify: m >> return ()  ==>  void m
bindReturnUnit :: Monad m => m a -> m ()
bindReturnUnit m = m >> pure ()

-- Should simplify: m >>= id  ==>  join m
bindWithId :: Monad m => m (m a) -> m a
bindWithId m = m >>= id

--------------------------------------------------------------------------------
-- Applicative Modernization
--------------------------------------------------------------------------------

-- Should modernize: return x  ==>  pure x
useReturn :: Monad m => Int -> m Int
useReturn x = pure x

-- Should modernize: liftM f m  ==>  fmap f m
useLiftM :: Monad m => (a -> b) -> m a -> m b
useLiftM f m = fmap f m

-- Should modernize: liftM2 f m1 m2  ==>  liftA2 f m1 m2
useLiftM2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
useLiftM2 f m1 m2 = liftA2 f m1 m2

-- Should modernize: f `ap` x  ==>  f <*> x
useAp :: Monad m => m (a -> b) -> m a -> m b
useAp f x = f `ap` x

-- Should modernize: m >>= \_ -> n  ==>  m *> n
bindIgnoreAp :: Monad m => m a -> m b -> m b
bindIgnoreAp m n = m >>= \_ -> n

--------------------------------------------------------------------------------
-- Traversal Modernization
--------------------------------------------------------------------------------

-- Should modernize: mapM f xs  ==>  traverse f xs
useMapM :: Monad m => (a -> m b) -> [a] -> m [b]
useMapM f xs = traverse f xs

-- Should modernize: mapM_ f xs  ==>  traverse_ f xs
useMapM_ :: Monad m => (a -> m b) -> [a] -> m ()
useMapM_ f xs = traverse_ f xs

-- Should modernize: forM xs f  ==>  for xs f
useForM :: Monad m => [a] -> (a -> m b) -> m [b]
useForM xs f = for xs f

-- Should modernize: forM_ xs f  ==>  for_ xs f
useForM_ :: Monad m => [a] -> (a -> m b) -> m ()
useForM_ xs f = for_ xs f

-- Should modernize: sequence actions  ==>  sequenceA actions
useSequence :: Monad m => [m a] -> m [a]
useSequence actions = sequenceA actions

-- Should modernize: sequence_ actions  ==>  sequenceA_ actions
useSequence_ :: Monad m => [m a] -> m ()
useSequence_ actions = sequence_ actions

--------------------------------------------------------------------------------
-- Do-Notation Simplification
--------------------------------------------------------------------------------

-- Should simplify: do { return x }  ==>  pure x
redundantDoReturn :: Monad m => Int -> m Int
redundantDoReturn x = do { return x }

-- Should simplify: do { x <- m; return x }  ==>  m
redundantDoBind :: Monad m => m a -> m a
redundantDoBind m = do
  x <- m
return x

-- Should simplify: do { let x = y; return x }  ==>  pure y
redundantDoLet :: Monad m => Int -> m Int
redundantDoLet y = do
  let x = y
return x

-- Should simplify: do { action }  ==>  action
singleStatementDo :: Monad m => m a -> m a
singleStatementDo action = do { action }

-- Should simplify: do { m1; m2 }  ==>  m1 >> m2
twoStatementDo :: Monad m => m a -> m b -> m b
twoStatementDo m1 m2 = do { m1; m2 }

--------------------------------------------------------------------------------
-- Nested fmap patterns
--------------------------------------------------------------------------------

-- Should use: (<$>)
nestedFmap :: Functor f => (a -> b) -> f a -> f b
nestedFmap f x = fmap f x

-- Chained fmap
chainedFmap :: Functor f => (b -> c) -> (a -> b) -> f a -> f c
chainedFmap g f x = fmap g (fmap f x)

--------------------------------------------------------------------------------
-- Kleisli Composition
--------------------------------------------------------------------------------

-- Should modernize: \x -> f x >>= g  ==>  f >=> g
kleisliPattern :: Monad m => (a -> m b) -> (b -> m c) -> (a -> m c)
kleisliPattern f g = \x -> f x >>= g

--------------------------------------------------------------------------------
-- Complex realistic examples
--------------------------------------------------------------------------------

-- Process a list of items with effects
processItems :: Monad m => (a -> m b) -> [a] -> m [b]
processItems processor items = traverse processor items

-- Validate and transform
validateAndTransform :: Monad m => (a -> m Bool) -> (a -> b) -> [a] -> m [b]
validateAndTransform validate transform items = do
validItems <- traverse validate items
return (map transform items)

-- Perform action and ignore result
performAndIgnore :: Monad m => m a -> m b -> m b
performAndIgnore action next = action >>= \_ -> next

-- Double bind return pattern
doubleBindReturn :: Monad m => m a -> (a -> b) -> m b
doubleBindReturn m f = m >>= \x -> return (f x)

-- Multiple actions in sequence
multipleActions :: Monad m => m a -> m b -> m c -> m c
multipleActions action1 action2 action3 = do
  action1
  action2
  action3