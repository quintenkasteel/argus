module Modernization where

import Control.Monad (liftM)

-- Should use 'pure' instead of 'return'
example1 :: Monad m => m Int
example1 = pure 42

-- Should use 'traverse' instead of 'mapM'
example2 :: Monad m => (a -> m b) -> [a] -> m [b]
example2 f xs = traverse f xs

-- Should use 'fmap' instead of 'liftM'
example3 :: Monad m => (a -> b) -> m a -> m b
example3 f x = fmap f x