{-# LANGUAGE ScopedTypeVariables #-}

module DListSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Control.DeepSeq (force)
import Control.Exception (evaluate)
import Data.Foldable qualified as F
import qualified Prelude as P

import Argus.Internal.DList
import Prelude hiding (length)

--------------------------------------------------------------------------------
-- Test Suite
--------------------------------------------------------------------------------

spec :: Spec
spec = do
  describe "DList construction" $ do
    it "empty creates an empty list" $
      toList empty `shouldBe` ([] :: [Int])

    it "singleton creates a single-element list" $
      toList (singleton 42) `shouldBe` [42]

    it "fromList preserves list elements" $ property $
      \(xs :: [Int]) -> toList (fromList xs) `shouldBe` xs

  describe "DList operations" $ do
    it "append concatenates two dlists" $ property $
      \(xs :: [Int]) (ys :: [Int]) ->
        toList (fromList xs `append` fromList ys) `shouldBe` (xs ++ ys)

    it "snoc adds element to end" $ property $
      \(xs :: [Int]) (x :: Int) ->
        toList (snoc (fromList xs) x) `shouldBe` (xs ++ [x])

    it "cons adds element to front" $ property $
      \(xs :: [Int]) (x :: Int) ->
        toList (cons x (fromList xs)) `shouldBe` (x : xs)

    it "multiple snocs preserve order" $ do
      let dl = empty `snoc` 1 `snoc` 2 `snoc` 3
      toList dl `shouldBe` [1, 2, 3]

    it "multiple cons preserve reverse order" $ do
      let dl = cons 3 (cons 2 (cons 1 empty))
      toList dl `shouldBe` [3, 2, 1]

  describe "DList Semigroup instance" $ do
    it "(<>) is the same as append" $ property $
      \(xs :: [Int]) (ys :: [Int]) ->
        toList (fromList xs <> fromList ys) `shouldBe` (xs ++ ys)

    it "satisfies associativity law" $ property $
      \(xs :: [Int]) (ys :: [Int]) (zs :: [Int]) ->
        let x = fromList xs
            y = fromList ys
            z = fromList zs
        in toList ((x <> y) <> z) `shouldBe` toList (x <> (y <> z))

  describe "DList Monoid instance" $ do
    it "mempty is empty" $
      toList (mempty :: DList Int) `shouldBe` []

    it "satisfies left identity law" $ property $
      \(xs :: [Int]) ->
        toList (mempty <> fromList xs) `shouldBe` xs

    it "satisfies right identity law" $ property $
      \(xs :: [Int]) ->
        toList (fromList xs <> mempty) `shouldBe` xs

    it "satisfies associativity law" $ property $
      \(xs :: [Int]) (ys :: [Int]) (zs :: [Int]) ->
        let x = fromList xs
            y = fromList ys
            z = fromList zs
        in toList ((x <> y) <> z) `shouldBe` toList (x <> (y <> z))

    it "mconcat works correctly" $ property $
      \(xss :: [[Int]]) ->
        toList (mconcat (map fromList xss)) `shouldBe` concat xss

  describe "DList Functor instance" $ do
    it "fmap preserves structure" $ property $
      \(xs :: [Int]) ->
        toList (fmap (*2) (fromList xs)) `shouldBe` map (*2) xs

    it "satisfies functor identity law" $ property $
      \(xs :: [Int]) ->
        toList (fmap id (fromList xs)) `shouldBe` toList (fromList xs)

    it "satisfies functor composition law" $ property $
      \(xs :: [Int]) ->
        let f = (*2)
            g = (+1)
        in toList (fmap (f . g) (fromList xs)) `shouldBe` toList (fmap f (fmap g (fromList xs)))

  describe "DList Applicative instance" $ do
    it "pure creates singleton" $ property $
      \(x :: Int) ->
        toList (pure x) `shouldBe` [x]

    it "(<*>) applies functions" $ do
      let fs = fromList [(+1), (*2), (+10)]
          xs = fromList [1, 2, 3]
      toList (fs <*> xs) `shouldBe` [f x | f <- toList fs, x <- toList xs]

    it "satisfies applicative identity law" $ property $
      \(xs :: [Int]) ->
        toList (pure id <*> fromList xs) `shouldBe` toList (fromList xs)

  describe "DList Monad instance" $ do
    it "return creates singleton" $ property $
      \(x :: Int) ->
        toList (return x) `shouldBe` [x]

    it "(>>=) chains operations" $ do
      let dl = fromList [1, 2, 3]
          f x = fromList [x, x * 10]
      toList (dl >>= f) `shouldBe` [1, 10, 2, 20, 3, 30]

    it "satisfies monad left identity law" $ property $
      \(x :: Int) ->
        let f a = fromList [a, a * 2]
        in toList (return x >>= f) `shouldBe` toList (f x)

    it "satisfies monad right identity law" $ property $
      \(xs :: [Int]) ->
        toList (fromList xs >>= return) `shouldBe` toList (fromList xs)

  describe "DList Foldable instance" $ do
    it "foldr works correctly" $ property $
      \(xs :: [Int]) ->
        F.foldr (+) 0 (fromList xs) `shouldBe` F.foldr (+) 0 xs

    it "foldl works correctly" $ property $
      \(xs :: [Int]) ->
        F.foldl (+) 0 (fromList xs) `shouldBe` F.foldl (+) 0 xs

    it "foldl' works correctly" $ property $
      \(xs :: [Int]) ->
        F.foldl' (+) 0 (fromList xs) `shouldBe` F.foldl' (+) 0 xs

    it "length works correctly" $ property $
      \(xs :: [Int]) ->
        F.length (fromList xs) `shouldBe` P.length xs

    it "null works correctly" $ do
      F.null empty `shouldBe` True
      F.null (singleton 1) `shouldBe` False

  describe "DList Eq instance" $ do
    it "equal lists are equal" $ property $
      \(xs :: [Int]) ->
        fromList xs `shouldBe` fromList xs

    it "different lists are not equal" $ do
      fromList [1, 2, 3] `shouldNotBe` fromList [1, 2, 4]

  describe "DList Ord instance" $ do
    it "compares correctly" $ do
      fromList [1, 2, 3] `shouldSatisfy` (< fromList [1, 2, 4])
      fromList [1, 2, 4] `shouldSatisfy` (> fromList [1, 2, 3])

  describe "DList Show instance" $ do
    it "shows as list" $ do
      show (fromList [1, 2, 3]) `shouldBe` "[1,2,3]"
      show (empty :: DList Int) `shouldBe` "[]"

  describe "DList round-trip" $ do
    it "fromList . toList is identity" $ property $
      \(xs :: [Int]) ->
        toList (fromList xs) `shouldBe` xs

    it "toList . fromList is identity" $ property $
      \(dl :: DList Int) ->
        fromList (toList dl) `shouldBe` dl

  describe "DList performance characteristics" $ do
    it "handles large lists efficiently" $ do
      let n = 10000 :: Int
          dl = F.foldl' snoc empty [1..n]
      P.length (toList dl) `shouldBe` n

    it "append is O(1)" $ do
      -- Build a list by repeatedly appending
      let n = 1000 :: Int
          buildWithAppend 0 acc = acc
          buildWithAppend i acc = buildWithAppend (i-1) (acc `append` singleton i)
          dl = buildWithAppend n empty
      P.length (toList dl) `shouldBe` n

    it "large nested appends don't overflow stack" $ do
      -- This would overflow stack with regular (++) if not properly optimized
      let n = 10000 :: Int
          dls = map singleton [1..n]
          combined = mconcat dls
      P.length (toList combined) `shouldBe` n

  describe "DList NFData instance" $ do
    it "forces evaluation" $ do
      let dl = fromList [1, 2, 3] :: DList Int
      evaluate (force dl) `shouldReturn` dl

  describe "DList stress tests" $ do
    it "handles alternating snoc and cons" $ property $
      \(xs :: [Int]) ->
        let build [] acc = acc
            build (y:ys) acc = build ys (cons y (snoc acc y))
            dl = build xs empty
        in P.length (toList dl) `shouldBe` 2 * P.length xs

    it "handles deeply nested monoid operations" $ do
      let xs = [[1,2], [3,4], [5,6], [7,8], [9,10]]
          dls = map fromList xs
          combined = mconcat [mconcat dls, mconcat dls, mconcat dls]
      toList combined `shouldBe` concat (replicate 3 (concat xs))

    it "handles mixed operations" $ property $
      \(xs :: [Int]) (ys :: [Int]) (z :: Int) ->
        let dl1 = fromList xs
            dl2 = fromList ys
            combined = cons z (dl1 <> dl2) `snoc` (z * 2)
        in toList combined `shouldBe` (z : xs ++ ys ++ [z * 2])

--------------------------------------------------------------------------------
-- QuickCheck Arbitrary instance for DList
--------------------------------------------------------------------------------

instance Arbitrary a => Arbitrary (DList a) where
  arbitrary = fromList <$> arbitrary
  shrink dl = map fromList (shrink (toList dl))

--------------------------------------------------------------------------------
-- Performance comparison test
-- (commented out by default, enable for benchmarking)
--------------------------------------------------------------------------------

{-
import Criterion.Main (defaultMain, bench, whnf)

performanceComparison :: IO ()
performanceComparison = defaultMain
  [ bench "DList append" $ whnf buildWithDList 1000
  , bench "List append" $ whnf buildWithList 1000
  ]
  where
    buildWithDList :: Int -> Int
    buildWithDList n = Prelude.length $ toList $ F.foldl' append empty (map singleton [1..n])

    buildWithList :: Int -> Int
    buildWithList n = Prelude.length $ F.foldl' (++) [] (map (:[]) [1..n])
-}
