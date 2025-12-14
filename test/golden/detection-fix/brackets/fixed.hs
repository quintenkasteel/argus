module BracketsTest where

-- ============================================================================
-- Dollar Operator Suggestions
-- ============================================================================

-- Suggest dollar instead of parentheses
dollarExample1 :: Int
dollarExample1 = abs (negate 5)

-- Multiple nested parentheses
dollarExample2 :: Int
dollarExample2 = abs (negate (signum (-3)))

-- Show with complex argument
dollarExample3 :: String -> String
dollarExample3 x = x

-- ============================================================================
-- Cases that should remain unchanged
-- ============================================================================

-- Parentheses needed for precedence
validPrecedence1 :: Int
validPrecedence1 = (1 + 2) * 3

-- Parentheses needed for composition section
validComposition :: Int -> Int
validComposition = negate . (+ 1)

-- Parentheses needed in arithmetic
validArithmetic :: Int
validArithmetic = 2 * (3 + 4)

-- Parentheses needed for function argument
validFunctionArg :: Int -> Int
validFunctionArg x = abs (negate x)

-- Parentheses needed in filter
validFilter :: [Int] -> [Int]
validFilter = filter (> 0)

-- Tuples need parentheses (syntactic)
validTuple :: (Int, Int)
validTuple = (1, 2)

-- Negative literals need parentheses
validNegative :: Int
validNegative = (-1)

-- Nested tuple
validNestedTuple :: (Int, (Bool, String))
validNestedTuple = (42, (True, "test"))

-- List of tuples
validListTuple :: [(Int, Bool)]
validListTuple = [(1, True), (2, False)]

-- Section with parentheses
validSection :: [Int] -> [Int]
validSection = map (+ 10)

-- Lambda with parentheses
validLambda :: (Int -> Int) -> Int
validLambda f = f (5 + 3)

-- Operator precedence preserved
validOperator :: Bool
validOperator = True && (False || True)