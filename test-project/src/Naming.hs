-- | Examples of naming convention issues that Argus should detect
-- Note: GHC enforces type names start with uppercase, so we demonstrate
-- other naming issues Argus can catch
module Naming where

-- =============================================================================
-- Unusual type naming (valid but unconventional)
-- =============================================================================

-- | Type with underscore in name - unusual for Haskell
data User_Record = UserRecord { urName :: String }
  deriving (Show, Eq)

-- | Very short type name - might be too terse
data X = X Int
  deriving (Show, Eq)

-- | Type name with numbers - unusual
data User2 = User2 { u2Name :: String }
  deriving (Show, Eq)

-- | Correct type naming for comparison
data Customer = Customer { customerName :: String }
  deriving (Show, Eq)

-- =============================================================================
-- Variable/function naming violations
-- =============================================================================

-- | Function with underscore_case - Haskell convention is camelCase
get_user_name :: User_Record -> String
get_user_name r = urName r

-- | Function with underscore_case - another example
calculate_total_price :: Double -> Double -> Double
calculate_total_price a b = a + b

-- | Single letter function name (too short, not descriptive)
f :: Int -> Int
f x = x + 1

-- | Meaningless short names
g :: Int -> Int -> Int
g a b = a * b

-- | Another single letter name
h :: String -> String
h = reverse

-- | Very long function name (excessive)
calculateTheTotalPriceOfAllItemsInTheShoppingCartIncludingTaxAndShipping :: Double -> Double -> Double
calculateTheTotalPriceOfAllItemsInTheShoppingCartIncludingTaxAndShipping price shipping = price + shipping

-- | Correct camelCase naming for comparison
calculateTotal :: Double -> Double -> Double
calculateTotal price shipping = price + shipping

-- =============================================================================
-- Constant naming (debatable conventions)
-- =============================================================================

-- | All caps constant with underscores (C-style, uncommon in Haskell)
mAX_RETRY_COUNT :: Int
mAX_RETRY_COUNT = 5

-- | Preferred Haskell constant style
maxRetryCount :: Int
maxRetryCount = 5

-- =============================================================================
-- Confusing similar names
-- =============================================================================

-- | These names are too similar - easy to confuse
data1 :: Int
data1 = 1

data2 :: Int
data2 = 2

dataa :: Int
dataa = 3

-- | Similar prefixes that might be confusing
processUser :: User_Record -> String
processUser r = urName r

processUserInfo :: User_Record -> (String, Int)
processUserInfo r = (urName r, 0)

processUserData :: User_Record -> String
processUserData = processUser

-- =============================================================================
-- Boolean naming (should indicate boolean nature)
-- =============================================================================

-- | Boolean without is/has/can prefix - not clear it's a Bool
valid :: Bool
valid = True

-- | Better boolean naming
isValid :: Bool
isValid = True

-- | Predicate function without clear boolean naming
check :: Int -> Bool
check n = n > 0

-- | Better predicate naming
isPositive :: Int -> Bool
isPositive n = n > 0

-- | Boolean field without clear naming
data Options = Options
  { optDebug :: Bool      -- Less clear that it's a flag
  , optIsEnabled :: Bool  -- Clearer
  , optHasLogger :: Bool  -- Clearer
  }
  deriving (Show, Eq)

-- =============================================================================
-- Hungarian notation (uncommon in Haskell)
-- =============================================================================

-- | Hungarian notation - unusual in Haskell
strUserName :: String
strUserName = "Alice"

intUserAge :: Int
intUserAge = 30

lstItems :: [Int]
lstItems = [1, 2, 3]

-- | Preferred Haskell style
userName :: String
userName = "Bob"

userAge :: Int
userAge = 25

items :: [Int]
items = [4, 5, 6]

-- =============================================================================
-- Overly abbreviated names
-- =============================================================================

-- | Too abbreviated - what does 'cfg' mean?
cfg :: Options
cfg = Options True True False

-- | Too abbreviated - what is 'req'?
processReq :: String -> String
processReq s = reverse s

-- | Better - spell out what it is
config :: Options
config = Options True True False

-- | Better - clear what we're processing
processRequest :: String -> String
processRequest s = reverse s
