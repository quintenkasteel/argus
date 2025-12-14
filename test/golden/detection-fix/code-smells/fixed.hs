{-# LANGUAGE OverloadedStrings #-}

module CodeSmellsExamples where


--------------------------------------------------------------------------------
-- Long function (complexity warning)
--------------------------------------------------------------------------------

-- Should warn: function is too long/complex
longFunction :: Int -> Int -> Int -> String
longFunction a b c =
  let x1 = a + b
      x2 = x1 * c
      x3 = x2 - a
      x4 = x3 + b
      x5 = x4 * 2
      x6 = x5 - 1
      x7 = x6 + x1
      x8 = x7 * x2
      x9 = x8 - x3
      x10 = x9 + x4
      result = x10 * x5
  in show result

--------------------------------------------------------------------------------
-- Too many parameters
--------------------------------------------------------------------------------

-- Should warn: too many parameters
tooManyParams :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int
tooManyParams a b c d e f g = a + b + c + d + e + f + g

-- Acceptable: fewer parameters
acceptableParams :: Int -> Int -> Int -> Int
acceptableParams a b c = a + b + c

--------------------------------------------------------------------------------
-- Deep nesting
--------------------------------------------------------------------------------

-- Should warn: deeply nested code
deeplyNested :: Maybe (Maybe (Maybe Int)) -> Int
deeplyNested m = case m of
  Nothing -> 0
  Just m2 -> case m2 of
    Nothing -> 1
    Just m3 -> case m3 of
      Nothing -> 2
      Just n -> n

-- Better: flatten with maybe
flattenedMaybe :: Maybe (Maybe (Maybe Int)) -> Int
flattenedMaybe m = maybe 0 (maybe 1 (maybe 2 id)) m

--------------------------------------------------------------------------------
-- God function (does too many things)
--------------------------------------------------------------------------------

-- Should warn: function does too many unrelated things
godFunction :: String -> IO ()
godFunction input = do
  -- Parse
  let parsed = words input
  -- Validate
  let validated = filter (/= "") parsed
  -- Process
  let processed = map length validated
  -- Format
  let formatted = unwords (map show processed)
  -- Output
  putStrLn formatted
  -- Log
  putStrLn $ "Processed " ++ show (length parsed) ++ " words"

--------------------------------------------------------------------------------
-- Magic numbers
--------------------------------------------------------------------------------

-- Should warn: magic number
magicNumber :: Int -> Bool
magicNumber n = n > 42

-- Better: use named constant
maxThreshold :: Int
maxThreshold = 42

namedConstant :: Int -> Bool
namedConstant n = n > maxThreshold

--------------------------------------------------------------------------------
-- Boolean blindness
--------------------------------------------------------------------------------

-- Should warn: boolean blindness
booleanBlindness :: Bool -> Bool -> Int
booleanBlindness isActive isAdmin =
  if isActive
    then if isAdmin then 3 else 1
    else 0

--------------------------------------------------------------------------------
-- Feature envy
--------------------------------------------------------------------------------

data Person = Person { name :: String, age :: Int, email :: String }

-- Should warn: feature envy - accessing many fields of another type
formatPerson :: Person -> String
formatPerson p =
  name p ++ " (" ++ show (age p) ++ ") <" ++ email p ++ ">"

--------------------------------------------------------------------------------
-- Primitive obsession
--------------------------------------------------------------------------------

-- Should warn: using String for email
sendEmail :: String -> String -> IO ()
sendEmail toEmail body = putStrLn $ "Sending to " ++ toEmail ++ ": " ++ body

-- Better: use newtype
newtype Email = Email { unEmail :: String }

sendEmailTyped :: Email -> String -> IO ()
sendEmailTyped (Email to) body = putStrLn $ "Sending to " ++ to ++ ": " ++ body

--------------------------------------------------------------------------------
-- Comments explaining bad code
--------------------------------------------------------------------------------

-- Should warn: comment explaining what code does (code should be self-documenting)
-- This function adds one to the input
addOne :: Int -> Int
addOne x = x + 1  -- increment by one

--------------------------------------------------------------------------------
-- Dead code
--------------------------------------------------------------------------------

-- Should warn: unused helper function
unusedHelper :: Int -> Int
unusedHelper x = x * 2

usedFunction :: Int -> Int
usedFunction = (* 3)

--------------------------------------------------------------------------------
-- Duplicate code patterns
--------------------------------------------------------------------------------

-- Should warn: duplicate logic
processA :: Int -> Int
processA x = x + 1 + 2 * 3

processB :: Int -> Int
processB x = x + 1 + 2 * 3  -- duplicate

processC :: Int -> Int
processC x = x + 1 + 2 * 3  -- duplicate
