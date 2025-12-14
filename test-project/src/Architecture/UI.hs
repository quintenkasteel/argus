-- | User Interface layer
-- This layer CAN depend on Core layer for types
-- It should NOT depend on DB layer directly (go through Core)
module Architecture.UI where

-- Note: In bad architecture, UI might import DB directly
-- We'll demonstrate this differently to avoid compile-time cycles

-- =============================================================================
-- UI types (duplicated - another code smell)
-- =============================================================================

-- | Duplicated User type for UI display
-- In proper architecture, this would come from Core or be a ViewModel
data UIUser = UIUser
  { uiUserId   :: Int
  , uiUserName :: String
  , uiUserAge  :: Int
  } deriving (Show, Eq)

-- =============================================================================
-- Presentation functions
-- =============================================================================

-- | Format user for display
formatForDisplay :: UIUser -> String
formatForDisplay u =
  "User: " ++ uiUserName u ++ " (age " ++ show (uiUserAge u) ++ ")"

-- | Show status message
showStatus :: String -> IO ()
showStatus msg = putStrLn $ "[STATUS] " ++ msg

-- | Display user with score
displayUserWithScore :: UIUser -> Int -> IO ()
displayUserWithScore u score = do
  putStrLn $ formatForDisplay u
  putStrLn $ "Score: " ++ show score

-- | Render a list of users
renderUserList :: [UIUser] -> String
renderUserList users = unlines $ map formatForDisplay users

-- =============================================================================
-- BAD: UI component with embedded logic (should be in Core)
-- =============================================================================

-- | BAD: Business logic in UI layer
calculateAgeCategory :: UIUser -> String
calculateAgeCategory u
  | uiUserAge u < 18 = "minor"
  | uiUserAge u < 65 = "adult"
  | otherwise = "senior"

-- | BAD: Validation in UI layer (should be in Core)
validateUserInput :: String -> Int -> Either String UIUser
validateUserInput name age
  | null name = Left "Name cannot be empty"
  | age < 0 = Left "Age cannot be negative"
  | age > 150 = Left "Age seems unrealistic"
  | otherwise = Right $ UIUser 0 name age

-- =============================================================================
-- GOOD: Pure presentation that receives data as parameters
-- =============================================================================

-- | Pure rendering function - receives data, returns string
renderUserCard :: UIUser -> Maybe Int -> String
renderUserCard user maybeScore =
  let header = "=== " ++ uiUserName user ++ " ==="
      body = "Age: " ++ show (uiUserAge user)
      footer = case maybeScore of
        Nothing -> ""
        Just s  -> "Score: " ++ show s
  in unlines [header, body, footer]
