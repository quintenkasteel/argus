-- | Core business logic layer
-- This layer should NOT depend on DB or UI layers
-- But this module VIOLATES that principle for demonstration
module Architecture.Core where

import Architecture.DB (saveUser, loadUser, DBUser(..))  -- VIOLATION: Core imports DB!
import Architecture.UI (formatForDisplay, UIUser(..))    -- VIOLATION: Core imports UI!

-- =============================================================================
-- Core business types
-- =============================================================================

data User = User
  { userId   :: Int
  , userName :: String
  , userAge  :: Int
  } deriving (Show, Eq)

data ValidationError
  = EmptyName
  | InvalidAge
  | NameTooLong
  deriving (Show, Eq)

-- =============================================================================
-- Business logic (should be pure)
-- =============================================================================

-- | Validate user data - this is proper core logic
validateUser :: User -> Either ValidationError User
validateUser u
  | null (userName u) = Left EmptyName
  | userAge u < 0 || userAge u > 150 = Left InvalidAge
  | length (userName u) > 100 = Left NameTooLong
  | otherwise = Right u

-- | Calculate some business metric - proper core logic
calculateScore :: User -> Int
calculateScore u = userAge u * length (userName u)

-- | Check if user is adult - proper core logic
isAdult :: User -> Bool
isAdult u = userAge u >= 18

-- =============================================================================
-- BAD: These functions violate layering
-- =============================================================================

-- | VIOLATION: Core layer calling DB layer directly
createUser :: String -> Int -> IO (Either ValidationError Int)
createUser name age = do
  let user = User 0 name age
  case validateUser user of
    Left err -> pure $ Left err
    Right _  -> do
      let dbUser = DBUser 0 name age  -- Converting to DB type in Core!
      uid <- saveUser dbUser  -- BAD: Direct DB call from Core
      pure $ Right uid

-- | VIOLATION: Core layer calling both DB and UI layers
processAndDisplay :: Int -> IO ()
processAndDisplay uid = do
  maybeUser <- loadUser uid       -- BAD: Direct DB call from Core
  case maybeUser of
    Nothing -> putStrLn "User not found"
    Just dbu -> do
      let uiUser = UIUser (dbUserId dbu) (dbUserName dbu) (dbUserAge dbu)
      putStrLn $ formatForDisplay uiUser  -- BAD: Direct UI call from Core

-- | VIOLATION: Core layer mixing concerns
getUserAndFormat :: Int -> IO String
getUserAndFormat uid = do
  maybeUser <- loadUser uid  -- BAD: DB call
  case maybeUser of
    Nothing -> pure "Not found"
    Just dbu -> pure $ formatForDisplay $  -- BAD: UI call
      UIUser (dbUserId dbu) (dbUserName dbu) (dbUserAge dbu)

-- =============================================================================
-- GOOD: Pure business logic (how it should be)
-- =============================================================================

-- | Pure function that doesn't depend on concrete layers
processUserPure :: User -> Either ValidationError Int
processUserPure u = do
  validUser <- validateUser u
  pure $ calculateScore validUser

-- | Pure transformation
userToScore :: User -> Maybe Int
userToScore u = case validateUser u of
  Left _ -> Nothing
  Right valid -> Just $ calculateScore valid
