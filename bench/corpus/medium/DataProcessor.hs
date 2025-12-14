{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Medium complexity data processing module
module DataProcessor where

import Data.List
import Data.Maybe
import Data.Text (Text)
import Data.Text qualified as T
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map

-- | User record type
data User = User
  { userId :: Int
  , userName :: Text
  , userEmail :: Text
  , userAge :: Int
  , userActive :: Bool
  }

-- | Process user data
processUser :: User -> Maybe User
processUser user@User{..}
  | userAge < 0 = Nothing
  | T.null userName = Nothing
  | T.null userEmail = Nothing
  | otherwise = Just user

-- | Filter active users
filterActive :: [User] -> [User]
filterActive = filter userActive

-- | Group users by age
groupByAge :: [User] -> Map Int [User]
groupByAge users = foldr insertUser Map.empty users
  where
    insertUser user = Map.insertWith (++) (userAge user) [user]

-- | Get oldest user (uses partial head)
getOldest :: [User] -> User
getOldest users = head $ sortBy (flip compare `on` userAge) users
  where on f g x y = f (g x) (g y)

-- | Calculate average age
averageAge :: [User] -> Double
averageAge [] = 0
averageAge users =
  let total = sum $ map userAge users
      count = length users
  in fromIntegral total / fromIntegral count

-- | Find user by ID (uses fromJust)
findUser :: Int -> [User] -> User
findUser uid users = fromJust $ find (\u -> userId u == uid) users

-- | Update user name
updateName :: Text -> User -> User
updateName newName user = user { userName = newName }

-- | Validate email format
validateEmail :: Text -> Bool
validateEmail email = T.elem '@' email && T.elem '.' email

-- | Process batch of users
processBatch :: [User] -> ([User], [User])
processBatch users = (valid, invalid)
  where
    results = map processUser users
    valid = catMaybes results
    invalid = [u | (u, Nothing) <- zip users results]

-- | Create user summary
summarize :: [User] -> Text
summarize users = T.unlines
  [ "Total users: " <> T.pack (show $ length users)
  , "Active users: " <> T.pack (show $ length $ filterActive users)
  , "Average age: " <> T.pack (show $ averageAge users)
  ]

-- | Sort users by name
sortByName :: [User] -> [User]
sortByName = sortBy (compare `on` userName)
  where on f g x y = f (g x) (g y)

-- | Get top N users by ID
topUsers :: Int -> [User] -> [User]
topUsers n = take n . sortBy (compare `on` userId)
  where on f g x y = f (g x) (g y)

-- | Check if user exists
userExists :: Int -> [User] -> Bool
userExists uid = any (\u -> userId u == uid)

-- | Remove duplicates by ID
uniqueUsers :: [User] -> [User]
uniqueUsers = nubBy (\a b -> userId a == userId b)

-- | Partition by age threshold
partitionByAge :: Int -> [User] -> ([User], [User])
partitionByAge threshold = partition (\u -> userAge u >= threshold)

-- | Count users by status
countByStatus :: [User] -> (Int, Int)
countByStatus users = (length active, length inactive)
  where (active, inactive) = partition userActive users

-- | Get user emails
getEmails :: [User] -> [Text]
getEmails = map userEmail

-- | Validate all users
validateAll :: [User] -> Bool
validateAll = all isValid
  where isValid User{..} = userAge >= 0 && not (T.null userName)

-- | Transform user list
transformUsers :: (User -> User) -> [User] -> [User]
transformUsers = map

-- | Filter users by predicate
filterUsers :: (User -> Bool) -> [User] -> [User]
filterUsers = filter

-- | Combine two user lists
combineUsers :: [User] -> [User] -> [User]
combineUsers = (++)

-- | Get user count
userCount :: [User] -> Int
userCount = length

-- | Check if empty
isEmpty :: [User] -> Bool
isEmpty = null
