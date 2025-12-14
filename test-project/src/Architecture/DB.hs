-- | Database layer
-- This layer should NOT depend on UI layer
-- It CAN depend on Core layer for types
module Architecture.DB where

-- =============================================================================
-- Database types (duplicated from Core - code smell)
-- =============================================================================

-- | Duplicated User type - code smell indicating architecture problem
-- In a proper setup, this would come from Core
data DBUser = DBUser
  { dbUserId   :: Int
  , dbUserName :: String
  , dbUserAge  :: Int
  } deriving (Show, Eq)

-- =============================================================================
-- Database operations
-- =============================================================================

-- | Simulated database storage
userDatabase :: [(Int, DBUser)]
userDatabase =
  [ (1, DBUser 1 "Alice" 30)
  , (2, DBUser 2 "Bob" 25)
  , (3, DBUser 3 "Charlie" 35)
  ]

-- | Save user to database (simulated)
-- Note: In bad architecture, this might call UI functions directly
saveUser :: DBUser -> IO Int
saveUser u = do
  -- BAD PRACTICE: DB layer printing to console (UI concern)
  putStrLn $ "[DB] Saving user: " ++ dbUserName u
  let newId = length userDatabase + 1
  putStrLn $ "[DB] Saved user with ID: " ++ show newId
  pure newId

-- | Load user from database (simulated)
loadUser :: Int -> IO (Maybe DBUser)
loadUser uid = do
  -- BAD PRACTICE: DB layer printing to console (UI concern)
  putStrLn $ "[DB] Loading user ID: " ++ show uid
  pure $ lookup uid userDatabase

-- | Delete user (simulated)
deleteUser :: Int -> IO Bool
deleteUser uid = do
  -- BAD PRACTICE: DB layer printing to console
  putStrLn $ "[DB] Deleting user ID: " ++ show uid
  pure $ uid `elem` map fst userDatabase

-- =============================================================================
-- GOOD: Pure database operations
-- =============================================================================

-- | Pure lookup without side effects
lookupUserPure :: Int -> Maybe DBUser
lookupUserPure uid = lookup uid userDatabase

-- | Query interface that just returns data
queryUsers :: (DBUser -> Bool) -> [DBUser]
queryUsers predicate = filter predicate (map snd userDatabase)

-- | Get all user IDs
allUserIds :: [Int]
allUserIds = map fst userDatabase
