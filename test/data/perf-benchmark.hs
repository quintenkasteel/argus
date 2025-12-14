{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

-- |
-- Module      : PerfBenchmark
-- Description : Realistic Haskell code for performance benchmarking
-- Copyright   : (c) 2024
-- License     : MIT
--
-- This module contains realistic Haskell code patterns for
-- benchmarking Argus performance. It includes:
--
-- * Various function definitions with different complexity levels
-- * Common Haskell patterns (maps, folds, filters)
-- * Type definitions with deriving clauses
-- * Import statements
-- * Documentation comments
-- * Some intentional issues for detection
module PerfBenchmark
  ( -- * Data types
    User (..)
  , Post (..)
  , Comment (..)
  , Tag (..)

    -- * User operations
  , createUser
  , updateUser
  , deleteUser
  , findUser
  , listUsers

    -- * Post operations
  , createPost
  , updatePost
  , deletePost
  , findPost
  , listPosts
  , postsWithTag

    -- * Comment operations
  , addComment
  , deleteComment
  , listComments

    -- * Analysis functions
  , analyzeEngagement
  , calculateScore
  , rankPosts
  , generateReport

    -- * Utility functions
  , validateEmail
  , sanitizeInput
  , formatDate
  , parseDate
  ) where

import Control.Monad (forM, when, unless)
import Data.Char (isAlphaNum, isDigit, toLower)
import Data.List (sort, sortBy, nub, elem, find, filter, foldl, group)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromJust, fromMaybe, catMaybes, mapMaybe, isJust, isNothing)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (UTCTime, getCurrentTime, addUTCTime)
import GHC.Generics (Generic)

--------------------------------------------------------------------------------
-- Data Types
--------------------------------------------------------------------------------

-- | User account
data User = User
  { userId        :: Int
  , userName      :: Text
  , userEmail     :: Text
  , userCreatedAt :: UTCTime
  , userActive    :: Bool
  }
  deriving stock (Show, Eq, Ord, Generic)

-- | Blog post
data Post = Post
  { postId        :: Int
  , postAuthorId  :: Int
  , postTitle     :: Text
  , postContent   :: Text
  , postTags      :: [Tag]
  , postCreatedAt :: UTCTime
  , postViews     :: Int
  , postLikes     :: Int
  }
  deriving stock (Show, Eq, Ord, Generic)

-- | Comment on a post
data Comment = Comment
  { commentId        :: Int
  , commentPostId    :: Int
  , commentAuthorId  :: Int
  , commentContent   :: Text
  , commentCreatedAt :: UTCTime
  }
  deriving stock (Show, Eq, Ord, Generic)

-- | Tag for categorizing posts
data Tag = Tag
  { tagId   :: Int
  , tagName :: Text
  }
  deriving stock (Show, Eq, Ord, Generic)

--------------------------------------------------------------------------------
-- User Operations
--------------------------------------------------------------------------------

-- | Create a new user
createUser :: Text -> Text -> IO User
createUser name email = do
  now <- getCurrentTime
  -- Validation with potential issues
  let validEmail = validateEmail email
  when (not validEmail) $
    error "Invalid email address"

  -- Generate ID (simplified)
  let uid = fromJust $ Just (hash name)  -- Intentional fromJust for detection

  pure User
    { userId = uid
    , userName = name
    , userEmail = email
    , userCreatedAt = now
    , userActive = True
    }

-- | Update user information
updateUser :: Int -> Text -> Text -> [User] -> [User]
updateUser uid name email users =
  map updateIfMatch users
  where
    updateIfMatch user
      | userId user == uid = user
          { userName = name
          , userEmail = email
          }
      | otherwise = user

-- | Delete a user
deleteUser :: Int -> [User] -> [User]
deleteUser uid users =
  filter (\u -> userId u /= uid) users

-- | Find a user by ID
findUser :: Int -> [User] -> Maybe User
findUser uid users =
  find (\u -> userId u == uid) users

-- | List all active users
listUsers :: [User] -> [User]
listUsers users =
  filter userActive users

--------------------------------------------------------------------------------
-- Post Operations
--------------------------------------------------------------------------------

-- | Create a new post
createPost :: Int -> Text -> Text -> [Tag] -> IO Post
createPost authorId title content tags = do
  now <- getCurrentTime
  -- Generate ID
  let pid = hash title

  pure Post
    { postId = pid
    , postAuthorId = authorId
    , postTitle = title
    , postContent = content
    , postTags = tags
    , postCreatedAt = now
    , postViews = 0
    , postLikes = 0
    }

-- | Update post content
updatePost :: Int -> Text -> Text -> [Post] -> [Post]
updatePost pid title content posts =
  map updateIfMatch posts
  where
    updateIfMatch post
      | postId post == pid = post
          { postTitle = title
          , postContent = content
          }
      | otherwise = post

-- | Delete a post
deletePost :: Int -> [Post] -> [Post]
deletePost pid posts =
  filter (\p -> postId p /= pid) posts

-- | Find a post by ID
findPost :: Int -> [Post] -> Maybe Post
findPost pid posts =
  find (\p -> postId p == pid) posts

-- | List all posts
listPosts :: [Post] -> [Post]
listPosts = id

-- | Find posts with a specific tag
postsWithTag :: Tag -> [Post] -> [Post]
postsWithTag tag posts =
  filter hasTag posts
  where
    hasTag post = tag `elem` postTags post  -- Intentional O(n) elem for detection

--------------------------------------------------------------------------------
-- Comment Operations
--------------------------------------------------------------------------------

-- | Add a comment to a post
addComment :: Int -> Int -> Text -> IO Comment
addComment postId authorId content = do
  now <- getCurrentTime
  let cid = hash content

  pure Comment
    { commentId = cid
    , commentPostId = postId
    , commentAuthorId = authorId
    , commentContent = content
    , commentCreatedAt = now
    }

-- | Delete a comment
deleteComment :: Int -> [Comment] -> [Comment]
deleteComment cid comments =
  filter (\c -> commentId c /= cid) comments

-- | List comments for a post
listComments :: Int -> [Comment] -> [Comment]
listComments pid comments =
  filter (\c -> commentPostId c == pid) comments

--------------------------------------------------------------------------------
-- Analysis Functions
--------------------------------------------------------------------------------

-- | Analyze user engagement
analyzeEngagement :: [User] -> [Post] -> [Comment] -> Map.Map Int Double
analyzeEngagement users posts comments =
  let userIds = map userId users
      scores = map (calculateUserScore posts comments) userIds
  in Map.fromList $ zip userIds scores
  where
    calculateUserScore :: [Post] -> [Comment] -> Int -> Double
    calculateUserScore ps cs uid =
      let userPosts = filter (\p -> postAuthorId p == uid) ps
          userComments = filter (\c -> commentAuthorId c == uid) cs
          postScore = sum $ map postViews userPosts
          commentScore = length userComments * 5
      in fromIntegral (postScore + commentScore)

-- | Calculate engagement score for a post
calculateScore :: Post -> [Comment] -> Double
calculateScore post comments =
  let postComments = filter (\c -> commentPostId c == postId post) comments
      viewScore = fromIntegral (postViews post) * 0.1
      likeScore = fromIntegral (postLikes post) * 1.0
      commentScore = fromIntegral (length postComments) * 2.0
  in viewScore + likeScore + commentScore

-- | Rank posts by engagement
rankPosts :: [Post] -> [Comment] -> [Post]
rankPosts posts comments =
  let scored = map (\p -> (p, calculateScore p comments)) posts
      sorted = sortBy compareScore scored
  in map fst sorted
  where
    compareScore (_, s1) (_, s2) = compare s2 s1

-- | Generate engagement report
generateReport :: [User] -> [Post] -> [Comment] -> Text
generateReport users posts comments =
  let totalUsers = length users
      totalPosts = length posts
      totalComments = length comments
      activeUsers = length $ filter userActive users
      topPosts = take 10 $ rankPosts posts comments

      header = "Engagement Report\n==================\n\n"
      stats = T.pack $
        "Total Users: " <> show totalUsers <> "\n" <>
        "Active Users: " <> show activeUsers <> "\n" <>
        "Total Posts: " <> show totalPosts <> "\n" <>
        "Total Comments: " <> show totalComments <> "\n\n"

      topPostsText = "Top 10 Posts:\n" <>
        T.unlines (map formatPost topPosts)
  in header <> stats <> topPostsText
  where
    formatPost post =
      "- " <> postTitle post <> " (Views: " <>
      T.pack (show (postViews post)) <> ", Likes: " <>
      T.pack (show (postLikes post)) <> ")"

--------------------------------------------------------------------------------
-- Utility Functions
--------------------------------------------------------------------------------

-- | Validate email format
validateEmail :: Text -> Bool
validateEmail email =
  let emailStr = T.unpack email
      parts = T.split (== '@') email
  in length parts == 2 && all isValidPart parts
  where
    isValidPart part = T.length part > 0

-- | Sanitize user input
sanitizeInput :: Text -> Text
sanitizeInput input =
  T.filter isAllowed input
  where
    isAllowed c = isAlphaNum c || c `elem` [' ', '.', ',', '!', '?']

-- | Format date as ISO 8601
formatDate :: UTCTime -> Text
formatDate time =
  T.pack $ show time  -- Simplified

-- | Parse date string
parseDate :: Text -> Maybe UTCTime
parseDate dateStr =
  -- Simplified parsing
  Nothing

--------------------------------------------------------------------------------
-- Helper Functions
--------------------------------------------------------------------------------

-- | Simple hash function for demonstration
hash :: Text -> Int
hash text =
  let chars = T.unpack text
      values = map fromEnum chars
  in foldl (\acc x -> acc * 31 + x) 0 values

-- | Get unique tags from posts
uniqueTags :: [Post] -> [Tag]
uniqueTags posts =
  let allTags = concatMap postTags posts  -- concat . map pattern for detection
      sorted = sort allTags
      grouped = group sorted
  in map head grouped  -- Intentional head for detection

-- | Count posts by tag
countByTag :: [Post] -> Map.Map Tag Int
countByTag posts =
  let tags = concatMap postTags posts
      counts = map (\t -> (t, countTag t posts)) (nub tags)  -- nub for detection
  in Map.fromList counts
  where
    countTag tag ps = length $ filter (hasTag tag) ps
    hasTag tag post = tag `elem` postTags post

-- | Filter posts by date range
filterByDateRange :: UTCTime -> UTCTime -> [Post] -> [Post]
filterByDateRange start end posts =
  filter inRange posts
  where
    inRange post =
      let created = postCreatedAt post
      in created >= start && created <= end

-- | Paginate results
paginate :: Int -> Int -> [a] -> [a]
paginate page pageSize items =
  let offset = page * pageSize
  in take pageSize $ drop offset items

-- | Search posts by title
searchPosts :: Text -> [Post] -> [Post]
searchPosts query posts =
  filter matches posts
  where
    matches post =
      let title = T.toLower (postTitle post)
          q = T.toLower query
      in q `T.isInfixOf` title

-- | Get most popular tags
popularTags :: Int -> [Post] -> [Tag]
popularTags n posts =
  let tagCounts = countByTag posts
      sorted = sortBy compareCount (Map.toList tagCounts)
      topN = take n sorted
  in map fst topN
  where
    compareCount (_, c1) (_, c2) = compare c2 c1

-- | Calculate average engagement
averageEngagement :: [Post] -> [Comment] -> Double
averageEngagement posts comments =
  if null posts
    then 0.0
    else
      let scores = map (\p -> calculateScore p comments) posts
          total = sum scores
      in total / fromIntegral (length posts)

-- | Find trending posts (high engagement in last 24 hours)
findTrendingPosts :: UTCTime -> [Post] -> [Comment] -> [Post]
findTrendingPosts now posts comments =
  let yesterday = addUTCTime (-86400) now
      recentPosts = filterByDateRange yesterday now posts
      ranked = rankPosts recentPosts comments
  in take 5 ranked

-- | Detect duplicate content
detectDuplicates :: [Post] -> [(Post, Post)]
detectDuplicates posts =
  let pairs = [(p1, p2) | p1 <- posts, p2 <- posts, postId p1 < postId p2]
      duplicates = filter isDuplicate pairs
  in duplicates
  where
    isDuplicate (p1, p2) =
      similarity (postContent p1) (postContent p2) > 0.8

-- | Calculate text similarity (simplified)
similarity :: Text -> Text -> Double
similarity t1 t2 =
  let w1 = Set.fromList $ T.words t1
      w2 = Set.fromList $ T.words t2
      intersection = Set.size $ Set.intersection w1 w2
      union = Set.size $ Set.union w1 w2
  in if union == 0
     then 0.0
     else fromIntegral intersection / fromIntegral union
