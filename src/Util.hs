{-# OPTIONS_GHC -fplugin=RecordDotPreprocessor #-}

module Util
  ( trim,
    trimParens,
    splitRespectingParens,
    matches,
    dropLast,
    match,
    listHaskellFiles,
    pPrint,
    replacerIgnoreUnderscore,
    writeToFile,
    debug,
    findIndex,
    runWithTimeouts,
  )
where

import ClassyPrelude
import Control.Concurrent (forkIO, killThread, threadDelay)
import qualified Data.ByteString.Char8 as ByteString
import qualified Data.Text as Text
import Debug.Trace as Trace
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath (takeExtension)
import System.IO (hPutStrLn)
import qualified Text.Pretty.Simple as Print

debug :: String -> a -> a
debug = Trace.trace

pPrint :: Show a => a -> IO ()
pPrint = Print.pPrint

-- Remove leading and trailing whitespace
trim :: String -> String
trim = f . f
  where
    f = reverse . dropWhile ((==) ' ')

-- Remove leading and trailing whitespace
trimParens :: Text -> Text
trimParens = f . f
  where
    f = reverse . Text.dropWhile (\v -> v == '(' || v == ')')

-- Helper function to split a string by a separator while respecting nested parentheses
splitRespectingParens :: String -> String -> [String]
splitRespectingParens sep str = go str (0 :: Int) ""
  where
    go [] _ acc = [reverse (trim acc)]
    go s@(c : cs) depth acc
      | Just rest <- stripPrefix sep s, depth == 0 = reverse (trim acc) : go rest 0 ""
      | c == '(' = go cs (depth + 1) (c : acc)
      | c == ')' = go cs (depth - 1) (c : acc)
      | otherwise = go cs depth (c : acc)

-- Function to match a string with wildcard and ignore characters
matches :: String -> String -> Bool
matches [] [] = True
matches ('*' : ps) str = any (matches ps) (suffixes str)
matches ('_' : ps) (_ : ss) = matches ps ss
matches (p : ps) (s : ss) = p == s && matches ps ss
matches _ _ = False

-- Generate all suffixes of a string (including the string itself and the empty suffix)
suffixes :: String -> [String]
suffixes [] = [[]]
suffixes str@(_ : xs) = str : suffixes xs

-- Main function to match patterns with wildcards and ignores
match :: Text -> Text -> Bool
match pattern str = matchParts (words (unpack pattern)) (words (unpack str))
  where
    matchParts [] [] = True
    matchParts (p : ps) (s : ss) = matches p s && matchParts ps ss
    matchParts _ _ = False

dropLast :: [a] -> [a]
dropLast [] = []
dropLast [_] = []
dropLast (x : xs) = x : dropLast xs

-- Recursively list Haskell files in a directory
listHaskellFiles :: FilePath -> IO [FilePath]
listHaskellFiles dir = do
  contents <- listDirectory dir
  files <-
    concat
      <$> mapM
        ( \path -> do
            let fullPath = dir </> path
            isDir <- doesDirectoryExist fullPath
            if isDir
              then listHaskellFiles fullPath
              else return [fullPath | takeExtension fullPath == ".hs"]
        )
        contents
  return files

writeToFile :: FilePath -> Text -> IO ()
writeToFile filename newContent =
  writeFile filename (ByteString.pack (unpack newContent))

replacerIgnoreUnderscore :: Text -> Text -> Text -> Text
replacerIgnoreUnderscore from to line =
  if length oWords == length nWords
    then foldr replaceWord line (zip oWords nWords)
    else Text.replace from to line
  where
    oWords = Text.words (trimParens from)
    nWords = Text.words (trimParens to)
    replaceWord (o, n) acc
      | o == "_" = acc
      | "_" `isPrefixOf` o && not ("_" `isPrefixOf` n) = Text.replace o ("_" <> n) acc
      | otherwise = Text.replace o n acc

-- | Find the index of the first occurrence of an element in a list
findIndex :: (a -> Bool) -> [a] -> Maybe Int
findIndex p xs = case filter (p . snd) (zip [0 ..] xs) of
  ((i, _) : _) -> Just i
  [] -> Nothing

-- | Run an IO action with timeout notifications
runWithTimeouts :: IO a -> [(Int, Text)] -> IO a
runWithTimeouts action timeouts = do
  done <- newEmptyMVar
  -- Create a channel for timeout messages
  chan <- newChan
  -- Start a separate thread for each timeout message
  threads <- forM timeouts $ \(seconds, message) -> forkIO $ do
    threadDelay (seconds * 1000000) -- Convert seconds to microseconds
    writeChan chan message
  -- Start a thread to read from the channel and print messages
  loggerThread <-
    forkIO $
      let loop = do
            message <- readChan chan
            notDone <- tryPutMVar done ()
            if notDone
              then hPutStrLn stderr (unpack message) >> loop
              else return ()
       in loop
  -- Perform the main action
  result <- action
  -- Signal that the action is complete
  putMVar done ()
  -- Cleanup all timeout threads
  forM_ threads killThread
  -- Kill the logger thread
  killThread loggerThread
  return result
