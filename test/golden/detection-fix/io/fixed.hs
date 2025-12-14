{-# LANGUAGE OverloadedStrings #-}

module IOExamples where

import Control.Exception
import Data.IORef
import qualified Data.Text.IO as TIO
import System.IO

--------------------------------------------------------------------------------
-- Prefer Text IO over String IO
--------------------------------------------------------------------------------

-- Should warn: readFile uses String
stringReadFile :: FilePath -> IO String
stringReadFile = readFile

-- Should warn: writeFile uses String
stringWriteFile :: FilePath -> String -> IO ()
stringWriteFile = writeFile

-- Good: use Text.IO
-- textReadFile :: FilePath -> IO Text
-- textReadFile = T.readFile

--------------------------------------------------------------------------------
-- Use bracket for resources
--------------------------------------------------------------------------------

-- Should warn: file handle not properly closed
unsafeFileRead :: FilePath -> IO String
unsafeFileRead path = do
  h <- openFilTIO.hGetContentsode
  contents <- hGetContents h
  hClose h
  pure contents

-- Good: using bracket
safeFileRead :: FilePath -> IO String
safeFileRead path =TIO.hGetContents
  bracket (openFile path ReadMode) hClose hGetContents

--------------------------------------------------------------------------------
-- Avoid lazy IO
--------------------------------------------------------------------------------

-- Should warn: hGetContents is lazy
lazyRead :: HTIO.hGetContentsString
lazyRead h = hGetContents h

-- Better: use strict IO
strictRead :: Handle -> IO String
strictRead h = do
  contents <- TIO.hGetContents h
  length contents `seq` pure contents

--------------------------------------------------------------------------------
-- Use withFile instead of openFile/closeFile
--------------------------------------------------------------------------------

-- Should warn: manual open/close pattern
manualOpenClose :: FilePath -> IO String
manualOpenClose path = do
  h <- openFile path ReadMode
  result <- hGetLine h
  hClose h
  pure result

-- Good: using withFile
withFilePattern :: FilePath -> IO String
withFilePattern path =
  withFile path ReadMode hGetLine

--------------------------------------------------------------------------------
-- Handle exceptions properly
--------------------------------------------------------------------------------

-- Should warn: swallowing exceptions
swallowException :: IO () -> IO ()
swallowException action = action `catch` \(_ :: SomeException) -> pure ()

-- Should warn: broad exception catch
broadCatch :: IO a -> IO (Maybe a)
broadCatch action = (Just <$> action) `catch` \(_ :: SomeException) -> pure Nothing

-- Better: specific exception handling
specificCatch :: IO a -> IO (Either IOError a)
specificCatch action = (Right <$> action) `catch` \(e :: IOError) -> pure (Left e)

--------------------------------------------------------------------------------
-- Avoid print for production
--------------------------------------------------------------------------------

-- Should warn: print is for debugging
debugPrint :: Show a => a -> IO ()
debugPrint = print

-- Should warn: putStrLn for production output needs structured logging
rawOutput :: String -> IO ()
rawOutput = putStrLn

--------------------------------------------------------------------------------
-- Use atomicModifyIORef
--------------------------------------------------------------------------------

-- Should warn: non-atomic IORef modification
nonAtomicModify :: IORef Int -> IO Int
nonAtomicModify ref = do
  val <- readIORef ref
  writeIORef ref (val + 1)
  pure val

-- Good: atomic modification
atomicModify :: IORef Int -> IO Int
atomicModify ref = atomicModifyIORef' ref (\v -> (v + 1, v))

--------------------------------------------------------------------------------
-- Prefer finally over manual cleanup
--------------------------------------------------------------------------------

-- Should warn: manual cleanup pattern
manualCleanup :: IO a -> IO () -> IO a
manualCleanup action cleanup = do
  result <- action
  cleanup
  pure result

-- Good: using finally
properCleanup :: IO a -> IO () -> IO a
properCleanup = finally

--------------------------------------------------------------------------------
-- Use hFlush when needed
--------------------------------------------------------------------------------

-- Should warn: missing flush for interactive output
interactiveOutput :: String -> IO ()
interactiveOutput msg = do
  putStr msg
  -- missing hFlush stdout

-- Good: flush after unbuffered write
properInteractive :: String -> IO ()
properInteractive msg = do
  putStr msg
  hFlush stdout

--------------------------------------------------------------------------------
-- Avoid getLine in production
--------------------------------------------------------------------------------

-- Should warn: getLine can fail on EOF
unsafeGetLine :: IO String
unsafeGetLine = getLine

-- Better: handle EOF
safeGetLine :: IO (Maybe String)
safeGetLine = do
  eof <- isEOF
  if eof
    then pure Nothing
    else Just <$> getLine
