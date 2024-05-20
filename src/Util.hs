module Util
  ( trim,
    trimParens,
    splitRespectingParens,
    splitFunctionArgs,
    matches,
    dropLast,
    match,
    listHaskellFiles,
    checkLints,
    pPrint,
    replacerIgnoreUnderscore,
    writeToFile,
  )
where

import ClassyPrelude
import qualified Data.ByteString.Char8 as ByteString
import qualified Data.Text as Text
import Lint (Lint (..))
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath (takeExtension)
import qualified Text.Pretty.Simple as Print

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

splitFunctionArgs :: Text -> [Text]
splitFunctionArgs = fmap (Text.strip . pack) . splitRespectingParens "->" . unpack

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

-- Function to replace a specific part of a line in a file
checkLints :: FilePath -> [Lint] -> IO (String, [Lint])
checkLints filename lints = do
  contents <- readFile filename
  -- Util.pPrint lints
  let (newContent, checkedLints) =
        foldr
          ( \l@(Lint {lineNumber, from, to}) acc ->
              replaceInFile acc lineNumber from to l
          )
          (ByteString.unpack contents, [])
          lints
  pure (newContent, checkedLints)

writeToFile :: Bool -> FilePath -> Text -> IO ()
writeToFile printToCmd filename newContent =
  if printToCmd
    then Util.pPrint newContent
    else writeFile filename (ByteString.pack (unpack newContent))

replaceInFile :: (String, [Lint]) -> Int -> Text -> Text -> Lint -> (String, [Lint])
replaceInFile (acc, lints) lineNumber oldText newText lint =
  let linesOfFile = lines acc
   in if (lineNumber > 0 && lineNumber <= length linesOfFile)
        then
          let (before, line : after) = splitAt (lineNumber - 1) linesOfFile
           in if oldText `isInfixOf` (pack line)
                then
                  ( unlines (before ++ [unpack (replacerIgnoreUnderscore oldText newText (pack line))] ++ after),
                    lint : lints
                  )
                else (acc, lints)
        else (acc, lints)

replacerIgnoreUnderscore :: Text -> Text -> Text -> Text
replacerIgnoreUnderscore oldText newText line =
  if length oWords == length nWords
    then foldr replaceWord line (zip oWords nWords)
    else Text.replace oldText newText line
  where
    oWords = Text.words (trimParens oldText)
    nWords = Text.words (trimParens newText)
    replaceWord (o, n) acc
      | o == "_" = acc
      | "_" `isPrefixOf` o && not ("_" `isPrefixOf` n) = Text.replace o ("_" <> n) acc
      | otherwise = Text.replace o n acc
