{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -fplugin=RecordDotPreprocessor #-}

module Improve where

import qualified Api
import ClassyPrelude
import Data.Aeson (FromJSON)
import qualified Data.Aeson as Aeson
import Data.String.Interpolate (i)
import qualified Util

data ImproveResponse = ImproveResponse
  { code :: Text,
    changes :: [Text]
  }
  deriving (Show, Generic, FromJSON, Read)

run :: Text -> IO Text
run content = do
  let prompt = toPrompt content
  res <- Api.call prompt
  let resContent = maybe (tshow content) (.response) res
  Util.pPrint (extractJson resContent)
  pure (maybe (tshow content) (.code) (extractJson resContent))

toPrompt :: Text -> Text
toPrompt content =
  [i| You are an expert programmer that helps to improve code where possible.
      I'm going to provide a piece of Haskell code to you at the end.
      Here is a list of things to focus on:
      - prefer where clauses over lets for variables.
      - use >>= over do syntax where possible
      - if a function has more then 15 lines in the body, please split it in multiple functions.
      - simplify the code if possible, you can split functions if it improves readability.
      Here are some things to absolutely avoid:
      - Rename anything, like variables, functions, types etc.
      - Remove any arguments of functions.
      - Add imports for modules, all imports are correct.
      - Add pragma's to the module, all pragma's are correct.
      - Add types to the module, all types are already defined.
      Return the code in a JSON object with 2 keys "code" as text and
      "changes" you made to the original code as a list of text.
      I only want to see you report changed you actually made. Not the onces you concidered.
      Here is the code to improve:
      #{content}
  |]

extractJson :: Text -> Maybe ImproveResponse
extractJson input =
  let linesList = lines input
      jsonLines = dropWhile (not . isPrefixOf "```json") linesList
      jsonContent = takeWhile (not . isPrefixOf "```") (drop 1 jsonLines)
   in Aeson.decode (fromStrict (encodeUtf8 (unlines jsonContent)))
