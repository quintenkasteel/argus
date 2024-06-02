{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -fno-warn-ambiguous-fields #-}
{-# OPTIONS_GHC -fplugin=RecordDotPreprocessor #-}

module Api (call, RequestBody (..)) where

import ClassyPrelude
import Data.Aeson
import Network.HTTP.Client.Conduit hiding (RequestBody)
import Network.HTTP.Simple

data RequestBody = RequestBody
  { model :: Text,
    prompt :: Text,
    stream :: Bool
  }
  deriving (Show)

instance ToJSON RequestBody where
  toJSON (RequestBody model prompt stream) =
    object
      [ "model" .= model,
        "prompt" .= prompt,
        "stream" .= stream
      ]

data ApiResponse = ApiResponse
  { model :: Text,
    createdAt :: Text,
    response :: Text,
    done :: Bool,
    doneReason :: Text
  }
  deriving (Show)

instance FromJSON ApiResponse where
  parseJSON = withObject "ApiResponse" $ \v ->
    ApiResponse
      <$> v .: "model"
      <*> v .: "created_at"
      <*> v .: "response"
      <*> v .: "done"
      <*> v .: "done_reason"

call :: Text -> IO (Maybe ApiResponse)
call promptText = do
  let requestBody =
        RequestBody
          { model = "phind-codellama",
            prompt = promptText,
            stream = False
          }
      request =
        setRequestBodyLBS (encode requestBody)
          . setRequestMethod "POST"
          . setRequestPath "/api/generate"
          . setRequestPort 11434
          . setRequestResponseTimeout responseTimeoutNone
          $ setRequestHost "localhost" defaultRequest
  response <- httpLBS request
  let responseBody = getResponseBody response
  pure (decode responseBody)
