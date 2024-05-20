{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Config (parse, Config (..), Variable (..), Signature (..)) where

import ClassyPrelude
import Data.Aeson
import qualified Data.HashMap.Strict as Map
import Data.Text (Text)
import qualified Data.Vector as Vector
import qualified Data.Yaml as Y
import GHC.Generics

data Config = Config
  { signatures :: [Signature],
    variables :: [Variable]
  }
  deriving (FromJSON, Show, Generic)

data Signature = Signature
  { from :: Text,
    to :: Text,
    within :: Maybe Text,
    msg :: Maybe Text
  }
  deriving (Show, Generic)

instance FromJSON Signature where
  parseJSON = withObject "signature" $ \v -> do
    from <- v .: "from"
    to <- v .: "to"
    within <- v .:? "within"
    msg <- v .:? "msg"
    return $ Signature from to within msg

data Variable = Variable
  { varType :: Text,
    varFrom :: Maybe Text,
    varTo :: Text,
    varMsg :: Maybe Text
  }
  deriving (Show, Generic)

instance FromJSON Variable where
  parseJSON = withObject "Variable" $ \v -> do
    varType <- v .: "type"
    from <- v .:? "from"
    to <- v .: "to"
    msg <- v .:? "msg"
    return $ Variable varType from to msg

parse :: FilePath -> IO Config
parse file = do
  Y.decodeFileThrow file
