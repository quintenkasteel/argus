{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-ambiguous-fields #-}
{-# OPTIONS_GHC -fplugin=RecordDotPreprocessor #-}

module Config
  ( parse,
    argParser,
    Config (..),
    Variable (..),
    Signature (..),
  )
where

import ClassyPrelude
import Data.Aeson (FromJSON, (.!=), (.:), (.:?))
import qualified Data.Aeson as Aeson
import qualified Data.Yaml as Yaml
import Options.Applicative

data Config = Config
  { signatures :: [Signature],
    variables :: [Variable],
    inPlace :: Bool,
    directory :: FilePath
  }
  deriving (Show, Generic)

instance FromJSON Config where
  parseJSON = Aeson.withObject "Config" $ \o -> do
    signatures <- o .:? "signatures" .!= []
    variables <- o .:? "variables" .!= []
    inPlace <- o .:? "in-place" .!= False
    directory <- o .:? "directory" .!= ""
    pure (Config {..})

data Args = Args
  { inPlace :: Bool,
    directory :: FilePath
  }
  deriving (FromJSON, Show, Generic)

argParser :: IO Args
argParser = do
  execParser opts
  where
    parser = do
      inPlace <-
        switch
          ( long "in-place"
              <> short 'i'
              <> help "Flag to indicate if operation should be in place"
          )
      directory <- argument str (metavar "FILE")
      pure (Args {..})
    opts =
      info
        (parser <**> helper)
        ( fullDesc
            <> progDesc "Example program with in-place option"
            <> header "header for example program"
        )

data Signature = Signature
  { from :: Text,
    to :: Text,
    within :: Maybe Text,
    msg :: Maybe Text
  }
  deriving (Show, Generic)

instance FromJSON Signature where
  parseJSON = Aeson.withObject "signature" $ \v -> do
    from <- v .: "from"
    to <- v .: "to"
    within <- v .:? "within"
    msg <- v .:? "msg"
    return $ Signature from to within msg

data Variable = Variable
  { type_ :: Text,
    from :: Maybe Text,
    to :: Text,
    msg :: Maybe Text
  }
  deriving (Show, Generic)

instance FromJSON Variable where
  parseJSON = Aeson.withObject "Variable" $ \v -> do
    type_ <- v .: "type"
    from <- v .:? "from"
    to <- v .: "to"
    msg <- v .:? "msg"
    return $ Variable type_ from to msg

merge :: Config -> Args -> Config
merge (Config {..}) args =
  Config
    { signatures = signatures,
      variables = variables,
      inPlace = if not inPlace then args.inPlace else inPlace,
      directory = if null directory then args.directory else directory
    }

parse :: FilePath -> IO Config
parse file = do
  args <- argParser
  config <- Yaml.decodeFileThrow file
  pure (merge config args)
