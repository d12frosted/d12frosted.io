--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

--------------------------------------------------------------------------------
module Config where

--------------------------------------------------------------------------------
import           ToContext

--------------------------------------------------------------------------------
import           Data.Text (Text)
import           Hakyll
import           Hakyll.Core.Compiler.Internal
import           Data.Aeson
import           GHC.Generics
import           Data.List (stripPrefix, isPrefixOf)
import           Data.Binary (Binary)
import qualified Data.ByteString.Lazy as BS
import qualified Data.Char as Char
import           Data.Monoid ((<>))

--------------------------------------------------------------------------------
data Config = Config {
  getSiteTitle :: String
} deriving (Generic, Show)

instance ToJSON Config where
  toEncoding = genericToEncoding customOptions

instance FromJSON Config where
  parseJSON = genericParseJSON customOptions

instance Binary Config

instance Writable Config where
  write fp item = BS.writeFile fp . encode . itemBody $ item

instance ToContext Config where
  toContext cfg =
    constField "siteTitle" (getSiteTitle cfg)

--------------------------------------------------------------------------------
customOptions = defaultOptions
  { fieldLabelModifier = stripCamelCasePrefix "get"
  }

--------------------------------------------------------------------------------
configCompiler :: Compiler (Item Config)
configCompiler = do
  body <- itemBody <$> getResourceLBS
  case decode body of
    Nothing -> compilerThrow ["Could not parse config"]
    Just config -> makeItem config

--------------------------------------------------------------------------------
appContext :: Config -> Context String
appContext config = toContext config <> defaultContext

--------------------------------------------------------------------------------
stripCamelCasePrefix :: String -> String -> String
stripCamelCasePrefix prefix label =
  case stripPrefix prefix label of
    Nothing    -> label
    Just (h:t) -> Char.toLower h : t
    Just t     -> t
