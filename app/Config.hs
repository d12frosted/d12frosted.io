--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

--------------------------------------------------------------------------------
module Config where

--------------------------------------------------------------------------------
import           ToContext

--------------------------------------------------------------------------------
import           Data.Aeson as Aeson
import           Data.Binary (Binary)
import qualified Data.ByteString.Lazy as BS
import qualified Data.Char as Char
import           Data.List (stripPrefix, isPrefixOf)
import           Data.Monoid ((<>))
import           Data.Text (Text)
import           GHC.Generics
import           Hakyll
import           Hakyll.Core.Compiler.Internal

--------------------------------------------------------------------------------
data Config
  = Config
  { getSiteTitle       :: String
  , getSiteUrl         :: String
  , getSiteKeywords    :: String
  , getGitHubUrl       :: String
  , getSourcesRoot     :: String
  , getAuthorName      :: String
  , getAuthorEmail     :: String
  , getFeedTitle       :: String
  , getFeedDescription :: String
  , getFeedSize        :: Int
  } deriving (Generic, Show)

instance ToJSON Config where
  toEncoding = genericToEncoding customOptions

instance FromJSON Config where
  parseJSON = genericParseJSON customOptions

instance Binary Config

instance Writable Config where
  write fp item = BS.writeFile fp . encode . itemBody $ item

instance ToContext Config where
  toContext cfg
    =  constField "siteTitle"    (getSiteTitle cfg)
    <> constField "siteUrl"      (getSiteUrl cfg)
    <> constField "siteKeywords" (getSiteKeywords cfg)
    <> constField "gitHubUrl"    (getGitHubUrl cfg)
    <> constField "sourcesRoot"  (getSourcesRoot cfg)

--------------------------------------------------------------------------------
customOptions :: Aeson.Options
customOptions = defaultOptions
  { fieldLabelModifier = stripCamelCasePrefix "get"
  }

--------------------------------------------------------------------------------
configCompiler :: Compiler (Item Config)
configCompiler = do
  body <- itemBody <$> getResourceLBS
  case eitherDecode body of
    Left msg     -> compilerThrow ["Could not parse config: " <> msg]
    Right config -> makeItem config

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
