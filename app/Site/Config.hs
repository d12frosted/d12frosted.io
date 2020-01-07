--------------------------------------------------------------------------------

{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

--------------------------------------------------------------------------------

module Site.Config
  ( Config(..)
  , configCompiler
  , configRule
  , loadConfig
  , loadAppCtx
  , appContext
  ) where

--------------------------------------------------------------------------------

import           Site.Core

--------------------------------------------------------------------------------

import           Data.Aeson                    as Aeson
import           Data.Binary                   (Binary)
import qualified Data.ByteString.Lazy          as BS
import qualified Data.Char                     as Char
import           Data.List                     (stripPrefix)
import           Data.String                   (IsString)
import           GHC.Generics
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
  write fp = BS.writeFile fp . encode . itemBody

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

stripCamelCasePrefix :: String -> String -> String
stripCamelCasePrefix prefix label =
  case stripPrefix prefix label of
    Nothing    -> label
    Just (h:t) -> Char.toLower h : t
    Just t     -> t

--------------------------------------------------------------------------------

configCompiler :: Compiler (Item Config)
configCompiler = do
  body <- itemBody <$> getResourceLBS
  case eitherDecode body of
    Left msg     -> compilerThrow ["Could not parse config: " <> msg]
    Right config -> makeItem config

--------------------------------------------------------------------------------

configRule :: Rules ()
configRule = match configPath (compile configCompiler)

--------------------------------------------------------------------------------

loadConfig :: Compiler (Item Config)
loadConfig = load configPath

loadAppCtx :: Compiler (Context String)
loadAppCtx = appContext <$> itemBody <$> loadConfig

appContext :: Config -> Context String
appContext config = toContext config <> defaultContext

--------------------------------------------------------------------------------

configPath :: IsString a => a
configPath = "assets/config.json"

--------------------------------------------------------------------------------
