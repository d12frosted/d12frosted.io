--------------------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings #-}

--------------------------------------------------------------------------------

module Site.RSS
  ( rssRule
  ) where

--------------------------------------------------------------------------------

import           Site.Config
import           Site.Core
import           Site.Posts

--------------------------------------------------------------------------------

rssRule :: Tags -> UTCTime -> Rules ()
rssRule tags now = create ["atom.xml"] $ do
  route idRoute
  compile $ do
    config  <- itemBody <$> loadConfig
    posts   <- (take . getFeedSize $ config) <$> loadAllPosts now
    feedCtx <- bodyField "description" <+> loadPostCtx tags
    renderAtom (feedConfiguration config) feedCtx posts

--------------------------------------------------------------------------------

feedConfiguration :: Config -> FeedConfiguration
feedConfiguration config
  = FeedConfiguration
  { feedTitle       = getFeedTitle config
  , feedDescription = getFeedDescription config
  , feedAuthorName  = getAuthorName config
  , feedAuthorEmail = getAuthorEmail config
  , feedRoot        = getSiteUrl config
  }

--------------------------------------------------------------------------------
