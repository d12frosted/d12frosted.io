--------------------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings #-}

--------------------------------------------------------------------------------

module Site.Tags
  ( tagsRule
  ) where

--------------------------------------------------------------------------------

import           Site.About
import           Site.Config
import           Site.Core
import           Site.Posts

--------------------------------------------------------------------------------

tagsRule :: Tags -> UTCTime -> Rules ()
tagsRule tags now = tagsRules tags $ \tag pat -> do
  route idRoute
  compile $ do
    about   <- loadAbout
    posts   <- loadPosts pat now
    postCtx <- loadPostCtx tags
    tagCtx  <- loadTagCtx tag postCtx posts about

    makeItem ""
      >>= loadAndApplyTemplate "templates/tag.html" tagCtx
      >>= relativizeUrls

--------------------------------------------------------------------------------

loadTagCtx :: String
           -> Context String
           -> [Item String]
           -> Item String
           -> Compiler (Context String)
loadTagCtx tag ctx posts about
  =   constField "tag" tag
  <+> listField "posts" ctx (return posts)
  <+> field "about" (const . return . itemBody $ about)
  <+> loadAppCtx

--------------------------------------------------------------------------------
