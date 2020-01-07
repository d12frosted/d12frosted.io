--------------------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings #-}

--------------------------------------------------------------------------------

module Site.Index
  ( indexRule
  ) where

--------------------------------------------------------------------------------

import           Site.About
import           Site.Config
import           Site.Core
import           Site.Posts

--------------------------------------------------------------------------------

indexRule :: Tags -> UTCTime -> Rules ()
indexRule tags now = match "templates/index.html" $ do
  route (constRoute "index.html")
  compile $ do
    about      <- loadAbout
    posts      <- loadAllPosts now
    postCtx    <- loadPostCtx tags
    indexCtx   <- loadIndexCtx postCtx posts about

    getResourceBody
      >>= applyAsTemplate indexCtx
      >>= relativizeUrls

--------------------------------------------------------------------------------

loadIndexCtx :: Context String
            -> [Item String]
            -> Item String
            -> Compiler (Context String)
loadIndexCtx postCtx posts about
  =   listField "posts" postCtx (return posts)
  <+> field "about" (const . return . itemBody $ about)
  <+> loadAppCtx

--------------------------------------------------------------------------------
