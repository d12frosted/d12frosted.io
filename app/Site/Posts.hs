--------------------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings #-}

--------------------------------------------------------------------------------

module Site.Posts
  ( postsRule
  , postsPattern
  , loadAllPosts
  , loadPosts
  , loadPostCtx
  ) where

--------------------------------------------------------------------------------

import           Site.Config
import           Site.Core
import           Site.Pandoc

--------------------------------------------------------------------------------

import           Control.Applicative     (Alternative (..))
import           Control.Monad           (filterM)
import           Data.List               (intersperse)
import           Data.Time               (utctDay)
import           Data.Time.Format        (formatTime)
import           Data.Time.Locale.Compat (TimeLocale, defaultTimeLocale)
import           Text.Blaze.Html         (toHtml)

--------------------------------------------------------------------------------

postsRule :: Tags -> Rules ()
postsRule tags = match postsPattern $ do
  route   $ setExtension "html"
  compile $ do
    ctx   <- loadPostCtx tags
    pandocMathCompiler
      >>= saveSnapshot "content"
      >>= loadAndApplyTemplate "templates/post.html" ctx
      >>= relativizeUrls

--------------------------------------------------------------------------------

loadAllPosts :: UTCTime -> Compiler [Item String]
loadAllPosts = loadPosts postsPattern

loadPosts :: Pattern -> UTCTime -> Compiler [Item String]
loadPosts pat now = skipAfter now =<< recentFirst =<< loadAll pat

skipAfter :: (MonadMetadata m) => UTCTime -> [Item a] -> m [Item a]
skipAfter now = filterM $ fmap (now >) .
  getItemUTC defaultTimeLocale . itemIdentifier

--------------------------------------------------------------------------------

loadPostCtx :: Tags -> Compiler (Context String)
loadPostCtx tags
  =   tagsField "tags" tags
  <+> rawTagsField "rawTags" tags
  <+> dateField "date" "%B %e, %Y"
  <+> updateField "update" "%B %e, %Y"
  <+> teaserField "teaser" "content"
  <+> loadAppCtx

--------------------------------------------------------------------------------

rawTagsField :: String -> Tags -> Context a
rawTagsField = tagsFieldWith getTags render (mconcat . intersperse ", ")
  where render _ Nothing = Nothing
        render tag _     = Just $ toHtml tag

updateField :: String -> String -> Context a
updateField = updateFieldWith defaultTimeLocale

updateFieldWith :: TimeLocale -> String -> String -> Context a
updateFieldWith locale key format = field key $ \i -> do
  createTime <- getItemUTC locale $ itemIdentifier i
  updateTime <- getItemModificationTime $ itemIdentifier i
  if utctDay createTime == utctDay updateTime
    then empty
    else pure $ formatTime locale format updateTime

--------------------------------------------------------------------------------

postsPattern :: Pattern
postsPattern = "posts/*"

--------------------------------------------------------------------------------
