--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}

--------------------------------------------------------------------------------

module Site.Posts
  ( postsRule,
    postsPattern,
    loadAllPosts,
    loadPosts,
    loadPostCtx,
  )
where

--------------------------------------------------------------------------------

import Control.Applicative (Alternative (..))
import Control.Monad (filterM)
import Data.List (intersperse)
import Data.Time (utctDay)
import Data.Time.Format (formatTime)
import Data.Time.Locale.Compat (TimeLocale, defaultTimeLocale)
import Site.Config
import Site.Core
import Site.Pandoc
import Text.Blaze.Html (toHtml)

--------------------------------------------------------------------------------

postsRule :: Tags -> Rules ()
postsRule tags = match postsPattern $ do
  route $ setExtension "html"
  compile $ do
    ctx <- loadPostCtx tags
    customPandocCompiler
      >>= saveSnapshot "content"
      >>= loadAndApplyTemplate "templates/post.html" ctx
      >>= relativizeUrls

--------------------------------------------------------------------------------

loadAllPosts :: UTCTime -> Compiler [Item String]
loadAllPosts = loadPosts postsPattern

loadPosts :: Pattern -> UTCTime -> Compiler [Item String]
loadPosts pat now =
  skipDrafts
    =<< skipAfter now
    =<< recentFirst
    =<< loadAll pat

skipAfter :: (MonadFail m, MonadMetadata m) => UTCTime -> [Item a] -> m [Item a]
skipAfter now =
  filterM $
    fmap (now >)
      . getItemUTC defaultTimeLocale
      . itemIdentifier

skipDrafts :: (MonadMetadata m) => [Item a] -> m [Item a]
skipDrafts = filterM publish
  where
    publish i =
      maybe True asFlag
        <$> getMetadataField (itemIdentifier i) "publish"
    asFlag "true" = True
    asFlag _ = False

--------------------------------------------------------------------------------

loadPostCtx :: Tags -> Compiler (Context String)
loadPostCtx tags =
  tagsField "tags" tags
    <+> rawTagsField "rawTags" tags
    <+> dateField "date" "%B %e, %Y"
    <+> updateField "update" "%B %e, %Y"
    <+> teaserField "teaser" "content"
    <+> loadAppCtx

--------------------------------------------------------------------------------

rawTagsField :: String -> Tags -> Context a
rawTagsField = tagsFieldWith getTags render (mconcat . intersperse ", ")
  where
    render _ Nothing = Nothing
    render tag _ = Just $ toHtml tag

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
