--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}

--------------------------------------------------------------------------------
module Main where

--------------------------------------------------------------------------------
import           Config
import           ToContext

--------------------------------------------------------------------------------
import           Hakyll
import           Hakyll.Core.Compiler.Internal
import           Data.Monoid ((<>))
import           Data.List (intercalate, intersperse, sortBy)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import           Text.Blaze.Html (toHtml, toValue, (!))
import           Control.Monad (foldM, forM, forM_, mplus, filterM)
import           Data.Maybe (catMaybes, fromMaybe)
import           Text.Blaze.Html.Renderer.String (renderHtml)
import           Data.Time.Locale.Compat (defaultTimeLocale)
import           Data.Time (getCurrentTime, UTCTime)

--------------------------------------------------------------------------------
main :: IO ()
main = getCurrentTime >>= \now -> hakyll $ do
  match "configs/config.json" $ do
    compile configCompiler

  match "images/*" $ do
    route   idRoute
    compile copyFileCompiler

  match "css/*" $ do
    route   idRoute
    compile compressCssCompiler

  match "static/index.html" $ do
    route $ gsubRoute "static/" (const "")
    compile $ do
      ctx <- loadIndexCtx
      getResourceBody
        >>= loadAndApplyTemplate "templates/default.html" ctx
        >>= relativizeUrls

  tags <- buildTags postsPattern (fromCapture "tags/*.html")

  tagsRules tags $ \tag pat -> do
    route idRoute
    compile $ do
      posts   <- skipFuture now =<< recentFirst =<< loadAll pat
      postCtx <- loadPostCtx tags
      tagCtx <- loadTagCtx tag postCtx posts

      makeItem ""
          >>= loadAndApplyTemplate "templates/tag.html"     tagCtx
          >>= loadAndApplyTemplate "templates/blog.html"    tagCtx
          >>= loadAndApplyTemplate "templates/default.html" tagCtx
          >>= relativizeUrls

  match postsPattern $ do
    route $ setExtension "html"
    compile $ do
      ctx <- loadPostCtx tags
      pandocCompiler
        >>= loadAndApplyTemplate "templates/post-body.html" ctx
        >>= saveSnapshot "content"
        >>= loadAndApplyTemplate "templates/blog.html"      ctx
        >>= loadAndApplyTemplate "templates/default.html"   ctx
        >>= relativizeUrls

  create ["blog.html"] $ do
    route idRoute
    compile $ do
      posts   <- skipFuture now =<< recentFirst =<< loadAll postsPattern
      postCtx <- loadPostCtx tags
      archiveCtx <- loadArchiveCtx postCtx posts
      makeItem ""
        >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
        >>= loadAndApplyTemplate "templates/blog.html"    archiveCtx
        >>= loadAndApplyTemplate "templates/default.html" archiveCtx
        >>= relativizeUrls

  create ["atom.xml"] $ do
    route idRoute
    compile $ do
      config  <- itemBody <$> load "configs/config.json"
      posts   <- fmap (take . getFeedSize $ config) . skipFuture now
        =<< recentFirst
        =<< loadAllSnapshots postsPattern "content"
      feedCtx <- bodyField "description" <+> loadPostCtx tags
      renderAtom (feedConfiguration config) feedCtx posts

  match "templates/*" $ compile templateBodyCompiler

--------------------------------------------------------------------------------
loadCtx :: Compiler (Context String)
loadCtx = appContext <$> itemBody <$> load "configs/config.json"

loadIndexCtx :: Compiler (Context String)
loadIndexCtx = indexCtx <+> loadCtx

loadPostCtx :: Tags -> Compiler (Context String)
loadPostCtx tags
  =   blogCtx
  <+> tagsField "tags" tags
  <+> dateField "date" "%B %e, %Y"
  <+> loadCtx

loadArchiveCtx :: Context String -> [Item String] -> Compiler (Context String)
loadArchiveCtx ctx posts
  =   blogCtx
  <+> listField "posts" ctx (return posts)
  <+> loadCtx

loadTagCtx :: String -> Context String -> [Item String] -> Compiler (Context String)
loadTagCtx tag ctx posts
  =   constField "tag" tag
  <+> blogCtx
  <+> listField "posts" ctx (return posts)
  <+> loadCtx

indexCtx :: Context String
indexCtx = constField "group" "index"

blogCtx :: Context String
blogCtx = constField "group" "blog"

--------------------------------------------------------------------------------
postsPattern :: Pattern
postsPattern = "posts/*"

--------------------------------------------------------------------------------
skipFuture :: (MonadMetadata m) => UTCTime -> [Item a] -> m [Item a]
skipFuture now = filterM $ fmap (now >) .
  getItemUTC defaultTimeLocale . itemIdentifier

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
(<+>) :: (Monoid a, Applicative m) => a -> m a -> m a
a <+> ma = mappend a <$> ma
infixr 6 <+>
