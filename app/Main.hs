--------------------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings #-}

--------------------------------------------------------------------------------

module Main (main) where

--------------------------------------------------------------------------------

import           Config

--------------------------------------------------------------------------------

import           Control.Applicative     (Alternative (..))
import           Control.Monad           (filterM)
import           Data.List               (intersperse)
import           Data.Time               (UTCTime, getCurrentTime, utctDay)
import           Data.Time.Format        (formatTime)
import           Data.Time.Locale.Compat (TimeLocale, defaultTimeLocale)
import           Hakyll
import           Text.Blaze.Html         (toHtml)

--------------------------------------------------------------------------------

hakyllConfig :: Configuration
hakyllConfig = defaultConfiguration {
  destinationDirectory = "public"
}


main :: IO ()
main = getCurrentTime >>= \now -> hakyllWith hakyllConfig $ do
  match "assets/config.json" $ do
    compile configCompiler

  match "assets/images/*" $ do
    route assetsRoute
    compile copyFileCompiler

  match "node_modules/**" $ do
    route nodeRoute
    compile copyFileCompiler

  match ("css/*" .||. "css/**/*") $ do
    route   idRoute
    compile compressCssCompiler

  create ["css/index-bundle.css"] $ do
    route idRoute
    compile $ do
      commonCss <- loadAll "css/common/*"
      indexCss  <- loadAll "css/index/*"
      let styleCtx = listField
                     "items"
                     defaultContext
                     (return $ commonCss <> indexCss)

      makeItem []
        >>= loadAndApplyTemplate "templates/concat.txt" styleCtx

  create ["css/post-bundle.css"] $ do
    route idRoute
    compile $ do
      commonCss <- loadAll "css/common/*"
      postCss   <- loadAll "css/post/*"
      let styleCtx = listField
                     "items"
                     defaultContext
                     (return $ commonCss <> postCss)

      makeItem []
        >>= loadAndApplyTemplate "templates/concat.txt" styleCtx

  tags <- buildTags postsPattern (fromCapture "tags/*.html")

  tagsRules tags $ \tag pat -> do
    route idRoute
    compile $ do
      posts   <- skipFuture now =<< recentFirst =<< loadAll pat
      postCtx <- loadPostCtx tags
      tagCtx  <- loadTagCtx tag postCtx posts

      makeItem ""
        >>= loadAndApplyTemplate "templates/tag.html" tagCtx
        >>= relativizeUrls

  match postsPattern $ do
    route   $ setExtension "html"
    compile $ do
      ctx <- loadPostCtx tags
      pandocCompiler
        >>= saveSnapshot "content"
        >>= loadAndApplyTemplate "templates/post.html" ctx
        >>= relativizeUrls

  create ["atom.xml"] $ do
    route idRoute
    compile $ do
      config  <- itemBody <$> load "assets/config.json"
      posts   <- fmap (take . getFeedSize $ config) . skipFuture now
        =<< recentFirst
        =<< loadAllSnapshots postsPattern "content"
      feedCtx <- bodyField "description" <+> loadPostCtx tags
      renderAtom (feedConfiguration config) feedCtx posts

  match "assets/about.org" $ do
    route   $ assetsRoute <> setExtension "html"
    compile $ pandocCompiler

  match "templates/index.html" $ do
    route (constRoute "index.html")
    compile $ do
      about    <- load $ fromFilePath "assets/about.org"
      posts    <- skipFuture now =<< recentFirst =<< loadAll postsPattern
      postCtx  <- loadPostCtx tags
      indexCtx <- loadIndexCtx postCtx posts about

      getResourceBody
        >>= applyAsTemplate indexCtx
        >>= relativizeUrls

  match "templates/*" $ compile templateBodyCompiler

--------------------------------------------------------------------------------

loadCtx :: Compiler (Context String)
loadCtx = appContext <$> itemBody <$> load "assets/config.json"

loadPostCtx :: Tags -> Compiler (Context String)
loadPostCtx tags
  =   tagsField "tags" tags
  <+> rawTagsField "rawTags" tags
  <+> dateField "date" "%B %e, %Y"
  <+> updateField "update" "%B %e, %Y"
  <+> teaserField "teaser" "content"
  <+> defaultContext
  <+> loadCtx

loadIndexCtx :: Context String
            -> [Item String]
            -> Item String
            -> Compiler (Context String)
loadIndexCtx ctx posts about
  =   listField "posts" ctx (return posts)
  <+> field "about" (const . return . itemBody $ about)
  <+> loadCtx

loadTagCtx :: String
           -> Context String
           -> [Item String]
           -> Compiler (Context String)
loadTagCtx tag ctx posts
  =   constField "tag" tag
  <+> listField "posts" ctx (return posts)
  <+> loadCtx

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

rawTagsField :: String -> Tags -> Context a
rawTagsField = tagsFieldWith getTags render (mconcat . intersperse ", ")
  where render _ Nothing = Nothing
        render tag _     = Just $ toHtml tag

--------------------------------------------------------------------------------

assetsRoute :: Routes
assetsRoute = gsubRoute "assets/" (const "")

nodeRoute :: Routes
nodeRoute = gsubRoute "node_modules" (const "library")

--------------------------------------------------------------------------------

(<+>) :: (Monoid a, Applicative m) => a -> m a -> m a
a <+> ma = mappend a <$> ma
infixr 6 <+>

--------------------------------------------------------------------------------
