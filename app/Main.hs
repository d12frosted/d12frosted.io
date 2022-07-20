{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Main (main) where

import Control.Lens (preview)
import Control.Monad (filterM, join, (<=<))
import Control.Monad.Error.Class (liftEither)
import Data.Aeson.Lens (key, _Integer)
import qualified Data.ByteString.Char8 as BS
import Data.List (intersperse, sortOn)
import Data.Maybe (fromMaybe, isJust, listToMaybe)
import Data.Monoid (Any (..), getAny)
import Data.String (IsString, fromString)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime, defaultTimeLocale, getCurrentTime)
import Hakyll hiding (fromList)
import Network.HTTP.Simple
  ( getResponseBody,
    httpBS,
    setRequestBasicAuth,
    setRequestHeader,
  )
import Site.ChartJS.Parse
import Site.ChartJS.Render
import Site.Config
import Site.Web.Template.Context (modificationDateField)
import System.Environment.Extra (envMaybe)
import System.FilePath (takeBaseName)
import Text.Blaze.Html (toHtml, toValue, (!))
import Text.Blaze.Html.Renderer.String (renderHtml)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Pandoc (Block (..), Inline (..), Pandoc (..))
import Text.Pandoc.Shared (mapLeft, stringify)
import Text.Pandoc.Walk

main :: IO ()
main = hakyll $ do
  now <- preprocess getCurrentTime

  configRule

  match "node_modules/**" $ do
    route (gsubRoute "node_modules" (const "library"))
    compile copyFileCompiler

  match ("images/*" .||. "images/**/*") $ do
    route idRoute
    compile copyFileCompiler

  match "css/*" $ do
    route idRoute
    compile compressCssCompiler

  match "assets/favicon/*" $ do
    route $ gsubRoute "assets/favicon/" (const "")
    compile copyFileCompiler

  match "assets/fonts/*" $ do
    route $ gsubRoute "assets/" (const "")
    compile copyFileCompiler

  match "assets/about.org" $ do
    route $ gsubRoute "assets/" (const "") <> setExtension "html"
    compile customPandocCompiler

  tags <- buildTags postsPattern (fromCapture "tags/*.html")

  tagsRules tags $ \tag pat -> do
    route idRoute
    compile $ do
      posts <- loadPosts pat now
      postCtx <- loadPostCtx tags
      tagCtx <- loadTagCtx tag postCtx posts

      makeItem ""
        >>= loadAndApplyTemplate "templates/tag.html" tagCtx
        >>= loadAndApplyTemplate "templates/default.html" tagCtx
        >>= relativizeUrls

    match postsPattern $ do
      route $ setExtension "html"
      compile $ do
        postCtx <- loadPostCtx tags
        customPandocCompiler
          >>= loadAndApplyTemplate "templates/post.html" postCtx
          >>= loadAndApplyTemplate "templates/default.html" postCtx
          >>= relativizeUrls

    create ["posts.html"] $ do
      route idRoute
      compile $ do
        posts <- recentFirst =<< loadAllPosts now
        appCtx <- loadAppCtx
        postCtx <- loadPostCtx tags
        let archiveCtx =
              listField "posts" postCtx (pure posts)
                <> constField "title" "Archives"
                <> appCtx

        makeItem ""
          >>= loadAndApplyTemplate "templates/posts.html" archiveCtx
          >>= loadAndApplyTemplate "templates/default.html" archiveCtx
          >>= relativizeUrls

    match "assets/index.html" $ do
      route $ gsubRoute "assets/" (const "")
      compile $ do
        posts <- recentFirst =<< loadAllPosts now
        intro <- load "assets/about.org"
        appCtx <- loadAppCtx
        postCtx <- loadPostCtx tags
        let indexCtx =
              listField "posts" postCtx (return posts)
                <> field "intro" (const . return . itemBody $ intro)
                <> appCtx

        getResourceBody
          >>= applyAsTemplate indexCtx
          >>= loadAndApplyTemplate "templates/default.html" indexCtx
          >>= relativizeUrls

    create ["atom.xml"] $ do
      route idRoute
      compile $ do
        config <- itemBody <$> loadConfig
        posts <- (take . getFeedSize $ config) <$> loadAllPosts now
        postCtx <- loadPostCtx tags
        let feedCtx = bodyField "description" <> postCtx
        renderAtom (feedConfiguration config) feedCtx posts

    match supportPath $ do
      route $ gsubRoute "assets/" (const "") <> setExtension "html"
      compile customPandocCompiler

    -- compile project definitions
    match projectsPattern (compile customPandocCompiler)

    -- create projects page
    match "assets/projects.html" $ do
      route (constRoute "projects.html")
      compile $ do
        support <- loadSupport
        projects <- loadProjects
        stars <- unsafeCompiler $ mapM (traverseToSnd fetchStargazersCount) (getTitle <$> projects)
        appCtx <- loadAppCtx
        let projectCtx = stargazersField stars <> appCtx
            projectsCtx = loadProjectsCtx appCtx projectCtx projects support

        getResourceBody
          >>= applyAsTemplate projectsCtx
          >>= loadAndApplyTemplate "templates/default.html" projectsCtx
          >>= relativizeUrls

    match "templates/*" $ compile templateBodyCompiler

--------------------------------------------------------------------------------

postsPattern :: Pattern
postsPattern = "posts/*.org"

loadPostCtx :: Tags -> Compiler (Context String)
loadPostCtx tags = do
  appCtx <- loadAppCtx
  pure $
    dateField "date" "%B %e, %Y"
      <> modificationDateField "update" "%B %e, %Y"
      <> teaserField "teaser" "content"
      <> customTagsField "tags" tags
      <> rawTagsField "rawTags" tags
      <> appCtx

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

loadTagCtx ::
  String ->
  Context String ->
  [Item String] ->
  Compiler (Context String)
loadTagCtx tag ctx posts = do
  appCtx <- loadAppCtx
  pure $
    constField "tag" tag
      <> listField "posts" ctx (return posts)
      <> appCtx

customTagsField :: String -> Tags -> Context a
customTagsField = tagsFieldWith getTags renderTag (mconcat . intersperse ", ")
  where
    renderTag _ Nothing = Nothing
    renderTag tag (Just filePath) =
      Just $
        H.a ! A.title (H.stringValue ("All pages tagged '" ++ tag ++ "'."))
          ! A.href (toValue $ toUrl filePath)
          $ toHtml ("#" <> tag)

rawTagsField :: String -> Tags -> Context a
rawTagsField = tagsFieldWith getTags renderTag (mconcat . intersperse ", ")
  where
    renderTag _ Nothing = Nothing
    renderTag tag _ = Just $ toHtml ("#" <> tag)

--------------------------------------------------------------------------------

loadSupport :: Compiler (Item String)
loadSupport = load $ fromFilePath supportPath

supportPath :: IsString a => a
supportPath = "assets/support.org"

--------------------------------------------------------------------------------

loadProjects :: Compiler [Item String]
loadProjects = loadAll projectsPattern >>= sortByMetadata "priority"

loadProjectsCtx :: Context String -> Context String -> [Item String] -> Item String -> Context String
loadProjectsCtx baseCtx projectCtx projects support =
  listField "projects-special" projectCtx (withCategory "Special" projects)
    <> listField "projects-emacs" projectCtx (withCategory "Emacs" projects)
    <> listField "projects-haskell" projectCtx (withCategory "Haskell" projects)
    <> listField "projects-other" projectCtx (withCategory "Other" projects)
    <> field "support" (const . return . itemBody $ support)
    <> baseCtx

withCategory :: (MonadFail m, MonadMetadata m) => String -> [Item a] -> m [Item a]
withCategory cat = filterM (hasCategory cat)

hasCategory :: (MonadFail m, MonadMetadata m) => String -> Item a -> m Bool
hasCategory cat item =
  (cat ==) <$> getMetadataField' (itemIdentifier item) "category"

projectsPattern :: Pattern
projectsPattern = "projects/*"

sortByMetadata :: (MonadFail m, MonadMetadata m) => String -> [Item a] -> m [Item a]
sortByMetadata name = sortByM $ \i -> getMetadataField' (itemIdentifier i) name
  where
    sortByM :: (Monad m, Ord k) => (a -> m k) -> [a] -> m [a]
    sortByM f xs = map fst . sortOn snd <$> mapM (\x -> fmap (x,) (f x)) xs

stargazersField :: [(String, Maybe Integer)] -> Context String
stargazersField stars =
  field
    "stargazers_count"
    (pure . show . fromMaybe 0 . join . flip lookup stars . getTitle)

fetchStargazersCount :: String -> IO (Maybe Integer)
fetchStargazersCount project = do
  body <- fetchProjectInfo project
  let count = preview (key "stargazers_count" . _Integer) body
  pure count

fetchProjectInfo :: String -> IO BS.ByteString
fetchProjectInfo project = do
  user <- envMaybe "GITHUB_API_USER"
  secret <- envMaybe "GITHUB_API_SECRET"
  let setAuth = setRequestBasicAuth <$> user <*> secret
  let url = "https://api.github.com/repos/d12frosted/" <> project
  let request =
        setRequestHeader "User-Agent" ["d12frosted"] $
          fromString url
  res <- httpBS (fromMaybe request (setAuth <*> pure request))
  pure (getResponseBody res)

--------------------------------------------------------------------------------

feedConfiguration :: Config -> FeedConfiguration
feedConfiguration config =
  FeedConfiguration
    { feedTitle = getFeedTitle config,
      feedDescription = getFeedDescription config,
      feedAuthorName = getAuthorName config,
      feedAuthorEmail = getAuthorEmail config,
      feedRoot = getSiteUrl config
    }

--------------------------------------------------------------------------------

pandoncTrasnformM :: Pandoc -> Compiler Pandoc
pandoncTrasnformM =
  convertIdLinks . wrapVideo . wrapTables <=< embedChartJS

customPandocCompiler :: Compiler (Item String)
customPandocCompiler = pandocCompilerWithTransformM readerOptions writerOptions transform
  where
    readerOptions = defaultHakyllReaderOptions
    writerOptions = defaultHakyllWriterOptions
    transform = pandoncTrasnformM

embedChartJS :: Pandoc -> Compiler Pandoc
embedChartJS pandoc = walkM embedChart . embedImport $ pandoc
  where
    queryTable :: Text -> Maybe Block
    queryTable name = listToMaybe . flip query pandoc $ \case
      t@(Table (n, _, _) _ _ _ _ _) | n == name -> [t]
      _ -> []

    hasChartJS = query $ \case
      Div (_, cs, _) _ | "chartjs" `elem` cs -> Any True
      _ -> mempty

    embedImport p@(Pandoc meta bs) =
      if getAny $ hasChartJS p
        then Pandoc meta (RawBlock "html" importStatement : bs)
        else p

    embedChart = liftEither . mapLeft (\e -> [T.unpack e]) . go []
    go names (b : bs) = case b of
      Div (name, _, _) _ | name `elem` names -> go names bs
      Table (name, _, _) _ _ _ _ _ | name `elem` names -> go names bs
      Div (name, cs, kvs) _ | "chartjs" `elem` cs -> do
        dataName <- noteMaybe name "missing 'data'" $ lookup "data" kvs
        dataBlock <- noteMaybe name ("missing table named " <> dataName) $ queryTable dataName
        tableData <- parseTableData name dataBlock
        chart <- parseChart name kvs tableData
        let chartHtml = RawBlock "html" $ render chart
        (chartHtml :) <$> go (dataName : names) bs
      _ -> (b :) <$> go names bs
    go _ [] = pure []

convertIdLinks :: Pandoc -> Compiler Pandoc
convertIdLinks = walkM $ \case
  link@(Link a is (urlRaw, title)) -> case T.stripPrefix "d12frosted:" urlRaw of
    Nothing -> pure link
    Just url' -> do
      let url = fromMaybe url' $ T.stripSuffix ".html" url'
      let i = fromFilePath . T.unpack . (<> ".org") . fromMaybe url . T.stripPrefix "/" $ url
      exists <- isJust <$> getRoute i
      pure $ if exists then Link a is (url <> ".html", title) else Str (stringify is)
  i -> pure i

wrapTables :: Pandoc -> Pandoc
wrapTables = walk go
  where
    go [] = []
    go (b : bs) = case b of
      Table {} -> Div ("", ["table-container"], []) [b] : go bs
      _ -> b : go bs

wrapVideo :: Pandoc -> Pandoc
wrapVideo = walk go
  where
    go [] = []
    go (b : bs) = case b of
      Para [Link _ _ (url, _)]
        | ".mp4" `T.isSuffixOf` url ->
          (RawBlock "html" . fromString . renderHtml $ toVideoBlock url) : go bs
      _ -> b : go bs

    toVideoBlock url =
      H.video
        ! A.autoplay ""
        ! A.loop ""
        $ (H.source ! A.src (fromString $ T.unpack url) ! A.type_ "video/mp4")

--------------------------------------------------------------------------------

getTitle :: Item a -> String
getTitle = takeBaseName . toFilePath . itemIdentifier

traverseToSnd :: Functor t => (a -> t b) -> a -> t (a, b)
traverseToSnd f a = (a,) <$> f a

--------------------------------------------------------------------------------
