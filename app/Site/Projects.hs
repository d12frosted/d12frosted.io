--------------------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

--------------------------------------------------------------------------------

module Site.Projects
  ( projectsRule
  ) where

--------------------------------------------------------------------------------

import           Site.About
import           Site.Config
import           Site.Core

--------------------------------------------------------------------------------

import           Control.Lens             (preview)
import           Control.Monad            (filterM, join, liftM)
import           Data.Aeson.Lens          (key, _Integer)
import qualified Data.ByteString.Char8    as BS
import           Data.List                (sortOn)
import           Data.Maybe               (fromMaybe)
import           Data.Ord                 (comparing)
import           Data.String              (IsString (..))
import           Hakyll
import           Network.HTTP.Simple      (getResponseBody, httpBS,
                                           setRequestBasicAuth,
                                           setRequestHeader)
import           System.Environment.Extra (envMaybe)

--------------------------------------------------------------------------------

projectsRule :: Rules ()
projectsRule = do
  -- compile project definitions
  match projectsPattern (compile pandocCompiler)

  -- create projects page
  match "templates/projects.html" $ do
    route (constRoute "projects.html")
    compile $ do
      about      <- loadAbout
      projects   <- loadProjects
      stars      <- unsafeCompiler $ mapM (traverseToSnd fetchStargazersCount)
                    (getTitle <$> projects)
      projectCtx <- stargazersField stars <+> loadAppCtx
      indexCtx   <- loadProjectsCtx loadAppCtx projectCtx projects about

      getResourceBody
        >>= applyAsTemplate indexCtx
        >>= relativizeUrls

--------------------------------------------------------------------------------

loadProjects :: Compiler [Item String]
loadProjects = loadAll projectsPattern >>= sortByMetadata "priority"

--------------------------------------------------------------------------------

loadProjectsCtx :: Compiler (Context String)
                -> Context String
                -> [Item String]
                -> Item String
                -> Compiler (Context String)
loadProjectsCtx baseCtx projectCtx projects about
  =   listField "projects-special" projectCtx (withCategory "Special" projects)
  <+> listField "projects-emacs" projectCtx (withCategory "Emacs" projects)
  <+> listField "projects-haskell" projectCtx (withCategory "Haskell" projects)
  <+> listField "projects-other" projectCtx (withCategory "Other" projects)
  <+> field "about" (const . return . itemBody $ about)
  <+> baseCtx

--------------------------------------------------------------------------------

withCategory :: MonadMetadata m => String -> [Item a] -> m [Item a]
withCategory cat = filterM (hasCategory cat)

hasCategory :: MonadMetadata m => String -> Item a -> m Bool
hasCategory cat item
  = (cat ==) <$> getMetadataField' (itemIdentifier item) "category"

--------------------------------------------------------------------------------

projectsPattern :: Pattern
projectsPattern = "projects/*"

--------------------------------------------------------------------------------

sortByMetadata :: MonadMetadata m => String -> [Item a] -> m [Item a]
sortByMetadata name = sortByM $ \i -> getMetadataField' (itemIdentifier i) name
  where
    sortByM :: (Monad m, Ord k) => (a -> m k) -> [a] -> m [a]
    sortByM f xs = map fst . sortOn snd <$> mapM (\x -> fmap (x,) (f x)) xs

--------------------------------------------------------------------------------

stargazersField :: [(String, Maybe Integer)] -> Context String
stargazersField stars
  = field "stargazers_count"
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
  let request
        = setRequestHeader "User-Agent" ["d12frosted"]
        $ fromString url
  res <- httpBS (fromMaybe request (setAuth <*> pure request))
  pure (getResponseBody res)

--------------------------------------------------------------------------------
