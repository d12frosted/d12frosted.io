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

import           Control.Monad (filterM, liftM)
import           Data.List     (sortOn)
import           Data.Ord      (comparing)
import           Hakyll

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
      projectCtx <- loadAppCtx
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
