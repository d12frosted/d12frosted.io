--------------------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

--------------------------------------------------------------------------------

module Main (main) where

--------------------------------------------------------------------------------

import           Site.About
import           Site.Config
import           Site.Core
import           Site.CSS
import           Site.Index
import           Site.Posts
import           Site.Projects
import           Site.RSS
import           Site.Static
import           Site.Tags

--------------------------------------------------------------------------------

import           Data.Time     (getCurrentTime)

--------------------------------------------------------------------------------

hakyllConfig :: Configuration
hakyllConfig = defaultConfiguration {
  destinationDirectory = "public"
}

main :: IO ()
main = getCurrentTime >>= \now -> hakyllWith hakyllConfig $ do
  staticsRule
  configRule
  cssRule
  aboutRule
  projectsRule

  tags <- buildTags postsPattern (fromCapture "tags/*.html")
  postsRule tags
  tagsRule tags now
  rssRule tags now

  indexRule tags now

  match "templates/*" $ compile templateBodyCompiler

--------------------------------------------------------------------------------
