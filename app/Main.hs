--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}

--------------------------------------------------------------------------------

module Main
  ( main,
  )
where

--------------------------------------------------------------------------------

import Data.Time (getCurrentTime)
import Site.About
import Site.CSS
import Site.Config
import Site.Core
import Site.Index
import Site.Posts
import Site.Projects
import Site.RSS
import Site.Static
import Site.Support
import Site.Tags

--------------------------------------------------------------------------------

hakyllConfig :: Configuration
hakyllConfig =
  defaultConfiguration
    { destinationDirectory = "public"
    }

main :: IO ()
main = hakyllWith hakyllConfig $ do
  staticsRule
  configRule
  cssRule
  aboutRule
  supportRule
  projectsRule

  now <- preprocess getCurrentTime
  tags <- buildTags postsPattern (fromCapture "tags/*.html")
  postsRule tags
  tagsRule tags now
  rssRule tags now

  indexRule tags now

  match "templates/*" $ compile templateBodyCompiler

--------------------------------------------------------------------------------
