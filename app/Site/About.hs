--------------------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings #-}

--------------------------------------------------------------------------------

module Site.About
  ( aboutRule
  , loadAbout
  ) where

--------------------------------------------------------------------------------

import           Site.Core

--------------------------------------------------------------------------------

import           Data.String (IsString)

--------------------------------------------------------------------------------

aboutRule :: Rules ()
aboutRule = match aboutPath $ do
  route $ assetsRoute <> setExtension "html"
  compile pandocCompiler

loadAbout :: Compiler (Item String)
loadAbout = load $ fromFilePath aboutPath

--------------------------------------------------------------------------------

aboutPath :: IsString a => a
aboutPath = "assets/about.org"

--------------------------------------------------------------------------------
