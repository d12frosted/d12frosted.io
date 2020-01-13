--------------------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings #-}

--------------------------------------------------------------------------------

module Site.About
  ( aboutRule
  , loadAbout
  ) where

--------------------------------------------------------------------------------

import           Site.Core
import           Site.Pandoc

--------------------------------------------------------------------------------

import           Data.String (IsString)

--------------------------------------------------------------------------------

aboutRule :: Rules ()
aboutRule = match aboutPath $ do
  route $ gsubRoute "assets/" (const "") <> setExtension "html"
  compile customPandocCompiler

loadAbout :: Compiler (Item String)
loadAbout = load $ fromFilePath aboutPath

--------------------------------------------------------------------------------

aboutPath :: IsString a => a
aboutPath = "assets/about.org"

--------------------------------------------------------------------------------
