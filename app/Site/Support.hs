{-# LANGUAGE OverloadedStrings #-}

module Site.Support
  ( supportRule,
    loadSupport,
  )
where

--------------------------------------------------------------------------------

import Data.String (IsString)
import Site.Core
import Site.Pandoc

--------------------------------------------------------------------------------

supportRule :: Rules ()
supportRule = match supportPath $ do
  route $ gsubRoute "assets/" (const "") <> setExtension "html"
  compile customPandocCompiler

loadSupport :: Compiler (Item String)
loadSupport = load $ fromFilePath supportPath

--------------------------------------------------------------------------------

supportPath :: IsString a => a
supportPath = "assets/support.org"

--------------------------------------------------------------------------------
