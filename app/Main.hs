--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}

--------------------------------------------------------------------------------
module Main where

--------------------------------------------------------------------------------
import Config
import ToContext

--------------------------------------------------------------------------------
import Hakyll
import Hakyll.Core.Compiler.Internal
import Data.Monoid ((<>))

--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
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
      context <- loadContext
      getResourceBody
        >>= loadAndApplyTemplate "templates/default.html" context
        >>= relativizeUrls

  match "templates/*" $ compile templateBodyCompiler

loadContext :: Compiler (Context String)
loadContext = appContext <$> itemBody <$> load "configs/config.json"
