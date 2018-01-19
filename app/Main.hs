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
    route idRoute
    compile configCompiler

  match "images/*" $ do
    route   idRoute
    compile copyFileCompiler

  match "css/*" $ do
    route   idRoute
    compile compressCssCompiler

  match "static/index.html" $ do
    route idRoute
    compile $ do
      config <- load "configs/config.json"
      getResourceBody
        >>= loadAndApplyTemplate "templates/default.html" (defContext $ itemBody config)
        >>= relativizeUrls

  match "templates/*" $ compile templateBodyCompiler

defContext :: Config -> Context String
defContext cfg = toContext cfg <> defaultContext
