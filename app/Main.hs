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
      ctx <- loadIndexCtx
      getResourceBody
        >>= loadAndApplyTemplate "templates/default.html" ctx
        >>= relativizeUrls

  match "posts/*.markdown" $ do
    route $ setExtension "html"
    compile $ do
      ctx <- loadPostCtx
      pandocCompiler
        >>= loadAndApplyTemplate "templates/post.html"    ctx
        >>= loadAndApplyTemplate "templates/default.html" ctx
        >>= relativizeUrls

  match "templates/*" $ compile templateBodyCompiler

--------------------------------------------------------------------------------
loadCtx :: Compiler (Context String)
loadCtx = appContext <$> itemBody <$> load "configs/config.json"

loadIndexCtx :: Compiler (Context String)
loadIndexCtx = constField "group" "index" <+> loadCtx

loadPostCtx :: Compiler (Context String)
loadPostCtx = constField "group" "post"
  <+> dateField "date" "%B %e, %Y"
  <+> loadCtx

--------------------------------------------------------------------------------
(<+>) :: (Monoid a, Applicative m) => a -> m a -> m a
a <+> ma = mappend a <$> ma
infixr 6 <+>
