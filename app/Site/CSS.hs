--------------------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings #-}

--------------------------------------------------------------------------------

module Site.CSS where

--------------------------------------------------------------------------------

import           Site.Core

--------------------------------------------------------------------------------

cssRule :: Rules ()
cssRule = do
  match ("css/*" .||. "css/**/*") $ do
    route   idRoute
    compile compressCssCompiler

  create ["css/index-bundle.css"] $ do
    route idRoute
    compile $ do
      commonCss <- loadAll "css/common/*"
      indexCss  <- loadAll "css/index/*"
      let styleCtx = listField
                     "items"
                     defaultContext
                     (return $ commonCss <> indexCss)

      makeItem []
        >>= loadAndApplyTemplate "templates/concat.txt" styleCtx

  create ["css/post-bundle.css"] $ do
    route idRoute
    compile $ do
      commonCss <- loadAll "css/common/*"
      postCss   <- loadAll "css/post/*"
      let styleCtx = listField
                     "items"
                     defaultContext
                     (return $ commonCss <> postCss)

      makeItem []
        >>= loadAndApplyTemplate "templates/concat.txt" styleCtx

--------------------------------------------------------------------------------
