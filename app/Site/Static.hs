--------------------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings #-}

--------------------------------------------------------------------------------

module Site.Static where

--------------------------------------------------------------------------------

import           Site.Core

--------------------------------------------------------------------------------

staticsRule :: Rules ()
staticsRule = do
  match "assets/images/*" $ do
    route $ gsubRoute "assets/" (const "")
    compile copyFileCompiler

  match "assets/criterion/**" $ do
    route $ gsubRoute "assets/" (const "")
    compile getResourceString

  match "node_modules/**" $ do
    route (gsubRoute "node_modules" (const "library"))
    compile copyFileCompiler

--------------------------------------------------------------------------------
