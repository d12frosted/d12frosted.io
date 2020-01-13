--------------------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings #-}

--------------------------------------------------------------------------------

module Site.Pandoc (customPandocCompiler) where

--------------------------------------------------------------------------------

import           Site.Core
import           Site.Criterion

--------------------------------------------------------------------------------

import           Control.Arrow                   ((&&&))
import           Data.List
import qualified Data.Map.Strict                 as M
import           Data.Maybe                      (maybeToList)
import           Data.Monoid                     (mappend)
import           System.FilePath
import           System.IO.Unsafe
import           Text.Blaze.Html.Renderer.String
import           Text.Blaze.Html5                as H
import           Text.Blaze.Html5.Attributes     as A
import           Text.Pandoc
import           Text.Pandoc.Definition
import           Text.Pandoc.Options
import           Text.Pandoc.Walk

--------------------------------------------------------------------------------

customPandocCompiler :: Compiler (Item String)
customPandocCompiler = do
  criterionMap <- loadCriterionMap
  let writerOptions
        = defaultHakyllWriterOptions
        { writerExtensions = defaultExtensions <> mathExtensions
        , writerHTMLMathMethod = MathJax ""
        }
      readerOptions = defaultHakyllReaderOptions
  pandocCompilerWithTransform readerOptions writerOptions $
    transform criterionMap

--------------------------------------------------------------------------------

mathExtensions :: Extensions
mathExtensions
  = extensionsFromList
  [ Ext_tex_math_dollars
  , Ext_tex_math_double_backslash
  , Ext_latex_macros
  ]

defaultExtensions :: Extensions
defaultExtensions = writerExtensions defaultHakyllWriterOptions

--------------------------------------------------------------------------------

transform :: M.Map FilePath String -> Pandoc -> Pandoc
transform criterionMap = walk $ \block -> case block of
  Div (_, cs, kvs) _ ->
    if "criterion" `elem` cs
    then maybe Null (RawBlock "html") (renderCriterion kvs criterionMap)
    else Null
  _ -> block

--------------------------------------------------------------------------------
