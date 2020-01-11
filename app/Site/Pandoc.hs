--------------------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings #-}

--------------------------------------------------------------------------------

module Site.Pandoc where

--------------------------------------------------------------------------------

import           Site.Core

--------------------------------------------------------------------------------

import           Data.List
import           Text.Pandoc.Options

--------------------------------------------------------------------------------

pandocMathCompiler :: Compiler (Item String)
pandocMathCompiler =
  let mathExtensions = [Ext_tex_math_dollars, Ext_tex_math_double_backslash, Ext_latex_macros]
      defaultExtensions = writerExtensions defaultHakyllWriterOptions
      newExtensions = foldr enableExtension defaultExtensions mathExtensions
      writerOptions = defaultHakyllWriterOptions {
                        writerExtensions = newExtensions,
                        writerHTMLMathMethod = MathJax ""
                      }
  in pandocCompilerWith defaultHakyllReaderOptions writerOptions

--------------------------------------------------------------------------------
