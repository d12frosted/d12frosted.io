--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}

--------------------------------------------------------------------------------

module Site.Criterion
  ( loadCriterionMap,
    renderCriterion,
  )
where

--------------------------------------------------------------------------------

import Control.Arrow ((&&&))
import Control.Lens ((<&>), (^..), (^?), (^?!), to)
import Data.Aeson.Lens
import qualified Data.Map.Strict as M
import Data.Text (Text, unpack)
import Site.Core
import Site.Criterion.Render
import Site.Criterion.Types
import System.FilePath (dropExtension)

--------------------------------------------------------------------------------

loadCriterionMap :: Compiler (M.Map FilePath String)
loadCriterionMap = toCriterionMap <$> loadAll "assets/criterion/**"

toCriterionMap :: [Item String] -> M.Map FilePath String
toCriterionMap is = M.fromList kvs
  where
    kvs = ((toFilePath . itemIdentifier) &&& itemBody) <$> is

--------------------------------------------------------------------------------

lookupCriterion :: FilePath -> M.Map FilePath String -> Maybe String
lookupCriterion a = M.lookup ("assets/criterion/" <> a)

--------------------------------------------------------------------------------

renderCriterion :: [(Text, Text)] -> M.Map FilePath String -> Maybe Text
renderCriterion kvs criterionMap = do
  file <- unpack <$> lookup "file" kvs
  render kvs (dropExtension file) <$> readBenchmarks file criterionMap

--------------------------------------------------------------------------------

readBenchmarks :: FilePath -> M.Map FilePath String -> Maybe [Benchmark]
readBenchmarks file criterionMap =
  lookupCriterion file criterionMap >>= parseBenchmarks

parseBenchmarks :: String -> Maybe [Benchmark]
parseBenchmarks content =
  let reportName o = o ^?! key "reportName" . _String
      mean o =
        o ^?! key "reportAnalysis"
          . key "anMean"
          . key "estPoint"
          . _Number
      benchmark = to (\o -> (reportName o, mean o))
      benchmarks = content ^? nth 2 <&> (^.. values . benchmark)
   in fmap (uncurry Benchmark) <$> benchmarks

--------------------------------------------------------------------------------
