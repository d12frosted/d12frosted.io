--------------------------------------------------------------------------------

{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}

--------------------------------------------------------------------------------

module Site.Criterion.Render
  ( render
  ) where

--------------------------------------------------------------------------------

import           Site.Core
import           Site.Criterion.Types

--------------------------------------------------------------------------------

import           Data.Aeson                      as Aeson
import qualified Data.Char                       as Char
import           Data.List                       (stripPrefix)
import qualified Data.Map.Strict                 as M
import           Data.Maybe                      (fromMaybe, isJust)
import           Data.Scientific
import qualified Data.Set                        as S
import           Data.String                     (IsString, fromString)
import           Data.Text                       (Text)
import qualified Data.Text                       as T
import qualified Data.Text.Lazy                  as LT
import           GHC.Generics
import           Text.Blaze.Html.Renderer.String
import           Text.Blaze.Html5                as H
import           Text.Blaze.Html5.Attributes     as A
import           Text.Julius

--------------------------------------------------------------------------------

render :: [(String, String)] -> String -> [Benchmark] -> String
render kvs name bs = renderHtml $ render' kvs name bs

render' :: [(String, String)] -> String -> [Benchmark] -> Html
render' kvs name bs
  = let _canvas = canvas mempty
                  ! A.id (fromString name)
                  ! maybe mempty (height . fromString) (lookup "height" kvs)
                  ! maybe mempty (width . fromString) (lookup "width" kvs)
        _js = chartJs kvs name bs
        _script = script $ toHtml _js
    in _canvas <> _script

chartJs :: [(String, String)] -> String -> [Benchmark] -> LT.Text
chartJs kvs name bs
  = let title = lookup "title" kvs
        chartType = fromMaybe "line" $ lookup "type" kvs
        yAxisType = fromMaybe "linear" $ lookup "yAxisType" kvs
        displayLegend = isJust $ lookup "legend" kvs
        chartData = toChartData bs
        labels = toJSON $ cdLabels chartData
        dataSets = toJSON $ cdDataSets chartData
    in renderJavascript $ [julius|
      new Chart(document.getElementById(#{name}), {
        type: #{chartType},
        data: {
          labels: #{labels},
          datasets: #{dataSets}
        },
        options: {
          title: {
            display: #{isJust title},
            text: #{fromMaybe "" title}
          },
          legend: {
            display: #{displayLegend}
          },
          scales: {
            yAxes: [{ type: #{yAxisType} }]
          }
        }
      });
      |] undefined

--------------------------------------------------------------------------------

data ChartData
  = ChartData
  { cdLabels   :: [Text]
  , cdDataSets :: [ChartDataSet]
  } deriving (Show)

data ChartDataSet
  = ChartDataSet
  { cdsData            :: [Maybe Scientific]
  , cdsLabel           :: Text
  , cdsBorderColor     :: Text
  , cdsBackgroundColor :: Text
  , cdsFill            :: Bool
  } deriving (Generic, Show)

instance ToJSON ChartDataSet where
  toJSON ChartDataSet {..}
    = Aeson.object
    [ "data"            .= toJSON cdsData
    , "label"           .= cdsLabel
    , "borderColor"     .= cdsBorderColor
    , "backgroundColor" .= cdsBackgroundColor
    , "fill"            .= cdsFill
    ]

  toEncoding = genericToEncoding $ defaultOptions
    { fieldLabelModifier = stripCamelCasePrefix "lds"
    }

--------------------------------------------------------------------------------

toDataMap :: [Benchmark] -> M.Map Text (M.Map Text Scientific)
toDataMap bs = M.fromList <$> toKVList (toRaw <$> bs)
  where toRaw :: Benchmark -> (Text, (Text, Scientific))
        toRaw Benchmark {..} = case T.splitOn "/" benchmarkName of
          (l:ls) -> (l, (T.concat ls, benchmarkMean))
          _      -> (benchmarkName, ("", benchmarkMean))

toChartData :: [Benchmark] -> ChartData
toChartData bs =
  let dataMap = toDataMap bs
      labels = S.toList . S.fromList . concat $ M.keys <$> M.elems dataMap
  in ChartData
  { cdLabels = labels
  , cdDataSets = (\(idx, (k, v)) -> toChartDataSet labels idx k v) <$>
                 zip [1..] (M.toList dataMap)
  }

toChartDataSet :: [Text] -> Int -> Text -> M.Map Text Scientific -> ChartDataSet
toChartDataSet labels idx ldsLabel dataMap
  = ChartDataSet
  { cdsData            = (`M.lookup` dataMap) <$> labels
  , cdsLabel           = ldsLabel
  , cdsBorderColor     = lineColors !! (idx `mod` length lineColors)
  , cdsBackgroundColor = lineColors !! (idx `mod` length lineColors)
  , cdsFill            = False
  }

lineColors :: [Text]
lineColors
  = [ "#ff6384"
    , "#36a2eb"
    , "#ffce56"
    , "#4bc0c0"
    , "#9966ff"
    , "#ff9f40"
    ]

--------------------------------------------------------------------------------

stripCamelCasePrefix :: String -> String -> String
stripCamelCasePrefix prefix label =
  case stripPrefix prefix label of
    Nothing    -> label
    Just (h:t) -> Char.toLower h : t
    Just t     -> t

toKVList :: (Ord a) => [(a, b)] -> M.Map a [b]
toKVList = M.fromListWith (++) . fmap (\(x,y) -> (x,[y]))

--------------------------------------------------------------------------------

-- | Not-a-number value if the type supports it.
{-# SPECIALIZE nan :: Double #-}
{-# SPECIALIZE nan :: Float #-}
nan :: (RealFloat a) => a
nan = 0/0

--------------------------------------------------------------------------------
