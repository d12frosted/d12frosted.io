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
import           Site.Criterion.Measurement
import           Site.Criterion.Types

--------------------------------------------------------------------------------

import           Data.Aeson                      as Aeson
import qualified Data.Char                       as Char
import           Data.Hashable
import           Data.List                       (stripPrefix)
import qualified Data.Map.Strict                 as M
import           Data.Maybe                      (fromMaybe, isJust,
                                                  maybeToList)
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
render kvs name bs = renderHtml $ render' kvs chartId bs
  -- ideally, chartId should contain random value to make it possible to render
  -- two identical charts, currently it's possible by adding trash to the kvs
  where chartId = name <> "-" <> show (hash kvs)

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
        chartScales = toChartScales chartType kvs
        displayLegend = fromMaybe False $ lookupFlag "legend" kvs
        chartData = toChartData bs
        labels = toJSON $ cdLabels chartData
        dataSets = toJSON $ cdDataSets chartData
        prettyValues = M.fromList $ (\b -> (benchmarkMean b, secs' . toRealFloat . benchmarkMean $ b)) <$> bs
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
          tooltips: {
            callbacks: {
              label: function(item, data) {
                var vtls = #{toJSON prettyValues}
                var label = data.datasets[item.datasetIndex].label || '';
                if (label) label += ": ";
                label += vtls[item.value];
                return label;
              }
            }
          },
          scales: #{toJSON chartScales}
        }
      });
      |] undefined

--------------------------------------------------------------------------------

data ChartScales
  = ChartScales
  { csAxisX :: Maybe ChartAxis
  , csAxisY :: Maybe ChartAxis
  }

data ChartAxis
  = ChartAxis
  { caType        :: Text
  , caBeginAtZero :: Bool
  }

instance ToJSON ChartScales where
  toJSON ChartScales {..}
    = Aeson.object
    [ "xAxes" .= toJSON (maybeToList csAxisX)
    , "yAxes" .= toJSON (maybeToList csAxisY)
    ]

instance ToJSON ChartAxis where
  toJSON ChartAxis {..}
    = Aeson.object
    [ "type" .= caType
    , "ticks" .= Aeson.object
      [ "beginAtZero" .= caBeginAtZero
      ]
    ]

toChartScales :: String -> [(String, String)] -> ChartScales
toChartScales "horizontalBar" kvs
  = ChartScales
  { csAxisX =
      Just $ ChartAxis
      { caType = fromString . fromMaybe "linear" $ lookup "xAxisType" kvs
      , caBeginAtZero = fromMaybe True $ lookupFlag "xAxisBeginAtZero" kvs
      }
  , csAxisY = Nothing
  }
toChartScales _ kvs
  = ChartScales
  { csAxisX = Nothing
  , csAxisY =
      Just $ ChartAxis
      { caType = fromString . fromMaybe "linear" $ lookup "yAxisType" kvs
      , caBeginAtZero = fromMaybe True $ lookupFlag "yAxisBeginAtZero" kvs
      }
  }

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

-- TODO: move to configurations
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

lookupFlag :: String -> [(String, String)] -> Maybe Bool
lookupFlag key kvs = case lookup key kvs of
  Just "t"     -> Just True
  Just "true"  -> Just True
  Just "false" -> Just False
  _            -> Just False

stripCamelCasePrefix :: String -> String -> String
stripCamelCasePrefix prefix label =
  case stripPrefix prefix label of
    Nothing    -> label
    Just (h:t) -> Char.toLower h : t
    Just t     -> t

toKVList :: (Ord a) => [(a, b)] -> M.Map a [b]
toKVList = M.fromListWith (++) . fmap (\(x,y) -> (x,[y]))

--------------------------------------------------------------------------------
