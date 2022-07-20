{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Site.ChartJS.Parse (parseTableData, parseChart, noteMaybe, note) where

import Control.Monad.Error.Class (throwError)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text as T (Text, unpack)
import Site.ChartJS.Types
import Text.Pandoc
import Text.Pandoc.Shared (stringify)
import Text.Pandoc.Walk

--------------------------------------------------------------------------------

parseTableData :: Text -> Block -> Either Text TableData
parseTableData _ (Table _ _ _ th [tb] _) = pure $ TableData aHeader (toValues tb)
  where
    aHeader = toHeader th
    toHeader = queryTableHead $ \case
      Plain ins -> [stringify ins]
      _ -> []
    toValues (TableBody _ _ _ rows) = zip aHeader . fmap stringify . getCells <$> rows
    getCells (Row _ cells) = cells
parseTableData name _ = note name "table data can be parsed only from Table block"

parseChart :: Text -> [(Text, Text)] -> TableData -> Either Text (Chart Text)
parseChart name kvs (TableData _ values) = do
  labelKey <- noteMaybe name "missing 'labels'" $ lookup "labels" kvs
  valueKey <- noteMaybe name "missing 'values'" $ lookup "values" kvs
  chartType <- case lookup "type" kvs of
    Just "bar" -> pure Bar
    Just "line" -> pure Line
    Just "pie" -> pure Pie
    Just n -> note name $ "unsupported chart type " <> n
    _ -> note name "missing 'type'"
  indexAxis <- case fromMaybe "x" $ lookup "index-axis" kvs of
    "x" -> pure X
    "y" -> pure Y
    n -> note name $ "unsupported index-axis " <> n
  let scales = parseScales indexAxis kvs
  let backgroundColors =
        [ "rgba(255, 99, 132, 0.2)",
          "rgba(54, 162, 235, 0.2)",
          "rgba(255, 206, 86, 0.2)",
          "rgba(75, 192, 192, 0.2)",
          "rgba(153, 102, 255, 0.2)",
          "rgba(255, 159, 64, 0.2)"
        ]
  let borderColors =
        [ "rgba(255, 99, 132, 1)",
          "rgba(54, 162, 235, 1)",
          "rgba(255, 206, 86, 1)",
          "rgba(75, 192, 192, 1)",
          "rgba(153, 102, 255, 1)",
          "rgba(255, 159, 64, 1)"
        ]
  let legend = fromMaybe False $ lookupFlag "legend" kvs
  let plugins = [LegendPlugin legend, DataLabelsPlugin]
  let options = case chartType of
        Bar ->
          OBar $
            BarOptions
              { barIndexAxis = indexAxis,
                barSkipNull = False,
                barScales = scales,
                barPlugins = plugins
              }
        Line ->
          OLine $
            LineOptions
              { lineIndexAxis = indexAxis,
                linePlugins = plugins
              }
        Pie ->
          OPie $
            PieOptions
              { pieRotation = 0,
                piePlugins = plugins
              }
  pure $
    Chart
      { chartName = name,
        chartHeight = read . T.unpack <$> lookup "height" kvs,
        chartWidth = read . T.unpack <$> lookup "width" kvs,
        chartOptions = options,
        chartData =
          ChartData
            { dataLabels = mapMaybe (lookup labelKey) values,
              dataSets =
                [ DataSet
                    { dataSetValues = map (lookup valueKey) values,
                      dataSetLabel = fromMaybe valueKey $ lookup "label" kvs,
                      dataSetBorderColors = borderColors,
                      dataSetBackgroundColors = backgroundColors,
                      dataSetBorderWidth = 1,
                      dataSetFill = False
                    }
                ]
            }
      }

parseScales :: Axis -> [(Text, Text)] -> Scales
parseScales X kvs = Scales {scalesAxisX = Nothing, scalesAxisY = Just $ parseAxisOptions Y kvs}
parseScales Y kvs = Scales {scalesAxisX = Just $ parseAxisOptions X kvs, scalesAxisY = Nothing}

parseAxisOptions :: Axis -> [(Text, Text)] -> AxisOptions
parseAxisOptions axis kvs =
  AxisOptions
    { axisType = fromMaybe "linear" $ lookup axisTypeKey kvs,
      axisBeginAtZero = fromMaybe True $ lookupFlag axisBeginAtZeroKey kvs
    }
  where
    prefix = case axis of
      X -> "x"
      Y -> "y"
    axisTypeKey = prefix <> "AxisType"
    axisBeginAtZeroKey = prefix <> "AxisBeginAtZero"

lookupFlag :: Text -> [(Text, Text)] -> Maybe Bool
lookupFlag key kvs = case lookup key kvs of
  Just "t" -> Just True
  Just "true" -> Just True
  Just "false" -> Just False
  _ -> Nothing

noteMaybe :: Text -> Text -> Maybe a -> Either Text a
noteMaybe _ _ (Just a) = pure a
noteMaybe name msg _ = note name msg

note :: Text -> Text -> Either Text a
note name msg = throwError $ "chartjs" <> "." <> name <> ": " <> msg
