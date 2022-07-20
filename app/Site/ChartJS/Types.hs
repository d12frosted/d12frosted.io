{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Site.ChartJS.Types where

import Data.Aeson as Aeson hiding (Options)
import Data.String (IsString)
import Data.Text (Text)
import GHC.Generics

--------------------------------------------------------------------------------

data TableData = TableData [Text] [[(Text, Text)]]

--------------------------------------------------------------------------------

data ChartType = Bar | Line | Pie deriving (Show)

instance ToJSON ChartType where
  toJSON Bar = "bar"
  toJSON Line = "line"
  toJSON Pie = "pie"

--------------------------------------------------------------------------------

data Chart a = Chart
  { chartName :: Text,
    chartHeight :: Maybe Int,
    chartWidth :: Maybe Int,
    chartOptions :: Options,
    chartData :: ChartData a
  }
  deriving (Show)

--------------------------------------------------------------------------------

data Axis = X | Y deriving (Show)

instance ToJSON Axis where
  toJSON X = "x"
  toJSON Y = "y"

--------------------------------------------------------------------------------

data Options
  = OBar BarOptions
  | OLine LineOptions
  | OPie PieOptions
  deriving (Show)

instance ToJSON Options where
  toJSON (OBar o) = toJSON o
  toJSON (OLine o) = toJSON o
  toJSON (OPie o) = toJSON o

--------------------------------------------------------------------------------

data Plugin
  = TitlePlugin Text
  | LegendPlugin Bool
  | DataLabelsPlugin
  deriving (Show)

pluginName :: (IsString a) => Plugin -> a
pluginName (TitlePlugin _) = "title"
pluginName (LegendPlugin _) = "legend"
pluginName DataLabelsPlugin = "datalabels"

pluginsToJSON :: [Plugin] -> Aeson.Value
pluginsToJSON = Aeson.object . fmap (\p -> pluginName p .= p)

instance ToJSON Plugin where
  toJSON (TitlePlugin title) =
    Aeson.object ["display" .= True, "text" .= title]
  toJSON (LegendPlugin display) =
    Aeson.object ["display" .= display]
  toJSON DataLabelsPlugin =
    Aeson.object ["anchor" .= ("end" :: Text), "align" .= ("start" :: Text)]

--------------------------------------------------------------------------------

data BarOptions = BarOptions
  { barIndexAxis :: Axis,
    barSkipNull :: Bool,
    barScales :: Scales,
    barPlugins :: [Plugin]
  }
  deriving (Show)

instance ToJSON BarOptions where
  toJSON BarOptions {..} =
    Aeson.object
      [ "indexAxis" .= barIndexAxis,
        "skipNull" .= barSkipNull,
        "scales" .= barScales,
        "plugins" .= pluginsToJSON barPlugins
      ]

data LineOptions = LineOptions
  { lineIndexAxis :: Axis,
    linePlugins :: [Plugin]
  }
  deriving (Show)

instance ToJSON LineOptions where
  toJSON LineOptions {..} =
    Aeson.object
      [ "indexAxis" .= lineIndexAxis,
        "plugins" .= pluginsToJSON linePlugins
      ]

data PieOptions = PieOptions
  { pieRotation :: Int,
    piePlugins :: [Plugin]
  }
  deriving (Show)

instance ToJSON PieOptions where
  toJSON PieOptions {..} =
    Aeson.object
      [ "rotation" .= pieRotation,
        "plugins" .= pluginsToJSON piePlugins
      ]

--------------------------------------------------------------------------------

data ChartData a = ChartData
  { dataLabels :: [Text],
    dataSets :: [DataSet a]
  }
  deriving (Show)

data DataSet a = DataSet
  { dataSetValues :: [Maybe a],
    dataSetLabel :: Text,
    dataSetBorderColors :: [Text],
    dataSetBackgroundColors :: [Text],
    dataSetBorderWidth :: Int,
    dataSetFill :: Bool
  }
  deriving (Generic, Show)

instance ToJSON a => ToJSON (DataSet a) where
  toJSON DataSet {..} =
    Aeson.object
      [ "data" .= toJSON dataSetValues,
        "label" .= dataSetLabel,
        "borderColor" .= dataSetBorderColors,
        "backgroundColor" .= dataSetBackgroundColors,
        "borderWidth" .= dataSetBorderWidth,
        "fill" .= dataSetFill
      ]

--------------------------------------------------------------------------------

data Scales = Scales
  { scalesAxisX :: Maybe AxisOptions,
    scalesAxisY :: Maybe AxisOptions
  }
  deriving (Show)

instance ToJSON Scales where
  toJSON Scales {..} =
    Aeson.object $
      maybe [] (\o -> ["x" .= o]) scalesAxisX
        <> maybe [] (\o -> ["y" .= o]) scalesAxisY

data AxisOptions = AxisOptions
  { axisType :: Text,
    axisBeginAtZero :: Bool
  }
  deriving (Show)

instance ToJSON AxisOptions where
  toJSON AxisOptions {..} =
    Aeson.object
      [ "type" .= axisType,
        "beginAtZero" .= axisBeginAtZero
      ]

--------------------------------------------------------------------------------
