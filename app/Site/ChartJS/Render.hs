{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Site.ChartJS.Render (render, importStatement) where

import Data.Aeson as Aeson
import Data.String (IsString, fromString)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import Site.ChartJS.Types
import Text.Blaze.Html.Renderer.String
import Text.Blaze.Html5 (Html, (!))
import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A
import qualified Text.Julius as J

--------------------------------------------------------------------------------

importStatement :: (IsString a) => a
importStatement = fromString . renderHtml $ importChartJS <> importPlugin
  where
    importChartJS = H.script mempty ! A.src "/library/chart.js/dist/chart.min.js"
    importPlugin = H.script mempty ! A.src "/library/chartjs-plugin-datalabels/dist/chartjs-plugin-datalabels.min.js"

--------------------------------------------------------------------------------

render :: (ToJSON a, IsString b) => Chart a -> b
render = fromString . renderHtml . renderToHtml

renderToHtml :: (ToJSON a) => Chart a -> Html
renderToHtml chart@(Chart {..}) = aDiv $ canvas <> script
  where
    canvas =
      H.canvas mempty
        ! A.id (fromText chartName)
        ! maybe mempty (height . fromString . show) chartHeight
        ! maybe mempty (width . fromString . show) chartWidth
    script = H.script . H.toHtml . chartJs $ chart
    aDiv = H.div ! A.class_ "chartjs"

--------------------------------------------------------------------------------

chartJs :: (ToJSON a) => Chart a -> LT.Text
chartJs Chart {..} =
  let labels = toJSON $ dataLabels chartData
      sets = toJSON $ dataSets chartData
      options = toJSON chartOptions
      chartType = case chartOptions of
        OBar _ -> Bar
        OLine _ -> Line
        OPie _ -> Pie
   in J.renderJavascript $
        [J.julius|
      new Chart(document.getElementById(#{chartName}).getContext('2d'), {
        plugins: [ChartDataLabels],
        type: #{toJSON chartType},
        data: {
          labels: #{labels},
          datasets: #{sets}
        },
        options: #{options}
      });
      |]
          undefined

--------------------------------------------------------------------------------

fromText :: IsString a => Text -> a
fromText = fromString . T.unpack
