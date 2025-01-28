'use client'

import {
  countriesGeoJson,
  countriesGeoJsonMap,
} from '@/lib/geo'
import * as Plot from '@observablehq/plot'
import { geoMercator } from 'd3-geo'
import { Country } from 'world-countries'
import countriesRaw from 'world-countries/countries.json'
import { PlotFigureRaw } from './Plot'
const countries: Country[] = countriesRaw as unknown as Country[]

export type GeoHeatMapData = Record<string, number>

export type GeoHeatMapProps = {
  worldGeoJson: GeoJSON.FeatureCollection
  data: GeoHeatMapData
  type: 'normal' | 'europe'
}

function mkProjection(type: 'normal' | 'europe') {
  if (type === 'normal') {
    return 'equirectangular'
  } else {
    return geoMercator().center([15, 50]).scale(600)
  }
}

function strokeWidth(type: 'normal' | 'europe') {
  return type === 'normal' ? 0.08 : 0.2
}

export function GeoHeatMap({
  data,
  type,
  worldGeoJson,
}: GeoHeatMapProps): JSX.Element {
  const minValue = Math.min(...Object.values(data))
  const maxValue = Math.max(...Object.values(data))

  const marks = Object.keys(data).map((name) => {
    const geoJson = countriesGeoJsonMap[name]
    if (geoJson == null) throw new Error(`Country not found: ${name}`)
    return Plot.geo(geoJson, {
      fill: getHeatmapColor(minValue, maxValue, data[name]),
      fillOpacity: 1,
      stroke: 'currentColor',
      strokeWidth: strokeWidth(type),
      tip: true,
    })
  })

  const options: Plot.PlotOptions = {
    width: 800,
    projection: mkProjection(type),
    style: {
      background: "white",
      border: "1px solid black",
    },
    marks: [
      Plot.geo(worldGeoJson, {
        fill: 'currentColor',
        fillOpacity: 0.02,
        stroke: 'currentColor',
        strokeWidth: strokeWidth(type),
      }),
      ...marks,
      Plot.tip(
        countriesGeoJson(Object.keys(data)),
        Plot.pointer(
          Plot.centroid({
            title: (d: GeoJSON.FeatureCollection) => {
              const cca2 = d.features[0].properties!.cca2
              const country = countries.find(
                (c) => c.cca2 === cca2.toUpperCase(),
              )
              if (country == null) throw new Error(`Country not found: ${cca2}`)
              return (
                country.flag +
                ' ' +
                country.name.common +
                ': ' +
                data[cca3ToCustomName(country.cca3)]
              )
            },
          }),
        ),
      ),
    ],
  }
  return <PlotFigureRaw {...options} />
}

function getHeatmapColor(min: number, max: number, value: number): string {
  const percent = (value - min) / (max - min)

  const colors2 = [
    '#ffffe5',
    '#ffffd1',
    '#ffffbd',
    '#ffffa9',
    '#ffff95',
    '#ffff81',
    '#ffff6d',
    '#ffeb59',
    '#ffd745',
    '#ffc331',
    '#ffaf1d',
    '#ff9b09',
    '#ff8700',
    '#ff7300',
    '#ff5f00',
    '#ff4b00',
    '#ff3700',
    '#ff2300',
    '#ff0f00',
    '#e00000',
  ]

  const colors = [
    "#fff5d7", "#ffeedb", "#ffe6de", "#ffdfdd", 
    "#ffd9db", "#ffd0d5", "#ffc9ce", "#ffc2c8", 
    "#ffbbbe", "#ffb4b3", "#ffada9", "#ffa59d", 
    "#ff9d92", "#ff9687", "#ff8e7c", "#ff8771", 
    "#ff8066", "#ff785a", "#ff714f", "#ff6950"
  ];

  const index = Math.min(
    Math.floor(percent * (colors.length - 1)),
    colors.length - 1,
  )

  return colors[index]
}

function cca3ToCustomName(cca3: string): string {
  if (cca3 === 'CZE') return 'Czech Republic'
  if (cca3 === 'GBR') return 'England'
  if (cca3 === 'USA') return 'USA'
  const country = countries.find((c) => c.cca3 === cca3.toUpperCase())
  if (country == null) throw new Error(`Country not found: ${cca3}`)
  return country.name.common
}
