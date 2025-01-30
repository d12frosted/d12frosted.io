'use client'

import { useEffect, useRef } from 'react'
import * as d3 from 'd3'

type Datum = { name: string; value: number }

export type DonutProps = {
  data: Datum[]
  height?: number
}
export function Donut(props: DonutProps) {
  const height = props.height ?? 500
  const width = Math.min(height, 500)
  const radius = Math.min(width, height) / 2

  const { data } = props

  const graphRef = useRef(null)
  useEffect(() => {
    const arc: d3.Arc<any, any> = d3
      .arc()
      .innerRadius(radius * 0.67)
      .outerRadius(radius - 1)

    const pie = d3
      .pie<Datum>()
      .padAngle(1 / radius)
      .sort(null)
      .value((d) => d.value)

    const color: d3.ScaleOrdinal<string, any, never> = d3
      .scaleOrdinal()
      .domain(data.map((d) => d.name))
      .range(
        d3
          .quantize((t) => d3.interpolateSpectral(t * 0.8 + 0.1), data.length)
          .reverse(),
      )

    const svg = d3
      .select(graphRef.current)
      .append('svg')
      .attr('width', width)
      .attr('height', height)
      .attr('viewBox', [-width / 2, -height / 2, width, height])
      .attr('style', 'max-width: 100%; height: auto;')

    svg
      .append('g')
      .selectAll()
      .data(pie(data))
      .join('path')
      .attr('fill', (d) => color(d.data.name))
      .attr('d', arc)
      .append('title')
      .text((d) => `${d.data.name}: ${d.data.value.toLocaleString()}`)

    svg
      .append('g')
      .attr('font-family', 'sans-serif')
      .attr('font-size', 12)
      .attr('text-anchor', 'middle')
      .selectAll()
      .data(pie(data))
      .join('text')
      .attr('transform', (d) => `translate(${arc.centroid(d)})`)
      .call((text) =>
        text
          .append('tspan')
          .attr('y', '-0.4em')
          .attr('font-weight', 'bold')
          .text((d) => d.data.name),
      )
      .call((text) =>
        text
          .filter((d) => d.endAngle - d.startAngle > 0.25)
          .append('tspan')
          .attr('x', 0)
          .attr('y', '0.7em')
          .attr('fill-opacity', 0.7)
          .text((d) => d.data.value.toLocaleString('en-US')),
      )

    return () => {
      d3.select(graphRef.current).select('svg').remove()
    }
  }, [])
  return <div ref={graphRef}></div>
}
