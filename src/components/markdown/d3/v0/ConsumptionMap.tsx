'use client'

import { PlotOptions } from '@observablehq/plot'
import * as Plot from '@observablehq/plot'
import { PlotFigure, PlotFigureMark } from '@/components/markdown/d3/v0/Plot'
import * as d3 from 'd3'
import { NoteLink } from '@/lib/note'
import moment from 'moment'

const dateFormat = "YYYY-MM-DD"
type Data = { date: moment.Moment; count: number }

export function ConsumptionMap({
  ratings,
}: {
  ratings: { date: Date; wine: NoteLink }[]
}) {
  const now = moment.utc();
  const dateMin = moment(now).subtract(364, 'days').utc()

  const ratingsByDate = d3.group(ratings, (d) => moment(d.date).format(dateFormat))
  const data: Data[] = d3.utcDays(dateMin.toDate(), now.toDate()).map((date) => {
    const m = moment(date).utc()
    return {
      date: m,
      count: ratingsByDate.get(m.format(dateFormat))?.length ?? 0,
    }
  })
  const shift = (7 - data[0].date.day()) % 7

  const options: PlotOptions = {
    padding: 0,
    x: {
      axis: 'top',
      tickFormat: (idx) => {
        const slice = data.slice(Math.max(0, shift + (idx - 1) * 7), shift + idx * 7)
        const firstDay = slice.find((x) => x.date.date() === 1)
        if (firstDay) return Plot.formatMonth('en', 'short')(firstDay.date.month())
        return ""
      },
      tickSize: 0,
    },
    y: { tickFormat: Plot.formatWeekday('en', 'short'), tickSize: 0 },
    fy: { tickFormat: '' },
    color: {
      scheme: 'greens',
      legend: false,
      label: 'Consumption',
    },
  }

  const marks: PlotFigureMark[] = [
    {
      type: 'cell',
      options: {
        x: (d: Data) => d3.utcWeek.count(dateMin.toDate(), d.date.toDate()),
        y: (d: Data) => d.date.day(),
        inset: 1.5,
        fill: (d: Data) =>
          // https://d3js.org/d3-scale-chromatic/sequential
          d.count > 0
            ? d3.scaleSequential(d3.interpolateGreens)(
                Math.ceil(1.5 * d.count) / 15,
              )
            : '#f3f4f6',
        rx: 3,
        ry: 3,
        stroke: '#f3f4f6',
      },
    },
    {
      type: 'tip',
      options: Plot.pointer({
        x: (d: Data) => d3.utcWeek.count(dateMin.toDate(), d.date.toDate()),
        y: (d: Data) => d.date.day(),
        title: (d: Data) =>
          `${d.count} wine(s) consumed\n${d.date.format("dddd, MMMM Do YYYY")} `,
      }),
    },
  ]
  return <PlotFigure data={data} options={options} marks={marks} />
}
