'use client'

import * as Plot from '@observablehq/plot'
import { useEffect, useRef } from 'react'

export function PlotFigureRaw(options: Plot.PlotOptions) {
  const containerRef = useRef<HTMLDivElement>(null)

  useEffect(() => {
    if (options == null) return
    if (containerRef.current == null) return

    const plot = Plot.plot(options)
    containerRef.current.append(plot)
    return () => plot.remove()
  }, [options])

  return <div ref={containerRef} className="w-full" />
}

export type PlotFigureMark =
  | {
      type: 'barX'
      options?: Plot.BarXOptions
      groupY?: Plot.GroupOutputs
    }
  | {
      type: 'barY'
      options?: Plot.BarYOptions
      groupX?: Plot.GroupOutputs
      binX?: {
        outputs?: Plot.BinOutputs
        options?: Plot.BinXInputs<Plot.BarYOptions>
      }
    }
  | {
      type: 'text'
      options?: Plot.TextOptions
    }
  | {
      type: 'ruleX'
      data?: Plot.Data
      options: Plot.RuleXOptions
    }
  | {
      type: 'ruleY'
      data?: Plot.Data
      options: Plot.RuleYOptions
    }
  | {
      type: 'line'
      options?: Plot.LineOptions
    }
  | {
      type: 'linearRegressionY'
      options?: Plot.LinearRegressionYOptions
    }
  | {
      type: 'axisX'
      options?: Plot.AxisXOptions
    }
  | {
      type: 'axisY'
      options?: Plot.AxisYOptions
    }
  | {
      type: 'cell'
      options?: Plot.CellOptions
    }
  | {
      type: 'tip'
      options?: Plot.TipOptions
    }
  | {
      type: 'frame'
      options?: Plot.FrameOptions
    }

export type PlotFigureProps = {
  data: Plot.Data
  options: Plot.PlotOptions
  marks: PlotFigureMark[]
}

export function PlotFigure(props: PlotFigureProps) {
  function toMark(mark: PlotFigureMark): Plot.Markish {
    if (mark.type === 'barX') {
      if (mark.groupY)
        return Plot.barX(props.data, Plot.groupY(mark.groupY, mark.options))
      return Plot.barX(props.data, mark.options)
    }

    if (mark.type === 'barY') {
      if (mark.groupX)
        return Plot.barY(props.data, Plot.groupX(mark.groupX, mark.options))
      if (mark.binX)
        return Plot.barY(
          props.data,
          Plot.binX(mark.binX.outputs, mark.binX.options),
        )
      return Plot.barY(props.data, mark.options)
    }

    if (mark.type === 'line') return Plot.line(props.data, mark.options)
    if (mark.type === 'linearRegressionY')
      return Plot.linearRegressionY(props.data, mark.options)

    if (mark.type === 'text') return Plot.text(props.data, mark.options)
    if (mark.type === 'ruleX') return Plot.ruleX(mark.data ?? [0], mark.options)
    if (mark.type === 'ruleY') return Plot.ruleY(mark.data ?? [0], mark.options)

    if (mark.type === 'axisX') return Plot.axisX(mark.options)
    if (mark.type === 'axisY') return Plot.axisY(mark.options)

    if (mark.type === 'cell') return Plot.cell(props.data, mark.options)

    if (mark.type === 'tip') return Plot.tip(props.data, mark.options)
    if (mark.type === 'frame') return Plot.frame(mark.options)
  }

  const options: Plot.PlotOptions = {
    ...props.options,
    width: 800,
    marks: props.marks.map(toMark),
  }
  return <PlotFigureRaw {...options} />
}
