'use client'

import { Direction, getRandomGradient } from '@/lib/random-gradient'
import clsx from 'clsx'
import { useEffect, useState } from 'react'

export function RandomImage({
  intensity,
  direction,
  className,
  ...props
}: React.ComponentProps<'div'> & {
  intensity?: number
  direction?: Direction
}) {
  const [gradient, setGradient] = useState<string>("")

  useEffect(() => {
    const v = getRandomGradient(intensity, direction)
    console.log(v)
    setGradient(v)
  }, [intensity, direction])

  return <div className={clsx(className, "bg-gradient-to-br from-neutral-300 to-slate-300", gradient)} {...props} />
}
