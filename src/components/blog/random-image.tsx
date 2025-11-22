'use client'

import clsx from 'clsx'

export function RandomImage({
  tags = [],
  className,
  ...props
}: React.ComponentProps<'div'> & {
  tags?: string[]
}) {
  // Use same color logic as accent bars for consistency
  const getBackgroundColor = () => {
    const tag = tags[0]?.toLowerCase() || ''
    if (tag.includes('emacs') || tag.includes('org')) return 'bg-mp-blue'
    if (tag.includes('haskell') || tag.includes('code')) return 'bg-hp-green'
    if (tag.includes('tutorial') || tag.includes('guide')) return 'bg-xp-orange'
    return 'bg-magic-purple'
  }

  // Brutalist geometric pattern using repeating linear gradients
  const pattern = {
    backgroundImage: `
      repeating-linear-gradient(
        45deg,
        transparent,
        transparent 20px,
        rgba(255, 255, 255, 0.1) 20px,
        rgba(255, 255, 255, 0.1) 40px
      )
    `
  }

  return (
    <div
      className={clsx(className, getBackgroundColor())}
      style={pattern}
      {...props}
    />
  )
}
