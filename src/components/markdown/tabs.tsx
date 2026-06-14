'use client'

import clsx from 'clsx'
import { KeyboardEvent, ReactNode, useId, useRef, useState } from 'react'

export type Tab = {
  label: string
  panel: ReactNode
}

/**
 * Tabbed content, styled after an Emacs `tab-line`: a flush strip of
 * monospace buffer-tabs sitting atop the panel, the active one owning the
 * line below it. Used in posts to switch between, say, a code example and a
 * screenshot of it running.
 */
export function Tabs({ tabs }: { tabs: Tab[] }): JSX.Element | null {
  const [active, setActive] = useState(0)
  const baseId = useId()
  const tabRefs = useRef<(HTMLButtonElement | null)[]>([])

  if (tabs.length === 0) {
    return null
  }
  // A lone tab needs no switcher — render it plainly.
  if (tabs.length === 1) {
    return <div className="tabs-block my-8">{tabs[0].panel}</div>
  }

  function focusTab(index: number): void {
    setActive(index)
    tabRefs.current[index]?.focus()
  }

  function onKeyDown(event: KeyboardEvent<HTMLDivElement>): void {
    const count = tabs.length
    switch (event.key) {
      case 'ArrowRight':
      case 'ArrowDown':
        event.preventDefault()
        focusTab((active + 1) % count)
        break
      case 'ArrowLeft':
      case 'ArrowUp':
        event.preventDefault()
        focusTab((active - 1 + count) % count)
        break
      case 'Home':
        event.preventDefault()
        focusTab(0)
        break
      case 'End':
        event.preventDefault()
        focusTab(count - 1)
        break
    }
  }

  return (
    <div className="tabs-block my-8">
      <div
        role="tablist"
        aria-label="Alternate views"
        onKeyDown={onKeyDown}
        className="flex flex-wrap items-stretch border-b border-border-soft"
      >
        {tabs.map((tab, index) => {
          const selected = index === active
          return (
            <button
              key={index}
              type="button"
              role="tab"
              id={`${baseId}-tab-${index}`}
              aria-selected={selected}
              aria-controls={`${baseId}-panel-${index}`}
              tabIndex={selected ? 0 : -1}
              ref={(el) => {
                tabRefs.current[index] = el
              }}
              onClick={() => setActive(index)}
              className={clsx(
                '-mb-px border-b-2 px-4 py-2 font-mono text-xs uppercase tracking-wider transition-colors',
                'focus-visible:outline-none focus-visible:ring-2 focus-visible:ring-mp-blue/40',
                selected
                  ? 'border-mp-blue text-ink dark:text-white'
                  : 'border-transparent text-ink-muted hover:border-border-soft hover:text-ink dark:hover:text-white'
              )}
            >
              {tab.label}
            </button>
          )
        })}
      </div>
      <div
        key={active}
        role="tabpanel"
        id={`${baseId}-panel-${active}`}
        aria-labelledby={`${baseId}-tab-${active}`}
        className="tabs-panel pt-5"
      >
        {tabs[active].panel}
      </div>
    </div>
  )
}
