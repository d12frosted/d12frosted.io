'use client'

import { useEffect, useRef } from 'react'
import { useTheme } from '@/components/theme-provider'

export function Comments(props: Omit<React.ComponentProps<'section'>, 'ref' | 'style'>) {
  const { theme } = useTheme()
  const containerRef = useRef<HTMLElement>(null)

  useEffect(() => {
    const element = containerRef.current
    if (!element) return

    // Clear existing content
    element.innerHTML = ''

    const scriptElement = document.createElement('script')
    scriptElement.setAttribute('src', 'https://utteranc.es/client.js')
    scriptElement.setAttribute('repo', 'd12frosted/d12frosted.io')
    scriptElement.setAttribute('issue-term', 'pathname')
    scriptElement.setAttribute('theme', theme === 'dark' ? 'github-dark' : 'github-light')
    scriptElement.setAttribute('crossorigin', 'anonymous')
    scriptElement.setAttribute('async', 'true')
    element.appendChild(scriptElement)
  }, [theme])

  return <section {...props} style={{ width: '100%' }} ref={containerRef} />
}
