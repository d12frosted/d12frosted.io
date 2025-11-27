'use client'

import * as React from 'react'
import { Prism as Highlighter, SyntaxHighlighterProps } from 'react-syntax-highlighter'

// Custom syntax theme matching our brutalist + jRPG color palette
const customStyle = {
  'pre[class*="language-"]': {
    background: '#FFFFFF !important', // white for better readability
    borderRadius: '0 !important',
    margin: '0 !important',
    padding: '1.5rem !important',
    border: 'none !important',
    color: '#1A1A1A', // ink
    fontFamily: 'IBM Plex Mono, Source Code Pro, monospace',
    fontSize: '0.875rem',
    lineHeight: '1.7',
  },
  'code[class*="language-"]': {
    background: 'transparent !important',
    color: '#1A1A1A', // ink
    fontFamily: 'IBM Plex Mono, Source Code Pro, monospace',
  },
  // Token colors using our palette
  'comment': { color: '#6B6B6B', fontStyle: 'italic' }, // ink-muted
  'prolog': { color: '#6B6B6B' },
  'doctype': { color: '#6B6B6B' },
  'cdata': { color: '#6B6B6B' },
  'punctuation': { color: '#1A1A1A' }, // ink
  'property': { color: '#5F87AF' }, // mp-blue
  'tag': { color: '#5F87AF' },
  'boolean': { color: '#5F87AF' },
  'number': { color: '#E9AE4E' }, // xp-orange
  'constant': { color: '#E9AE4E' },
  'symbol': { color: '#E9AE4E' },
  'deleted': { color: '#D66853' }, // critical-red
  'selector': { color: '#7FB069' }, // hp-green
  'attr-name': { color: '#7FB069' },
  'string': { color: '#7FB069' },
  'char': { color: '#7FB069' },
  'builtin': { color: '#7FB069' },
  'inserted': { color: '#7FB069' },
  'operator': { color: '#1A1A1A' },
  'entity': { color: '#1A1A1A' },
  'url': { color: '#5F87AF' },
  'variable': { color: '#1A1A1A' },
  'atrule': { color: '#5F87AF' },
  'attr-value': { color: '#7FB069' },
  'function': { color: '#5F87AF' },
  'class-name': { color: '#E9AE4E' },
  'keyword': { color: '#5F87AF', fontWeight: '600' },
  'regex': { color: '#7FB069' },
  'important': { color: '#D66853', fontWeight: '600' },
}

export const SyntaxHighlighter: React.FC<Omit<SyntaxHighlighterProps, 'style' | 'PreTag' | 'codeTagProps'>> = ({
  language,
  children,
  ...props
}) => {
  return (
    <div className="overflow-x-auto border-l-[3px] border-mp-blue bg-white dark:bg-code-bg-dark">
      {/* @ts-ignore */}
      <Highlighter
        {...props}
        style={customStyle}
        language={language}
        PreTag="div"
        codeTagProps={{ className: 'font-mono dark:text-ink-dark' }}
      >
        {children}
      </Highlighter>
    </div>
  )
}
