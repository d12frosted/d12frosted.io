'use client'

import * as React from 'react'
import { Prism as Highlighter, SyntaxHighlighterProps } from 'react-syntax-highlighter'

// Base style - background handled by CSS for dark mode support
const baseStyle = {
  'pre[class*="language-"]': {
    background: 'transparent',
    borderRadius: '0',
    margin: '0',
    padding: '1.5rem',
    border: 'none',
    fontFamily: 'IBM Plex Mono, Source Code Pro, monospace',
    fontSize: '0.875rem',
    lineHeight: '1.7',
  },
  'code[class*="language-"]': {
    background: 'transparent',
    fontFamily: 'IBM Plex Mono, Source Code Pro, monospace',
  },
  // Token colors using CSS variables - these work in both light and dark
  'comment': { color: 'var(--syntax-comment)', fontStyle: 'italic' },
  'prolog': { color: 'var(--syntax-comment)' },
  'doctype': { color: 'var(--syntax-comment)' },
  'cdata': { color: 'var(--syntax-comment)' },
  'punctuation': { color: 'var(--syntax-text)' },
  'property': { color: 'var(--syntax-keyword)' },
  'tag': { color: 'var(--syntax-keyword)' },
  'boolean': { color: 'var(--syntax-keyword)' },
  'number': { color: 'var(--syntax-number)' },
  'constant': { color: 'var(--syntax-number)' },
  'symbol': { color: 'var(--syntax-number)' },
  'deleted': { color: 'var(--syntax-deleted)' },
  'selector': { color: 'var(--syntax-string)' },
  'attr-name': { color: 'var(--syntax-string)' },
  'string': { color: 'var(--syntax-string)' },
  'char': { color: 'var(--syntax-string)' },
  'builtin': { color: 'var(--syntax-string)' },
  'inserted': { color: 'var(--syntax-string)' },
  'operator': { color: 'var(--syntax-text)' },
  'entity': { color: 'var(--syntax-text)' },
  'url': { color: 'var(--syntax-keyword)' },
  'variable': { color: 'var(--syntax-text)' },
  'atrule': { color: 'var(--syntax-keyword)' },
  'attr-value': { color: 'var(--syntax-string)' },
  'function': { color: 'var(--syntax-keyword)' },
  'class-name': { color: 'var(--syntax-number)' },
  'keyword': { color: 'var(--syntax-keyword)', fontWeight: '600' },
  'regex': { color: 'var(--syntax-string)' },
  'important': { color: 'var(--syntax-deleted)', fontWeight: '600' },
}

export const SyntaxHighlighter: React.FC<Omit<SyntaxHighlighterProps, 'style' | 'PreTag' | 'codeTagProps'>> = ({
  language,
  children,
  ...props
}) => {
  return (
    <div className="syntax-highlighter border-l-[3px] border-mp-blue overflow-x-auto">
      {/* @ts-ignore */}
      <Highlighter
        {...props}
        style={baseStyle}
        language={language}
        PreTag="div"
        codeTagProps={{ className: 'font-mono' }}
      >
        {children}
      </Highlighter>
    </div>
  )
}
