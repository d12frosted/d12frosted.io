'use client'

import * as React from 'react'
import { Prism as Highlighter, SyntaxHighlighterProps } from 'react-syntax-highlighter'
import { base16AteliersulphurpoolLight as style } from 'react-syntax-highlighter/dist/esm/styles/prism'

import { fontMono } from '@/config/fonts'

export const SyntaxHighlighter: React.FC<Omit<SyntaxHighlighterProps, 'style' | 'PreTag' | 'codeTagProps'>> = ({
  language,
  children,
  ...props
}) => {
  return (
    // @ts-ignore
    <Highlighter {...props} style={style} language={language} PreTag="div" codeTagProps={{ style: fontMono.style }}>
      {children}
    </Highlighter>
  )
}
