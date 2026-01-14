import { getEvents } from '@/data'
import '@/styles/tailwind.css'
import type { Metadata } from 'next'
import type React from 'react'
import { ApplicationLayout } from './application-layout'
import {siteConfig} from "@/config/config";
import { ThemeProvider, themeInitScript } from '@/components/theme-provider'

export const metadata: Metadata = {
  title: {
    template: `%s - ${siteConfig.name}`,
    default: siteConfig.name,
  },
  description: siteConfig.description,
  alternates: {
    types: {
      'application/rss+xml': '/feed.xml',
      'application/atom+xml': '/atom.xml',
    },
  },
}

export default async function RootLayout({ children }: { children: React.ReactNode }) {
  // let events = await getEvents()

  return (
    <html
      lang="en"
      className="overflow-x-hidden text-ink antialiased bg-paper"
      suppressHydrationWarning
    >
      <head>
        <script dangerouslySetInnerHTML={{ __html: themeInitScript }} />
        <link rel="preconnect" href="https://rsms.me/" />
        <link rel="stylesheet" href="https://rsms.me/inter/inter.css" />
      </head>
      <body className="overflow-x-hidden bg-paper text-ink">
        <ThemeProvider>
          <ApplicationLayout>{children}</ApplicationLayout>
        </ThemeProvider>
      </body>
    </html>
  )
}
