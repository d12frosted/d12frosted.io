import { ThemeProvider } from '@/components/theme-provider'
import '@/styles/tailwind.css'
import type { Metadata } from 'next'
import type React from 'react'
import { ApplicationLayout } from './application-layout'
import { siteConfig } from '@/config/config'

export const metadata: Metadata = {
  title: {
    template: `%s - ${siteConfig.name}`,
    default: siteConfig.name,
  },
  description: siteConfig.description,
}

export default async function RootLayout({ children }: { children: React.ReactNode }) {
  return (
    <html lang="en" className="overflow-x-hidden antialiased" suppressHydrationWarning>
      <head>
        <link rel="preconnect" href="https://rsms.me/" />
        <link rel="stylesheet" href="https://rsms.me/inter/inter.css" />
      </head>
      <body className="overflow-x-hidden bg-paper text-ink dark:bg-zinc-950 dark:text-zinc-100">
        <ThemeProvider>
          <ApplicationLayout>{children}</ApplicationLayout>
        </ThemeProvider>
      </body>
    </html>
  )
}
