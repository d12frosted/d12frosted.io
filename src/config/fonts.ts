import { IBM_Plex_Mono, Source_Code_Pro as FontMonoFallback, Inter as FontSans } from "next/font/google"

export const fontSans = FontSans({
  subsets: ["latin"],
  variable: "--font-sans",
})

export const fontMono = IBM_Plex_Mono({
  weight: ['400', '500', '600'],
  subsets: ["latin"],
  variable: "--font-mono",
})

export const fontMonoFallback = FontMonoFallback({
  subsets: ["latin"],
  variable: "--font-mono-fallback",
})
