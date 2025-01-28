import typographyPlugin from '@tailwindcss/typography'
import { type Config } from 'tailwindcss'
import defaultTheme from 'tailwindcss/defaultTheme'

module.exports = {
  content: [
    './src/**/*.{js,ts,jsx,tsx,mdx}',
  ],
  theme: {
    extend: {
      colors: {
        successful: '#E9F7DF',
        critical: '#EFC6BE',
      },
      fontFamily: {
        sans: ['Satoshi', ...defaultTheme.fontFamily.sans],
        mono: ['Source Code Pro', 'monospace'],
      },
      spacing: {
        18: '4.5rem',
        112: '28rem',
        120: '30rem',
      },
      typography: {
        DEFAULT: {
          css: {
            'blockquote p:first-of-type::before': false,
            'blockquote p:first-of-type::after': false,
          },
        },
      },
    },
  },
  plugins: [typographyPlugin],
} satisfies Config
