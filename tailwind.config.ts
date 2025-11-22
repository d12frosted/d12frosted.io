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
        // New Emacs + jRPG inspired palette
        'paper': '#FAFAF8',
        'canvas': '#FFFFFF',
        'ink': '#1A1A1A',
        'ink-muted': '#6B6B6B',
        'border-soft': '#E8E6E3',
        'code-bg': '#F0F4F8',
        // jRPG status colors
        'hp-green': '#7FB069',
        'mp-blue': '#5F87AF',
        'xp-orange': '#E9AE4E',
        'critical-red': '#D66853',
        // Legacy colors for compatibility
        successful: '#E9F7DF',
        critical: '#EFC6BE',
      },
      fontFamily: {
        sans: ['Inter', ...defaultTheme.fontFamily.sans],
        mono: ['IBM Plex Mono', 'Source Code Pro', 'monospace'],
      },
      spacing: {
        18: '4.5rem',
        112: '28rem',
        120: '30rem',
      },
      letterSpacing: {
        'tighter': '-0.02em',
      },
      lineHeight: {
        'relaxed-plus': '1.7',
      },
      typography: {
        DEFAULT: {
          css: {
            'blockquote p:first-of-type::before': false,
            'blockquote p:first-of-type::after': false,
            'blockquote': {
              borderLeftWidth: '3px',
              borderLeftColor: '#5F87AF',
              fontStyle: 'normal',
              paddingLeft: '1rem',
            },
            'code': {
              backgroundColor: '#F0F4F8',
              padding: '0.125rem 0.25rem',
              borderRadius: '0',
              fontWeight: '500',
            },
            'code::before': {
              content: '""',
            },
            'code::after': {
              content: '""',
            },
            'pre': {
              backgroundColor: '#F0F4F8',
              borderLeft: '3px solid #5F87AF',
              borderRadius: '0',
            },
          },
        },
      },
    },
  },
  plugins: [typographyPlugin],
} satisfies Config
