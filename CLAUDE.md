# Claude Context - d12frosted.io

## Project Overview
Personal technical blog for Boris Buliga (d12frosted) - an engineering manager, Emacs enthusiast, and open source maintainer. Built with Next.js 15, Tailwind CSS v4, and a custom brutalist + jRPG-inspired design aesthetic.

## Design Philosophy

### Visual Style: Brutalist + jRPG + Emacs
- **Brutalist elements**: Bold borders, sharp corners (no border-radius), stark contrasts, geometric accent bars
- **jRPG inspiration**: Status bar-style color accents on cards, color-coded categories (blue=Emacs, green=code, orange=tutorials)
- **Emacs aesthetic**: Clean, precise spacing inspired by Nicolas P. Rougier's work, monospace metadata
- **White theme**: User specifically loves white/light themes

### Color Palette
All colors defined in `src/styles/tailwind.css` under `@theme`:

```
--color-paper: #FAFAF8        /* Warm background */
--color-canvas: #FFFFFF       /* Pure white content */
--color-ink: #1A1A1A         /* Primary text */
--color-ink-muted: #6B6B6B   /* Secondary text */
--color-border-soft: #E8E6E3 /* Subtle borders */
--color-code-bg: #F0F4F8     /* Light blue for special blocks */

/* jRPG status colors */
--color-hp-green: #7FB069
--color-mp-blue: #5F87AF     /* Primary accent */
--color-xp-orange: #E9AE4E
--color-critical-red: #D66853
```

### Typography
- **Sans**: Inter (from Google Fonts)
- **Mono**: IBM Plex Mono (primary), Source Code Pro (fallback)
- **Headings**: Bold, tight tracking (`tracking-tighter`)
- **Body**: Generous line-height (1.7 - `leading-relaxed-plus`)
- **Metadata**: Monospace with tabular numbers, uppercase, wide tracking

## Key Technical Decisions

### Content Management
- **Blog posts**: Synced from Vulpea database (org notes)
- **DO NOT EDIT**: Post content files are managed externally - only edit layout/styling
- Posts are markdown with custom handlers for D3 visualizations, related posts, etc.

### Dynamic Data with ISR
- **GitHub stars**: Fetched at build time via `src/lib/github.ts`
- **ISR enabled**: Pages regenerate every 24 hours (`export const revalidate = 86400`)
- Home page (`/`) and Projects page (`/projects`) both use ISR
- No external cron needed - Next.js handles it automatically

### Responsive Design
- Mobile-first approach with `sm:` and `lg:` breakpoints
- Navbar: Compact on mobile (smaller gaps, hidden divider, smaller text)
- Content padding: `px-4 py-8` on mobile, `px-16 py-20` on desktop
- Code blocks: Horizontally scrollable on mobile (`overflow-x-auto`)
- Body/HTML: `overflow-x-hidden` to prevent horizontal scroll issues

## Page Structure

### Home Page (`/`)
**Two-column layout** (stacks on mobile):
- **Left column (400px)**: About section + Quick stats
- **Right column**: Latest posts + Featured projects

**No main heading** - jumps straight into content (navbar already shows name)

### Posts Page (`/posts`)
- **Grouped by year** with bold year headers
- Shows post count per year
- 2-column grid of cards (no images, color-coded accent bars)

### Projects Page (`/projects`)
- Grid of project cards (2 columns on desktop)
- Dynamic GitHub star counts
- Callout sections for GitHub profile and Barberry Garden

### Individual Post Pages (`/posts/[slug]`)
- Clean header with monospace metadata
- Blue geometric divider
- Related posts section (light blue background, blue left border)

## Component Patterns

### Blog Post Cards
- No images - text-only cards with colored accent bars
- Colored top bar (jRPG status bar style) based on first tag
- Monospace metadata with • separators
- Hover: shadow-2xl

### Color-Coded Accents
Color logic centralized in `src/lib/colors.ts`. Cards and projects use `getAccentColorFromTags()`:
- Emacs ecosystem (emacs, org-roam, vulpea, vui, elisp, etc.) → `mp-blue`
- Code & tools (haskell, shell, fish, nix, git, macos, etc.) → `hp-green`
- Meta & releases (blog, release, readings, wine) → `xp-orange`
- AI → `critical-red`
- Default → `ink` (black)

### Code Blocks
- **White background** for readability (#FFFFFF)
- **Blue left border** (3px solid mp-blue)
- **Custom syntax theme** using our color palette
- **Sharp corners** (no border-radius)
- **IBM Plex Mono** font
- Defined in: `src/components/markdown/syntax-highlighter.tsx`

### Related Posts
Special markdown block (````related_posts```):
- Light blue background (`code-bg`)
- Blue left border (4px)
- Links: underlined, blue, hover to dark
- No top margin on heading (use `!mt-0`)

## Important Files

### Configuration
- `src/config/config.ts` - Site metadata, links
- `src/config/fonts.ts` - Font setup (Inter, IBM Plex Mono)
- `tailwind.config.ts` - Legacy config (v3 style)
- `src/styles/tailwind.css` - Tailwind v4 theme, custom colors

### Layouts
- `src/app/layout.tsx` - Root layout with fonts
- `src/app/application-layout.tsx` - Main layout with navbar
- `src/components/navbar.tsx` - Navigation components

### Blog Components
- `src/components/blog/card.tsx` - FeaturedPostCard, RegularPostCard
- `src/components/markdown/custom-markdown.tsx` - Markdown renderer with custom handlers
- `src/components/markdown/syntax-highlighter.tsx` - Code block styling

### Utilities
- `src/lib/colors.ts` - Tag-to-color mapping for accent colors
- `src/lib/github.ts` - GitHub API fetching for star counts
- `src/lib/posts.ts` - Post loading/filtering

## Common Tasks

### Adding a New Color
1. Add to `src/styles/tailwind.css` under `@theme` as `--color-name`
2. Use in components as `bg-name`, `text-name`, `border-name`

### Updating Project List
Edit `src/app/projects/page.tsx`:
- Projects array defines repo names and metadata
- Stars fetched automatically from GitHub API
- ISR keeps data fresh (rebuilds every 24 hours)

### Fixing Responsive Issues
1. Check mobile padding/margins (should be smaller)
2. Use `overflow-x-hidden` on containers if horizontal scroll appears
3. Navbar is tight on mobile - gaps, padding, font sizes all reduce with `sm:` variants
4. Test with browser dev tools at 375px width

### Working with Prose Styles
- Tailwind Typography plugin adds default styles to `.prose` elements
- Use `!` prefix to override: `!mt-0`, `!mb-6`
- Custom prose utilities in `src/styles/tailwind.css`

## Design Principles to Maintain

1. **Sharp, not rounded**: No `rounded-lg`, use sharp corners or `border` only
2. **Geometric accents**: Colored bars (h-1 w-16) before section headings
3. **Monospace metadata**: Dates, tags, stats all use `font-mono` with `uppercase` and `tracking-wider`
4. **Spacious**: Generous padding (p-8, p-12) and gaps (gap-12, gap-20)
5. **Bold typography**: Font weights are bold (700) for headings
6. **Subtle underlines**: Links use `decoration-{color}/30` for subtle, elegant underlines

## Things to Avoid

- **Don't edit post content files** - they're synced from external source
- **Don't add rounded corners** - breaks brutalist aesthetic
- **Don't use generic grays** - use our named colors (ink, ink-muted, etc.)
- **Don't add emojis** unless explicitly requested
- **Don't create documentation files** proactively (only when asked)
- **Don't hardcode star counts** - they're fetched dynamically

## Known Issues / Watch-outs

- Tailwind v4 uses different syntax: colors defined in CSS with `--color-*`, not in JS config
- Image component needs explicit width/height to avoid distortion
- Prose styles can override custom margins - use `!important` prefix when needed
- Related posts block needs `!mt-0` on heading to remove prose top margin
- Code blocks need wrapper div for full-height left border
- **Tailwind responsive utilities don't work reliably in all contexts** (e.g., `lg:block` in markdown components). Use plain CSS with media queries in `src/styles/blog.css` instead. See `.heading-level-indicator` for an example.

## Project Context

**Owner**: Boris Buliga (@d12frosted)
- Based in Kyiv, Ukraine (from Chișinău, Moldova)
- Engineering manager
- Emacs enthusiast with major open source projects (homebrew-emacs-plus: 2.7k stars)
- Wine aficionado (runs barberry.io)
- jRPG gamer (Final Fantasy XIV)

**Name origin**: "d12frosted" = 12-sided dice from Chessex frosted series (tabletop RPG reference)

---

Last updated: 2025-12-15
