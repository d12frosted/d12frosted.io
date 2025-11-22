After years of the same look, I decided it was time to give this site a proper redesign. Not just a few color tweaks or font changes—a complete visual overhaul from the ground up.

# The Vision: Brutalist Meets jRPG

I've always loved clean, functional design that doesn't hide behind unnecessary flourishes. At the same time, I'm a huge jRPG fan (yes, I still play FFXIV), and I wanted to bring some of that visual language into the site. The result? A brutalist aesthetic with jRPG-inspired status bar accents.

Think sharp corners instead of rounded edges, bold geometric accent bars, and color-coded sections that feel like character stats in a menu screen. Blue for technical posts, green for code projects, orange for tutorials—each category gets its own "element type" color.

# Design Principles

The redesign follows a few core principles:

## Sharp, Not Rounded

No `border-radius` anywhere. Every element has crisp, clean edges. Geometric accent bars use solid colors and stark contrasts.

## Emacs-Inspired Spacing

As an Emacs enthusiast, I drew heavy inspiration from Nicolas P. Rougier's work on clean, precise spacing. Everything breathes—generous padding, clear visual hierarchy, and monospace metadata for that terminal aesthetic.

## White Theme First

I know dark mode is all the rage, but I genuinely prefer light themes. The site uses a warm off-white background (`#FAFAF8`) with pure white content areas and a carefully chosen color palette:

- **Paper** (`#FAFAF8`) — Warm background
- **Canvas** (`#FFFFFF`) — Pure white content
- **Ink** (`#1A1A1A`) — Primary text
- **MP Blue** (`#5F87AF`) — Primary accent (like magic points!)
- **HP Green** (`#7FB069`) — Success/code color (health points!)
- **XP Orange** (`#E9AE4E`) — Warning/tutorial color (experience!)

## Typography

Switched to **Inter** for sans-serif and **IBM Plex Mono** as the primary monospace font. Headings use bold weights with tight tracking, body text has generous line-height (1.7), and all metadata is rendered in monospace with uppercase, wide tracking for that classic terminal look.

# From Hakyll to Next.js

This site was previously built on top of Hakyll, the Haskell static site generator. While I love Haskell and functional programming, any serious design work required way too much effort from my non-frontend-developer ass. Fighting with templates and rebuilding the entire site just to tweak spacing got old fast.

Next.js with Tailwind turned out to be exactly what I needed—quick iteration, hot reload, and a massive ecosystem of components and tools. I could finally focus on design instead of wrestling with build systems.

# What Changed

## Home Page

Completely reworked the home page into a two-column layout. Left side has a condensed about section and quick stats. Right side shows latest posts and featured projects. No redundant heading—the navbar already shows my name, so why repeat it?

## Posts Page

Blog posts are now grouped by year with bold year headers. Each post card features a large image, colored accent bar based on the first tag, and monospace metadata. The cards have that jRPG status bar feel—hover effects add shadow for depth.

## Projects Page

New dedicated projects page with a grid layout. Each project card shows the GitHub repo with live star counts, description, and color-coded left borders. I also added callout sections for my GitHub profile and Barberry Garden (my wine project).

## Individual Posts

Cleaner post headers with monospace metadata, blue geometric dividers, and a refined typography scale. Code blocks now have a white background with a 3px blue left border—much more readable than the old style.

## Code Syntax Highlighting

Custom syntax theme using the site's color palette. Everything is sharp corners, clean backgrounds, and that signature blue accent border.

# What's Next

Now that the site redesign is done, I'm back to working on [Vulpea v2](https://github.com/d12frosted/vulpea/tree/v2-rewrite)—a complete rewrite of my Emacs note-taking toolkit. There's a lot to cover there, and I'll definitely be writing about it. Expect more posts about task management, org-roam workflows, and how I use Emacs for everything from project planning to wine notes.

But first, let me enjoy this fresh coat of paint for a bit.

—

*Built with Next.js 15, Tailwind CSS v4, and an unhealthy amount of time spent in browser DevTools. Just kidding, Claude Code FTW.*
