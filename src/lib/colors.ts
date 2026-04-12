/**
 * Get the accent color for a tag.
 *
 * Color scheme (jRPG-inspired):
 * - mp-blue: Emacs ecosystem (emacs, org-roam, vulpea, vui, etc.)
 * - hp-green: Code & tools (haskell, shell, fish, nix, git, etc.)
 * - xp-orange: Meta & releases (blog, release, readings, wine)
 * - critical-red: AI & thought pieces
 * - ink: Default
 */
export function getTagColor(tag: string): string {
  const t = tag.toLowerCase()

  // Emacs ecosystem - blue
  if (
    t === 'emacs' ||
    t === 'elisp' ||
    t === 'org-mode' ||
    t === 'org-roam' ||
    t === 'vulpea' ||
    t === 'vui' ||
    t === 'flyspell-correct' ||
    t === 'emacs-plus' ||
    t.includes('emacs') ||
    t.includes('org-')
  ) {
    return 'mp-blue'
  }

  // Code & tools - green
  if (
    t === 'haskell' ||
    t === 'shell' ||
    t === 'fish' ||
    t === 'nix' ||
    t === 'git' ||
    t === 'github' ||
    t === 'environment' ||
    t === 'macos' ||
    t === 'yabai' ||
    t === 'applescript'
  ) {
    return 'hp-green'
  }

  // Meta, releases, wine - orange
  if (
    t === 'blog' ||
    t === 'release' ||
    t === 'readings' ||
    t === 'wine' ||
    t === 'vino'
  ) {
    return 'xp-orange'
  }

  // AI & thought pieces - red
  if (t === 'ai') {
    return 'critical-red'
  }

  // Default
  return 'ink'
}

/**
 * Get accent color from an array of tags (uses first tag).
 */
export function getAccentColorFromTags(tags: string[]): string {
  if (!tags || tags.length === 0) return 'ink'
  return getTagColor(tags[0])
}

const bgClasses: Record<string, string> = {
  'mp-blue': 'bg-mp-blue',
  'hp-green': 'bg-hp-green',
  'xp-orange': 'bg-xp-orange',
  'critical-red': 'bg-critical-red',
  'ink': 'bg-ink',
}

const borderClasses: Record<string, string> = {
  'mp-blue': 'border-mp-blue',
  'hp-green': 'border-hp-green',
  'xp-orange': 'border-xp-orange',
  'critical-red': 'border-critical-red',
  'ink': 'border-ink',
}

/**
 * Get Tailwind background class for a color token.
 * Uses a static mapping so Tailwind's JIT can detect the classes.
 */
export function getAccentBgClass(tags: string[]): string {
  const color = getAccentColorFromTags(tags)
  return bgClasses[color] ?? 'bg-ink'
}

/**
 * Get Tailwind border class for a color token.
 * Uses a static mapping so Tailwind's JIT can detect the classes.
 */
export function getAccentBorderClass(tags: string[]): string {
  const color = getAccentColorFromTags(tags)
  return borderClasses[color] ?? 'border-ink'
}

/**
 * Get Tailwind background class for a tag color.
 */
export function getTagBgClass(tag: string): string {
  const color = getTagColor(tag)
  return bgClasses[color] ?? 'bg-ink'
}

/**
 * Get Tailwind border class for a tag color.
 */
export function getTagBorderClass(tag: string): string {
  const color = getTagColor(tag)
  return borderClasses[color] ?? 'border-ink'
}
