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

/**
 * Get Tailwind background class for a tag color.
 */
export function getTagBgClass(tag: string): string {
  return `bg-${getTagColor(tag)}`
}

/**
 * Get Tailwind border class for a tag color.
 */
export function getTagBorderClass(tag: string): string {
  return `border-${getTagColor(tag)}`
}
