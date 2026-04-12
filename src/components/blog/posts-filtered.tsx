'use client'

import { getAccentColorFromTags, getTagColor } from '@/lib/colors'
import { useState } from 'react'
import { RegularPostCard } from './card'
import type { BlogPost } from '@/lib/posts'

type SerializedPost = Omit<BlogPost, 'published' | 'updated' | 'content'> & {
  published: string
  updated: string
  content?: string
}

type Category = {
  id: string
  label: string
  color: string
}

const categories: Category[] = [
  { id: 'all', label: 'All', color: 'ink' },
  { id: 'mp-blue', label: 'Emacs', color: 'mp-blue' },
  { id: 'hp-green', label: 'Code & Tools', color: 'hp-green' },
  { id: 'xp-orange', label: 'Meta', color: 'xp-orange' },
  { id: 'critical-red', label: 'AI', color: 'critical-red' },
]

const activeBgClasses: Record<string, string> = {
  'ink': 'bg-ink text-white dark:bg-white dark:text-ink',
  'mp-blue': 'bg-mp-blue text-white',
  'hp-green': 'bg-hp-green text-white',
  'xp-orange': 'bg-xp-orange text-white',
  'critical-red': 'bg-critical-red text-white',
}

const inactiveBgClasses: Record<string, string> = {
  'ink': 'border-2 border-ink text-ink hover:bg-ink hover:text-white dark:border-white dark:text-white dark:hover:bg-white dark:hover:text-ink',
  'mp-blue': 'border-2 border-mp-blue text-mp-blue hover:bg-mp-blue hover:text-white',
  'hp-green': 'border-2 border-hp-green text-hp-green hover:bg-hp-green hover:text-white',
  'xp-orange': 'border-2 border-xp-orange text-xp-orange hover:bg-xp-orange hover:text-white',
  'critical-red': 'border-2 border-critical-red text-critical-red hover:bg-critical-red hover:text-white',
}

function deserializePost(post: SerializedPost): BlogPost {
  return {
    ...post,
    content: post.content ?? '',
    published: new Date(post.published),
    updated: new Date(post.updated),
  }
}

export function PostsFiltered({
  pinnedPosts: serializedPinned,
  regularPosts: serializedRegular,
  totalCount,
  yearCount,
}: {
  pinnedPosts: SerializedPost[]
  regularPosts: SerializedPost[]
  totalCount: number
  yearCount: number
}) {
  const [activeCategory, setActiveCategory] = useState('all')

  const pinnedPosts = serializedPinned.map(deserializePost)
  const regularPosts = serializedRegular.map(deserializePost)

  const filterPost = (post: BlogPost) => {
    if (activeCategory === 'all') return true
    return getAccentColorFromTags(post.tags) === activeCategory
  }

  const filteredPinned = pinnedPosts.filter(filterPost)
  const filteredRegular = regularPosts.filter(filterPost)

  const postsByYear = filteredRegular.reduce(
    (acc, post) => {
      const year = post.published.getFullYear()
      if (!acc[year]) acc[year] = []
      acc[year].push(post)
      return acc
    },
    {} as Record<number, BlogPost[]>
  )

  const years = Object.keys(postsByYear)
    .map(Number)
    .sort((a, b) => b - a)

  const filteredCount = filteredPinned.length + filteredRegular.length

  return (
    <>
      {/* Filter bar */}
      <div className="mb-16 flex flex-wrap gap-2">
        {categories.map((cat) => {
          const isActive = activeCategory === cat.id
          return (
            <button
              key={cat.id}
              onClick={() => setActiveCategory(cat.id)}
              className={`px-4 py-1.5 font-mono text-xs uppercase tracking-wider transition-colors ${
                isActive ? activeBgClasses[cat.color] : inactiveBgClasses[cat.color]
              }`}
            >
              {cat.label}
            </button>
          )
        })}
      </div>

      {/* Post count */}
      <p className="-mt-10 mb-16 font-mono text-sm uppercase tracking-wider text-ink-muted">
        {activeCategory === 'all'
          ? `${totalCount} posts across ${yearCount} years`
          : `${filteredCount} posts`}
      </p>

      {/* Pinned posts */}
      {filteredPinned.length > 0 && (
        <section className="mb-16">
          <div className="mb-8 flex items-center gap-6">
            <div className="h-1 w-12 bg-mp-blue" />
            <h2 className="text-4xl font-bold tracking-tight text-ink dark:text-white">Pinned</h2>
          </div>
          <div className="grid gap-6 lg:grid-cols-2">
            {filteredPinned.map((post) => (
              <RegularPostCard key={post.id} post={post} />
            ))}
          </div>
        </section>
      )}

      {/* Posts grouped by year */}
      {years.length > 0 ? (
        <div className="space-y-16">
          {years.map((year) => (
            <section key={year}>
              <div className="mb-8 flex items-center gap-6">
                <div className="h-1 w-12 bg-xp-orange" />
                <h2 className="text-4xl font-bold tabular-nums tracking-tight text-ink dark:text-white">
                  {year}
                </h2>
                <div className="font-mono text-sm text-ink-muted">
                  {postsByYear[year].length} {postsByYear[year].length === 1 ? 'post' : 'posts'}
                </div>
              </div>
              <div className="grid gap-6 lg:grid-cols-2">
                {postsByYear[year].map((post) => (
                  <RegularPostCard key={post.id} post={post} />
                ))}
              </div>
            </section>
          ))}
        </div>
      ) : (
        <div className="py-16 text-center">
          <p className="font-mono text-sm uppercase tracking-wider text-ink-muted">
            No posts in this category
          </p>
        </div>
      )}
    </>
  )
}
