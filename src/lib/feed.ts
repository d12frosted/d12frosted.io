import { siteConfig } from '@/config/config'
import { getAllPosts } from '@/lib/posts'
import { Feed } from 'feed'

// Sanitize content for XML: remove invalid characters and escape CDATA terminators
function sanitizeForXml(content: string): string {
  return content
    // Remove invalid XML characters (control chars except tab, newline, carriage return)
    .replace(/[\x00-\x08\x0B\x0C\x0E-\x1F\x7F]/g, '')
    // Escape CDATA terminator to prevent breaking CDATA sections
    .replace(/]]>/g, ']]&gt;')
}

export const siteUrl =
  process.env.VERCEL_ENV === 'production'
    ? 'https://www.d12frosted.io'
    : process.env.VERCEL_URL
      ? `https://${process.env.VERCEL_URL}`
      : 'http://localhost:3000'

export async function generateFeed(): Promise<Feed> {
  const allPosts = await getAllPosts()
  const today = new Date()
  const posts = allPosts.filter((post) => !post.hide && post.published <= today)

  const feed = new Feed({
    title: siteConfig.name,
    description: siteConfig.description,
    id: siteUrl,
    link: siteUrl,
    language: 'en',
    favicon: `${siteUrl}/favicon.ico`,
    copyright: `All rights reserved ${new Date().getFullYear()}, ${siteConfig.name}`,
    author: {
      name: siteConfig.authors[0],
      link: siteUrl,
    },
    feedLinks: {
      rss2: `${siteUrl}/feed.xml`,
      atom: `${siteUrl}/atom.xml`,
    },
  })

  for (const post of posts) {
    feed.addItem({
      title: post.title,
      id: `${siteUrl}${post.href}`,
      link: `${siteUrl}${post.href}`,
      description: post.description,
      content: sanitizeForXml(post.content),
      author: post.authors.map((name) => ({ name })),
      date: post.updated && !isNaN(post.updated.getTime()) ? post.updated : post.published,
      published: post.published,
      category: post.tags.map((tag) => ({ name: tag })),
    })
  }

  return feed
}
