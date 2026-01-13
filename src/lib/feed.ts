import { siteConfig } from '@/config/config'
import { getAllPosts } from '@/lib/posts'
import { Feed } from 'feed'

export const siteUrl =
  process.env.VERCEL_URL
    ? `https://${process.env.VERCEL_URL}`
    : process.env.NODE_ENV === 'production'
      ? 'https://d12frosted.io'
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
      content: post.content,
      author: post.authors.map((name) => ({ name })),
      date: post.published,
      published: post.published,
      updated: post.updated,
      category: post.tags.map((tag) => ({ name: tag })),
    })
  }

  return feed
}
