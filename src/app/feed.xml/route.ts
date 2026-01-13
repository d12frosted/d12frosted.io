import { siteConfig } from '@/config/config'
import { getAllPosts } from '@/lib/posts'
import { Feed } from 'feed'

export const revalidate = 86400 // 24 hours

const siteUrl =
  process.env.VERCEL_URL
    ? `https://${process.env.VERCEL_URL}`
    : process.env.NODE_ENV === 'production'
      ? 'https://d12frosted.io'
      : 'http://localhost:3000'

export async function GET() {
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
  })

  for (const post of posts) {
    feed.addItem({
      title: post.title,
      id: `${siteUrl}${post.href}`,
      link: `${siteUrl}${post.href}`,
      description: post.description,
      author: post.authors.map((name) => ({ name })),
      date: post.published,
      category: post.tags.map((tag) => ({ name: tag })),
    })
  }

  return new Response(feed.rss2(), {
    headers: {
      'Content-Type': 'application/rss+xml; charset=utf-8',
    },
  })
}
