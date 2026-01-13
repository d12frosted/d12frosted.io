import { generateFeed } from '@/lib/feed'

export const revalidate = 86400 // 24 hours

export async function GET() {
  const feed = await generateFeed()

  return new Response(feed.atom1(), {
    headers: {
      'Content-Type': 'application/atom+xml; charset=utf-8',
    },
  })
}
