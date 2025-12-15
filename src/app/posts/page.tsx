import { RegularPostCard } from '@/components/blog/card'
import { getAllPosts } from '@/lib/posts'

export default async function Posts() {
  const allPosts = await getAllPosts()
  const today = new Date()
  const publishedPosts = allPosts.filter((post) => !post.hide && post.published <= today)

  // Group posts by year
  const postsByYear = publishedPosts.reduce((acc, post) => {
    const year = post.published.getFullYear()
    if (!acc[year]) {
      acc[year] = []
    }
    acc[year].push(post)
    return acc
  }, {} as Record<number, typeof publishedPosts>)

  const years = Object.keys(postsByYear)
    .map(Number)
    .sort((a, b) => b - a)

  return (
    <>
      {/* Bold page header */}
      <div className="mb-16 lg:mb-24">
        <div className="h-1 w-32 bg-mp-blue" />
        <h1 className="mt-8 text-5xl font-bold tracking-tight text-ink lg:text-6xl dark:text-white">
          All posts
        </h1>
        <p className="mt-6 font-mono text-sm uppercase tracking-wider text-ink-muted dark:text-zinc-500">
          {publishedPosts.length} posts across {years.length} years
        </p>
      </div>

      {/* Posts grouped by year */}
      <div className="space-y-16">
        {years.map((year) => (
          <section key={year}>
            {/* Year header with bold geometric element */}
            <div className="mb-8 flex items-center gap-6">
              <div className="h-1 w-12 bg-xp-orange" />
              <h2 className="text-4xl font-bold tabular-nums tracking-tight text-ink dark:text-white">{year}</h2>
              <div className="font-mono text-sm text-ink-muted dark:text-zinc-500">
                {postsByYear[year].length} {postsByYear[year].length === 1 ? 'post' : 'posts'}
              </div>
            </div>

            {/* Posts for this year - cards without images */}
            <div className="grid gap-6 lg:grid-cols-2">
              {postsByYear[year].map((post) => (
                <RegularPostCard key={post.id} post={post} />
              ))}
            </div>
          </section>
        ))}
      </div>
    </>
  )
}
