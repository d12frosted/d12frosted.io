import { PostsFiltered } from '@/components/blog/posts-filtered'
import { getAllPosts } from '@/lib/posts'

export default async function Posts() {
  const allPosts = await getAllPosts()
  const today = new Date()
  const publishedPosts = allPosts.filter((post) => !post.hide && post.published <= today)

  const pinnedPosts = publishedPosts.filter((post) => post.pinned)
  const regularPosts = publishedPosts.filter((post) => !post.pinned)

  const years = [...new Set(regularPosts.map((p) => p.published.getFullYear()))].sort((a, b) => b - a)

  // Serialize dates and strip content for client component
  const serialize = (posts: typeof publishedPosts) =>
    posts.map(({ content, ...post }) => ({
      ...post,
      published: post.published.toISOString(),
      updated: isNaN(post.updated.getTime()) ? post.published.toISOString() : post.updated.toISOString(),
    }))

  return (
    <>
      {/* Bold page header */}
      <div className="mb-16 lg:mb-24">
        <div className="h-1 w-32 bg-mp-blue" />
        <h1 className="mt-8 text-5xl font-bold tracking-tight text-ink lg:text-6xl dark:text-white">
          All posts
        </h1>
      </div>

      <PostsFiltered
        pinnedPosts={serialize(pinnedPosts)}
        regularPosts={serialize(regularPosts)}
        totalCount={publishedPosts.length}
        yearCount={years.length}
      />
    </>
  )
}
