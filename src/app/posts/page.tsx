import { AllPosts } from '@/components/blog/posts'
import { getAllPosts } from '@/lib/posts'

export default async function Posts() {
  const allPosts = await getAllPosts()

  return (
    <>
      {/* Bold page header */}
      <div className="mb-16 lg:mb-24">
        <div className="h-1 w-32 bg-mp-blue" />
        <h1 className="mt-8 text-5xl font-bold tracking-tight text-ink lg:text-6xl dark:text-white">From the blog</h1>
      </div>
      <AllPosts allPosts={allPosts} />
    </>
  )
}
