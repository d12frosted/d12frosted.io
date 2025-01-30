import { AllPosts } from '@/components/blog/posts'
import { getAllPosts } from '@/lib/posts'

export default async function Posts() {
  const allPosts = await getAllPosts()

  return (
    <>
      <div className="prose prose-stone">
        <h1>From the blog</h1>
      </div>
      <AllPosts allPosts={allPosts} className="bg-white py-8" />
    </>
  )
}
