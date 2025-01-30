import { LatestPosts } from '@/components/blog/posts'
import { CustomMarkdown } from '@/components/markdown/custom-markdown'
import { getAllPosts } from '@/lib/posts'
import { promises as fs } from 'fs'

export default async function Home() {
  const allPosts = await getAllPosts()
  const aboutText = await fs.readFile(process.cwd() + '/src/public/content/about.md', 'utf8')

  return (
    <>
      <div className="prose prose-stone">
        <h1>d12frosted</h1>
        <CustomMarkdown>{aboutText}</CustomMarkdown>
        <h2>From the blog</h2>
      </div>
      <LatestPosts allPosts={allPosts} className="bg-white py-8" />
    </>
  )
}
