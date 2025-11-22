import { LatestPosts } from '@/components/blog/posts'
import { CustomMarkdown } from '@/components/markdown/custom-markdown'
import { getAllPosts } from '@/lib/posts'
import { promises as fs } from 'fs'

export default async function Home() {
  const allPosts = await getAllPosts()
  const aboutText = await fs.readFile(process.cwd() + '/src/public/content/about.md', 'utf8')

  return (
    <>
      {/* Bold hero section */}
      <div className="prose max-w-none prose-stone">
        <h1 className="mb-8 text-5xl font-bold tracking-tight text-ink lg:text-6xl dark:text-white">d12frosted</h1>
        <CustomMarkdown>{aboutText}</CustomMarkdown>
      </div>

      {/* Bold architectural divider */}
      <div className="my-16 lg:my-24">
        <div className="h-1 w-32 bg-mp-blue" />
        <h2 className="mt-8 text-3xl font-bold tracking-tight text-ink lg:text-4xl dark:text-white">From the blog</h2>
      </div>

      <LatestPosts allPosts={allPosts} />
    </>
  )
}
