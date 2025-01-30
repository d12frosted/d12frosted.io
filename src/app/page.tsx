import { Heading } from '@/components/heading'
import { CustomMarkdown } from '@/components/markdown/custom-markdown'
import { getAllPosts } from '@/lib/posts'
import { promises as fs } from 'fs'

export default async function Home() {
  const allPosts = await getAllPosts()
  const aboutText = await fs.readFile(process.cwd() + '/src/public/content/about.md', 'utf8')

  return (
    <>
      <div className="prose prose-stone">
        <h1>Welcome</h1>
        <CustomMarkdown>{aboutText}</CustomMarkdown>
        <h2>Latest Blog Posts</h2>
      </div>
    </>
  )
}
