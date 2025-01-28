import fs from 'fs'
import { join } from 'path'

const postsDir = join('src/public/content/posts')

export interface BlogPost {
  slug: string
  id: string
  title: string
  authors: string[]
  published: Date
  updated: Date
  description?: string
  image?: string
  tags: string[]
  language: string
  hide: boolean
  pinned: boolean
  related?: string[]

  content: string
}

export async function getPostBySlug(
  slug: string,
): Promise<BlogPost | undefined> {
  const realSlug = slug.replace(/\.html$/, '').replace(/\.md$/, '')
  const fullPath = join(postsDir, `${realSlug}.md`)
  const jsonPath = join(postsDir, `${realSlug}.json`)

  if (!fs.existsSync(fullPath)) {
    return undefined
  }

  const content = await fs.promises.readFile(fullPath, 'utf8')
  const data = JSON.parse(await fs.promises.readFile(jsonPath, 'utf8'))

  return {
    slug: realSlug,
    id: data.id,
    title: data.title,
    authors: data.authors,
    published: new Date(realSlug.slice(0, 10)),
    updated: new Date(data.updated),
    tags: data.tags,
    language: data.language,
    hide: !data.publish || data.hide,
    pinned: data.pinned ?? false,
    description: data.description,
    image: data.image,
    related: data.related,
    content: content,
  }
}

export async function getAllPosts(): Promise<BlogPost[]> {
  const posts = await Promise.all(
    fs
      .readdirSync(postsDir)
      .filter((f) => f.endsWith('.md'))
      .map((slug) => getPostBySlug(slug)),
  )
  return posts.map((p) => p!).sort((post1, post2) =>
    post1.published > post2.published ? -1 : 1,
  )
}
