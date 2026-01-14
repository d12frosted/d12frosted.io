import { Comments } from '@/components/blog/comments'
import { FormattedDate } from '@/components/blog/date'
import { getImage } from '@/components/content/images'
import { CustomMarkdown } from '@/components/markdown/custom-markdown'
import { siteConfig } from '@/config/config'
import { getAllPosts, getPostBySlug } from '@/lib/posts'
import { Metadata } from 'next'
import { notFound } from 'next/navigation'
import { cache } from 'react'

type PageParams = {
  slug: string
}

type Props = {
  params: Promise<PageParams>
}

const getPost = cache(async (slug: string) => {
  let post = await getPostBySlug(slug)
  if (!post) {
    notFound()
  }
  return post
})

const getAllPostsCached = cache(async () => {
  return await getAllPosts()
})

export async function generateStaticParams(): Promise<PageParams[]> {
  const posts = await getAllPosts()
  return posts.map((post) => ({
    slug: post.slug,
  }))
}

export async function generateMetadata(props: Props): Promise<Metadata> {
  const params = await props.params
  const post = await getPost(params.slug)
  const image = post.image ? getImage(post.image.replace('src/public/content', '')) : undefined

  return {
    title: post.title,
    openGraph: {
      title: post.title,
      description: post.description,
      type: 'article',
      siteName: siteConfig.name,
      images: image
        ? {
            url: image.src,
            width: image.width,
            height: image.height,
          }
        : undefined,
    },
  }
}

export default async function Post({ params }: Props) {
  const { slug } = await params
  let post = await getPost(slug)
  let date = new Date(post.published)
  let allPosts = await getAllPostsCached()
  let context = { post, allPosts }

  return (
    <article className="">
      {/* Bold post header with architectural elements */}
      <header className="mb-12 flex flex-col lg:mb-16">
        {/* Monospace metadata */}
        <div className="mb-6 flex items-center gap-x-4 font-mono text-xs tracking-wider text-ink-muted uppercase dark:text-zinc-500">
          <FormattedDate date={post.published} />
          <span>•</span>
          <span>{post.tags.join(' · ')}</span>
        </div>

        <h1 className="mb-6 text-4xl font-bold tracking-tight text-ink lg:text-5xl dark:text-white">{post.title}</h1>

        <p className="text-xl leading-relaxed text-ink-muted dark:text-zinc-400">{post.description}</p>

        {/* Bold geometric divider */}
        <div className="mt-8 h-1 w-24 bg-mp-blue" />
      </header>

      <div
        id="post-content"
        className="prose max-w-none prose-stone dark:prose-invert prose-h1:mt-12 prose-h1:mb-4 prose-h1:text-3xl prose-h1:font-bold prose-h1:tracking-tight prose-h2:mt-10 prose-h2:mb-3 prose-h2:text-2xl prose-h2:font-bold prose-h2:tracking-tight prose-p:mt-4 prose-p:leading-relaxed [&>ul]:mt-6 [&>ul]:list-['\2013\20'] [&>ul]:pl-5"
      >
        <CustomMarkdown context={context}>{post.content}</CustomMarkdown>
      </div>

      <Comments className="mt-16 lg:mt-24" />
    </article>
  )
}
