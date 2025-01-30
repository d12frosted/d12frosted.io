import { FormattedDate } from '@/components/blog/date'
import { getImage } from '@/components/content/images'
import { CustomMarkdown } from '@/components/markdown/custom-markdown'
import { siteConfig } from '@/config/config'
import { getAllPosts, getPostBySlug } from '@/lib/posts'
import { Metadata } from 'next'
import { notFound } from 'next/navigation'
import { cache } from 'react'
import {Comments} from "@/components/blog/comments";

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
    <article className="py-8">
      <header className="flex flex-col">
        <div className="flex items-center gap-6">
          <div className="flex flex-col">
            <h1 className="mt-2 text-4xl font-bold text-slate-900">{post.title}</h1>
            <div className="order-first flex items-center gap-4 font-mono text-sm leading-7 text-slate-500">
              <FormattedDate date={date}/>
              <p>·</p>
              <p>{post.language}</p>
              <p>·</p>
              <p>{post.authors.join(', ')}</p>
            </div>
          </div>
        </div>
        <p className="mt-3 text-lg leading-8 font-medium text-slate-700">{post.description}</p>
      </header>
      <hr className="my-8 border-gray-200"/>
      <div
        id="post-content"
        className="prose max-w-none mt-14 prose-stone prose-h1:mt-6 prose-h1:mb-2 prose-h2:mt-4 prose-h2:mb-2 prose-p:mt-2 [&>ul]:mt-6 [&>ul]:list-['\2013\20'] [&>ul]:pl-5"
      >
        <CustomMarkdown context={context}>{post.content}</CustomMarkdown>
      </div>

      <Comments className="mt-8" />
    </article>
  )
}