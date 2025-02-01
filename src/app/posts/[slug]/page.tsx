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
    <article className="">
      <header className="flex flex-col">
        <div className="flex items-center gap-x-4 text-xs">
          <div className="flex flex-col">
            <div className="flex items-center gap-x-4 text-xs">
              <FormattedDate date={post.published} className="text-gray-500" />
              <div className="relative z-10 rounded-full bg-gray-50 px-3 py-1.5 font-medium text-gray-600 hover:bg-gray-100">
                {post.tags.join(' · ')}
              </div>
            </div>
            <h1 className="mt-2 text-4xl font-bold text-slate-900">{post.title}</h1>
          </div>
        </div>
        <p className="mt-5 line-clamp-24 text-sm/6 text-gray-600">{post.description}</p>
      </header>
      <hr className="my-8 border-gray-200" />
      <div
        id="post-content"
        className="prose max-w-none prose-stone prose-h1:mt-6 prose-h1:mb-2 prose-h2:mt-4 prose-h2:mb-2 prose-p:mt-2 [&>ul]:mt-6 [&>ul]:list-['\2013\20'] [&>ul]:pl-5"
      >
        <CustomMarkdown context={context}>{post.content}</CustomMarkdown>
      </div>

      <Comments className="mt-8" />
    </article>
  )
}