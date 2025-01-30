import { FormattedDate } from '@/components/blog/date'
import { BlogPost } from '@/lib/posts'
import clsx from 'clsx'

const DISPLAY_AUTHOR: boolean = false

export function FeaturedPostCard({ post, className, ...props }: React.ComponentProps<'article'> & { post: BlogPost }) {
  return (
    <article className={clsx('mx-0 w-full max-w-2xl lg:mx-0 lg:max-w-lg', className)} {...props}>
      <FormattedDate date={post.published} className="block text-sm/6 text-gray-600" />
      <h2
        id="featured-post"
        className="mt-4 text-3xl font-semibold tracking-tight text-pretty text-gray-900 sm:text-4xl"
      >
        {post.title}
      </h2>
      <p className="mt-4 text-lg/8 text-gray-600">{post.description}</p>
      <div
        className={clsx(
          'mt-4 flex flex-col justify-between gap-6 sm:mt-8 sm:gap-8 lg:mt-4 lg:flex-col',
          DISPLAY_AUTHOR ? 'sm:flex-row-reverse' : ''
        )}
      >
        <div className="flex">
          <a href={post.href} aria-describedby="featured-post" className="text-sm/6 font-semibold text-indigo-600">
            Continue reading <span aria-hidden="true">&rarr;</span>
          </a>
        </div>
        {DISPLAY_AUTHOR && (
          <div className="flex lg:border-t lg:border-gray-900/10 lg:pt-8">
            <a href="" className="flex gap-x-2.5 text-sm/6 font-semibold text-gray-900">
              <img alt="" src="/d12frosted.png" className="size-6 flex-none rounded-full bg-gray-50" />
              {post.authors[0]}
            </a>
          </div>
        )}
      </div>
    </article>
  )
}

export function RegularPostCard({ post, ...props }: React.ComponentProps<'article'> & { post: BlogPost }) {
  return (
    <article {...props}>
      <div className="group relative max-w-xl">
        <FormattedDate date={post.published} className="block text-sm/6 text-gray-600" />
        <h2 className="mt-2 text-lg font-semibold text-gray-900 group-hover:text-gray-600">
          <a href={post.href}>
            <span className="absolute inset-0" />
            {post.title}
          </a>
        </h2>
        <p className="mt-4 text-sm/6 text-gray-600">{post.description}</p>
      </div>
      {DISPLAY_AUTHOR && (
        <div className="mt-4 flex">
          <a href="" className="relative flex gap-x-2.5 text-sm/6 font-semibold text-gray-900">
            <img alt="" src="/d12frosted.png" className="size-6 flex-none rounded-full bg-gray-50" />
            {post.authors[0]}
          </a>
        </div>
      )}
    </article>
  )
}
