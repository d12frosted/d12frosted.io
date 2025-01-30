import { FormattedDate } from '@/components/blog/date'
import { BlogPost } from '@/lib/posts'
import clsx from "clsx";

const DISPLAY_AUTHOR: boolean = false

export function LatestPosts({ allPosts }: { allPosts: BlogPost[] }) {
  // this is a server component, so today is time of compilation
  const today = new Date()

  const posts = allPosts.filter((post) => !post.hide && post.published <= today).slice(0, 3)
  const featuredPost = posts.shift()

  if (posts.length === 0 || !featuredPost) {
    return (
      <div>
        <p>So empty...</p>
      </div>
    )
  }

  return (
    <div className="bg-white py-8">
      <div className="mx-auto grid max-w-7xl grid-cols-1 gap-x-8 gap-y-12 sm:gap-y-16 lg:grid-cols-2">
        <article className="mx-0 w-full max-w-2xl lg:mx-0 lg:max-w-lg">
          <FormattedDate date={featuredPost.published} className="block text-sm/6 text-gray-600" />
          <h2
            id="featured-post"
            className="mt-4 text-3xl font-semibold tracking-tight text-pretty text-gray-900 sm:text-4xl"
          >
            {featuredPost.title}
          </h2>
          <p className="mt-4 text-lg/8 text-gray-600">{featuredPost.description}</p>
          <div className={
            clsx(
              "mt-4 flex flex-col justify-between gap-6 sm:mt-8 sm:gap-8 lg:mt-4 lg:flex-col",
              DISPLAY_AUTHOR ? "sm:flex-row-reverse" : ""
            )
          }>
            <div className="flex">
              <a
                href={featuredPost.href}
                aria-describedby="featured-post"
                className="text-sm/6 font-semibold text-indigo-600"
              >
                Continue reading <span aria-hidden="true">&rarr;</span>
              </a>
            </div>
            {DISPLAY_AUTHOR && (
              <div className="flex lg:border-t lg:border-gray-900/10 lg:pt-8">
                <a href="" className="flex gap-x-2.5 text-sm/6 font-semibold text-gray-900">
                  <img alt="" src="/d12frosted.png" className="size-6 flex-none rounded-full bg-gray-50" />
                  {featuredPost.authors[0]}
                </a>
              </div>
            )}
          </div>
        </article>
        <div className="mx-0 w-full max-w-2xl border-t border-gray-900/10 pt-12 sm:pt-16 lg:mx-0 lg:max-w-none lg:border-t-0 lg:pt-0">
          <div className="-my-12 divide-y divide-gray-900/10">
            {posts.map((post) => (
              <article key={post.id} className="py-12">
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
            ))}
          </div>
        </div>
      </div>
    </div>
  )
}
