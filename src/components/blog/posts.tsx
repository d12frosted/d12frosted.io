import { FeaturedPostCard, RegularPostCard } from '@/components/blog/card'
import { BlogPost } from '@/lib/posts'

export function AllPosts({ allPosts, ...props }: React.ComponentProps<'div'> & { allPosts: BlogPost[] }) {
  if (allPosts.length === 0) {
    return (
      <div {...props}>
        <p>So empty...</p>
      </div>
    )
  }

  return (
    <div className="mx-auto grid max-w-7xl grid-cols-1 gap-x-8 gap-y-12 sm:gap-y-16 lg:grid-cols-2">
      <div className="mx-0 w-full max-w-2xl border-t border-gray-900/10 pt-12 sm:pt-16 lg:mx-0 lg:max-w-none lg:border-t-0 lg:pt-0">
        <div className="divide-y divide-gray-900/10">
          {allPosts.map((post) => (
            <RegularPostCard key={post.id} post={post} className="py-8" />
          ))}
        </div>
      </div>
    </div>
  )
}

export function LatestPosts({ allPosts, ...props }: React.ComponentProps<'div'> & { allPosts: BlogPost[] }) {
  // this is a server component, so today is time of compilation
  const today = new Date()

  const posts = allPosts.filter((post) => !post.hide && post.published <= today).slice(0, 3)
  const featuredPost = posts.shift()

  if (posts.length === 0 || !featuredPost) {
    return (
      <div {...props}>
        <p>So empty...</p>
      </div>
    )
  }

  return (
    <div {...props}>
      <div className="mx-auto grid max-w-7xl grid-cols-1 gap-x-8 gap-y-12 sm:gap-y-16 lg:grid-cols-2">
        <FeaturedPostCard post={featuredPost} />
        <div className="mx-0 w-full max-w-2xl border-t border-gray-900/10 pt-12 sm:pt-16 lg:mx-0 lg:max-w-none lg:border-t-0 lg:pt-0">
          <div className="-my-12 divide-y divide-gray-900/10">
            {posts.map((post) => (
              <RegularPostCard key={post.id} post={post} className="py-8" />
            ))}
          </div>
        </div>
      </div>
    </div>
  )
}
