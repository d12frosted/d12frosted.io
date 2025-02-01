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
    <div className="mt-16 space-y-20 lg:mt-20 lg:space-y-20">
      {allPosts.map((post) => (
        <RegularPostCard key={post.id} post={post} />
      ))}
    </div>
  )
}

export function LatestPosts({ allPosts, ...props }: React.ComponentProps<'div'> & { allPosts: BlogPost[] }) {
  // this is a server component, so today is time of compilation
  const today = new Date()

  const posts = allPosts.filter((post) => !post.hide && post.published <= today).slice(0, 3)

  if (posts.length === 0) {
    return (
      <div {...props}>
        <p>So empty...</p>
      </div>
    )
  }

  return (
    <div {...props}>
      <div className="mx-auto grid grid-cols-1 gap-x-8 gap-y-20 lg:mx-0 lg:max-w-none lg:grid-cols-3">
        {posts.map((post) => (
          <FeaturedPostCard key={post.id} post={post} />
        ))}
      </div>
    </div>
  )
}
