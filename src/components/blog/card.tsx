import { FormattedDate } from '@/components/blog/date'
import { RandomImage } from '@/components/blog/random-image'
import { getImage } from '@/components/content/images'
import { BlogPost } from '@/lib/posts'
import clsx from 'clsx'
import Image from 'next/image'

function PostImage({
  post,
  className,
  ...props
}: Omit<React.ComponentProps<typeof Image>, 'src' | 'alt'> & {
  post: BlogPost
}) {
  function getSource() {
    if (post.image) return getImage(post.image.replace('src/public/content', ''))
    if (post.tags.includes('vulpea')) return getImage('/images/vulpea.png')
    if (post.tags.includes('vino')) return getImage('/images/vino.png')
    if (post.tags.includes('org-mode')) return getImage('/images/org-mode.png')
    if (post.tags.includes('org-roam')) return getImage('/images/org-mode.png')
    if (post.tags.includes('flyspell-correct')) return getImage('/images/flyspell-correct.png')
    if (post.tags.includes('emacs-plus')) return getImage('/images/emacs.png')
    if (post.tags.includes('emacs')) return getImage('/images/emacs.png')
    if (post.tags.includes('yabai')) return getImage('/images/yabai.png')
    if (post.tags.includes('haskell')) return getImage('/images/haskell.png')
    if (post.tags.includes('fish')) return getImage('/images/fish.png')
    return undefined
  }

  const source = getSource()
  if (source) return <Image src={source} alt="Post illustration" className={className} {...props} />

  return <RandomImage className={className} />
}

export function FeaturedPostCard({ post, ...props }: React.ComponentProps<'article'> & { post: BlogPost }) {
  return (
    <article key={post.id} className="flex flex-col items-start" {...props}>
      <div className="group relative w-full">
        <PostImage
          post={post}
          className="aspect-video w-full rounded-2xl object-contain group-hover:opacity-75 sm:aspect-[2/1] lg:aspect-[3/2]"
        />
        <div className="absolute inset-0 rounded-2xl ring-1 ring-gray-900/10 ring-inset" />
      </div>
      <div className="max-w-xl">
        <div className="mt-8 flex items-center gap-x-4 text-xs">
          <FormattedDate date={post.published} className="text-gray-500" />
          <div className="relative z-10 rounded-full bg-gray-50 px-3 py-1.5 font-medium text-gray-600 hover:bg-gray-100">
            {post.tags.join(' · ')}
          </div>
        </div>
        <div className="group relative">
          <h3 className="mt-3 text-lg/6 font-semibold text-gray-900 group-hover:text-gray-600">
            <a href={post.href}>
              <span className="absolute inset-0" />
              {post.title}
            </a>
          </h3>
          <p className="mt-5 line-clamp-24 text-sm/6 text-gray-600">{post.description}</p>
        </div>
      </div>
    </article>
  )
}

export function RegularPostCard({ post, className, ...props }: React.ComponentProps<'article'> & { post: BlogPost }) {
  return (
    <article className={clsx('group relative isolate flex flex-col gap-8 lg:flex-row', className)} {...props}>
      <div className="relative aspect-video sm:aspect-[3/1] lg:aspect-square lg:w-64 lg:shrink-0">
        <PostImage
          post={post}
          className="absolute inset-0 size-full rounded-2xl object-contain group-hover:opacity-75"
        />
        <div className="absolute inset-0 rounded-2xl ring-1 ring-gray-900/10 ring-inset" />
      </div>
      <div>
        <div className="flex items-center gap-x-4 text-xs">
          <FormattedDate date={post.published} className="text-gray-500" />
          <div className="relative z-10 rounded-full bg-gray-50 px-3 py-1.5 font-medium text-gray-600 hover:bg-gray-100">
            {post.tags.join(' · ')}
          </div>
        </div>
        <div className="group relative">
          <h3 className="mt-3 text-lg/6 font-semibold text-gray-900 group-hover:text-gray-600">
            <a href={post.href}>
              <span className="absolute inset-0" />
              {post.title}
            </a>
          </h3>
          <p className="mt-5 text-sm/6 text-gray-600">{post.description}</p>
        </div>
      </div>
    </article>
  )
}
