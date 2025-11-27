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

  return <RandomImage tags={post.tags} className={className} />
}

export function FeaturedPostCard({ post, className, ...props }: React.ComponentProps<'article'> & { post: BlogPost }) {
  // Bold accent color blocks - jRPG inspired
  const getAccentColor = () => {
    const tag = post.tags[0]?.toLowerCase() || ''
    if (tag.includes('emacs') || tag.includes('org')) return 'bg-mp-blue'
    if (tag.includes('haskell') || tag.includes('code')) return 'bg-hp-green'
    if (tag.includes('tutorial') || tag.includes('guide')) return 'bg-xp-orange'
    return 'bg-ink'
  }

  return (
    <article key={post.id} className={clsx("group relative overflow-hidden bg-canvas transition-all hover:shadow-2xl dark:bg-canvas-dark", className)} {...props}>
      {/* Hero image - smaller on mobile, larger on desktop */}
      <div className="relative aspect-[3/2] w-full overflow-hidden bg-paper sm:aspect-[4/3] dark:bg-paper-dark">
        <PostImage
          post={post}
          className="size-full object-contain transition-transform duration-500 group-hover:scale-105"
        />
        {/* Bold color block overlay */}
        <div className={clsx("absolute bottom-0 left-0 h-2 w-full", getAccentColor())} />
      </div>

      {/* Content area - smaller padding on mobile */}
      <div className="p-5 sm:p-8">
        {/* Monospace metadata */}
        <div className="mb-4 flex items-center gap-x-4 font-mono text-xs uppercase tracking-wider text-ink-muted dark:text-ink-muted-dark">
          <FormattedDate date={post.published} />
          <span>•</span>
          <span>{post.tags[0]}</span>
        </div>

        <div className="relative">
          <h3 className="text-2xl font-bold leading-tight text-ink transition-colors group-hover:text-mp-blue dark:text-ink-dark dark:group-hover:text-mp-blue">
            <a href={post.href}>
              <span className="absolute inset-0" />
              {post.title}
            </a>
          </h3>
          <p className="mt-4 line-clamp-2 text-base leading-relaxed text-ink-muted dark:text-ink-muted-dark">{post.description}</p>
        </div>
      </div>
    </article>
  )
}

export function RegularPostCard({ post, className, ...props }: React.ComponentProps<'article'> & { post: BlogPost }) {
  // Bold accent color blocks
  const getAccentColor = () => {
    const tag = post.tags[0]?.toLowerCase() || ''
    if (tag.includes('emacs') || tag.includes('org')) return 'bg-mp-blue'
    if (tag.includes('haskell') || tag.includes('code')) return 'bg-hp-green'
    if (tag.includes('tutorial') || tag.includes('guide')) return 'bg-xp-orange'
    return 'bg-ink dark:bg-ink-dark'
  }

  return (
    <article className={clsx('group relative overflow-hidden bg-canvas transition-all hover:shadow-2xl dark:bg-canvas-dark', className)} {...props}>
      <div className="flex flex-col lg:flex-row lg:items-stretch">
        {/* Image - smaller on mobile, larger on desktop */}
        <div className="relative aspect-[3/1] w-full overflow-hidden bg-paper sm:aspect-video lg:aspect-square lg:w-80 lg:shrink-0 dark:bg-paper-dark">
          <PostImage
            post={post}
            className="size-full object-contain transition-transform duration-500 group-hover:scale-105"
          />
          {/* Bold accent block */}
          <div className={clsx("absolute bottom-0 right-0 h-full w-2 lg:h-2 lg:w-full", getAccentColor())} />
        </div>

        {/* Content - smaller padding on mobile */}
        <div className="flex flex-1 flex-col justify-center p-5 sm:p-8 lg:p-12">
          {/* Monospace metadata */}
          <div className="mb-4 flex items-center gap-x-4 font-mono text-xs uppercase tracking-wider text-ink-muted dark:text-ink-muted-dark">
            <FormattedDate date={post.published} />
            <span>•</span>
            <span>{post.tags[0]}</span>
          </div>

          <div className="relative">
            <h3 className="text-2xl font-bold leading-tight text-ink transition-colors group-hover:text-mp-blue lg:text-3xl dark:text-ink-dark dark:group-hover:text-mp-blue">
              <a href={post.href}>
                <span className="absolute inset-0" />
                {post.title}
              </a>
            </h3>
            <p className="mt-4 text-base leading-relaxed text-ink-muted dark:text-ink-muted-dark">{post.description}</p>
          </div>
        </div>
      </div>
    </article>
  )
}
