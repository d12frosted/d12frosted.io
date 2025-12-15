import { FormattedDate } from '@/components/blog/date'
import { getAccentColorFromTags } from '@/lib/colors'
import { BlogPost } from '@/lib/posts'
import clsx from 'clsx'

export function FeaturedPostCard({ post, className, ...props }: React.ComponentProps<'article'> & { post: BlogPost }) {
  const accentColor = `bg-${getAccentColorFromTags(post.tags)}`

  return (
    <article
      key={post.id}
      className={clsx(
        'group relative overflow-hidden bg-canvas transition-all hover:shadow-2xl dark:bg-zinc-900',
        className
      )}
      {...props}
    >
      {/* Bold accent bar at top */}
      <div className={clsx('h-1', accentColor)} />

      {/* Spacious content area */}
      <div className="p-6 sm:p-8">
        {/* Monospace metadata */}
        <div className="mb-4 flex items-center gap-x-4 font-mono text-xs tracking-wider text-ink-muted uppercase dark:text-zinc-500">
          <FormattedDate date={post.published} />
          <span>•</span>
          <span>{post.tags[0]}</span>
        </div>

        <div className="relative">
          <h3 className="text-2xl leading-tight font-bold text-ink transition-colors group-hover:text-mp-blue dark:text-white dark:group-hover:text-mp-blue">
            <a href={post.href}>
              <span className="absolute inset-0" />
              {post.title}
            </a>
          </h3>
          <p className="mt-4 line-clamp-2 text-base leading-relaxed text-ink-muted dark:text-zinc-400">
            {post.description}
          </p>
        </div>
      </div>
    </article>
  )
}

export function RegularPostCard({ post, className, ...props }: React.ComponentProps<'article'> & { post: BlogPost }) {
  const accentColor = `bg-${getAccentColorFromTags(post.tags)}`

  return (
    <article
      className={clsx(
        'group relative overflow-hidden bg-canvas transition-all hover:shadow-2xl dark:bg-zinc-900',
        className
      )}
      {...props}
    >
      {/* Bold accent bar at top */}
      <div className={clsx('h-1', accentColor)} />

      {/* Content */}
      <div className="p-6 sm:p-8 lg:p-12">
        {/* Monospace metadata */}
        <div className="mb-4 flex items-center gap-x-4 font-mono text-xs tracking-wider text-ink-muted uppercase dark:text-zinc-500">
          <FormattedDate date={post.published} />
          <span>•</span>
          <span>{post.tags[0]}</span>
        </div>

        <div className="relative">
          <h3 className="text-2xl leading-tight font-bold text-ink transition-colors group-hover:text-mp-blue lg:text-3xl dark:text-white dark:group-hover:text-mp-blue">
            <a href={post.href}>
              <span className="absolute inset-0" />
              {post.title}
            </a>
          </h3>
          <p className="mt-4 text-base leading-relaxed text-ink-muted dark:text-zinc-400">{post.description}</p>
        </div>
      </div>
    </article>
  )
}
