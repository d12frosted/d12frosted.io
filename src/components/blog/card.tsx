import { FormattedDate } from '@/components/blog/date'
import { getAccentBgClass } from '@/lib/colors'
import { BlogPost } from '@/lib/posts'
import clsx from 'clsx'

export function RegularPostCard({ post, className, ...props }: React.ComponentProps<'article'> & { post: BlogPost }) {
  const accentColor = getAccentBgClass(post.tags)

  return (
    <article
      className={clsx(
        'group relative overflow-hidden bg-canvas transition-all hover:shadow-2xl',
        className
      )}
      {...props}
    >
      {/* Bold accent bar at top */}
      <div className={clsx('h-1', accentColor)} />

      {/* Content */}
      <div className="p-6 sm:p-8 lg:p-12">
        {/* Monospace metadata */}
        <div className="mb-4 flex items-center gap-x-4 font-mono text-xs tracking-wider text-ink-muted uppercase">
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
          <p className="mt-4 text-base leading-relaxed text-ink-muted">{post.description}</p>
        </div>
      </div>
    </article>
  )
}
