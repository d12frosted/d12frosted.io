import { CustomMarkdown } from '@/components/markdown/custom-markdown'
import { getAllPosts } from '@/lib/posts'
import { fetchGitHubRepo, formatStarCount } from '@/lib/github'
import { promises as fs } from 'fs'

// Enable ISR - page regenerates every 24 hours
export const revalidate = 86400

export default async function Home() {
  const allPosts = await getAllPosts()
  const aboutText = await fs.readFile(process.cwd() + '/src/public/content/about.md', 'utf8')

  // Get published posts for stats
  const today = new Date()
  const publishedPosts = allPosts.filter((post) => !post.hide && post.published <= today)
  const latestPosts = publishedPosts.slice(0, 3)

  // Fetch star counts for featured projects
  const [homebrewStars, vulpeaStars] = await Promise.all([
    fetchGitHubRepo('d12frosted', 'homebrew-emacs-plus'),
    fetchGitHubRepo('d12frosted', 'vulpea'),
  ])

  return (
    <>
      {/* Bold hero */}
      <h1 className="mb-12 text-5xl font-bold tracking-tight text-ink lg:mb-16 lg:text-6xl dark:text-white">
        d12frosted
      </h1>

      {/* Two-column layout */}
      <div className="grid gap-12 lg:grid-cols-[400px_1fr] lg:gap-20">
        {/* Left column: About */}
        <div className="space-y-8">
          <div>
            <div className="mb-4 h-1 w-16 bg-mp-blue" />
            <h2 className="mb-6 text-2xl font-bold tracking-tight text-ink dark:text-white">About</h2>
            <div className="prose prose-stone max-w-none text-base leading-relaxed">
              <CustomMarkdown>{aboutText}</CustomMarkdown>
            </div>
          </div>

          {/* Quick stats */}
          <div className="space-y-3 border-l-4 border-mp-blue pl-6">
            <div className="font-mono text-sm uppercase tracking-wider text-ink-muted dark:text-zinc-500">
              <div>{publishedPosts.length} posts</div>
              <div className="mt-1">Since {new Date(publishedPosts[publishedPosts.length - 1]?.published || new Date()).getFullYear()}</div>
            </div>
          </div>
        </div>

        {/* Right column: Content sections */}
        <div className="space-y-16">
          {/* Latest posts */}
          <section>
            <div className="mb-8 flex items-end justify-between">
              <div>
                <div className="mb-4 h-1 w-16 bg-xp-orange" />
                <h2 className="text-3xl font-bold tracking-tight text-ink dark:text-white">Latest</h2>
              </div>
              <a
                href="/posts"
                className="font-mono text-sm uppercase tracking-wider text-ink-muted transition-colors hover:text-mp-blue dark:text-zinc-500 dark:hover:text-mp-blue"
              >
                All posts →
              </a>
            </div>
            <div className="space-y-8">
              {latestPosts.map((post) => (
                <article key={post.id} className="group">
                  <div className="mb-3 flex items-center gap-x-3 font-mono text-xs uppercase tracking-wider text-ink-muted dark:text-zinc-500">
                    <time dateTime={post.published.toISOString()}>
                      {post.published.toLocaleDateString('en-US', { month: 'short', day: 'numeric', year: 'numeric' })}
                    </time>
                    <span>•</span>
                    <span>{post.tags[0]}</span>
                  </div>
                  <h3 className="mb-2 text-xl font-bold leading-tight text-ink transition-colors group-hover:text-mp-blue dark:text-white dark:group-hover:text-mp-blue">
                    <a href={post.href}>{post.title}</a>
                  </h3>
                  <p className="text-sm leading-relaxed text-ink-muted dark:text-zinc-400">{post.description}</p>
                </article>
              ))}
            </div>
          </section>

          {/* Projects */}
          <section>
            <div className="mb-8 flex items-end justify-between">
              <div>
                <div className="mb-4 h-1 w-16 bg-hp-green" />
                <h2 className="text-3xl font-bold tracking-tight text-ink dark:text-white">Projects</h2>
              </div>
              <a
                href="/projects"
                className="font-mono text-sm uppercase tracking-wider text-ink-muted transition-colors hover:text-mp-blue dark:text-zinc-500 dark:hover:text-mp-blue"
              >
                All projects →
              </a>
            </div>
            <div className="space-y-6">
              <a
                href="https://github.com/d12frosted/homebrew-emacs-plus"
                target="_blank"
                rel="noopener noreferrer"
                className="group block border-l-4 border-hp-green bg-paper p-6 transition-all hover:shadow-lg dark:bg-zinc-900/50"
              >
                <div className="mb-2 flex items-baseline justify-between gap-4">
                  <h3 className="text-lg font-bold text-ink transition-colors group-hover:text-mp-blue dark:text-white dark:group-hover:text-mp-blue">
                    homebrew-emacs-plus
                  </h3>
                  <span className="font-mono text-xs text-ink-muted dark:text-zinc-500">
                    ★ {homebrewStars ? formatStarCount(homebrewStars.stargazers_count) : '—'}
                  </span>
                </div>
                <p className="text-sm leading-relaxed text-ink-muted dark:text-zinc-400">
                  GNU Emacs formulae for macOS Homebrew. Used by thousands of Emacs users.
                </p>
              </a>

              <a
                href="https://github.com/d12frosted/vulpea"
                target="_blank"
                rel="noopener noreferrer"
                className="group block border-l-4 border-hp-green bg-paper p-6 transition-all hover:shadow-lg dark:bg-zinc-900/50"
              >
                <div className="mb-2 flex items-baseline justify-between gap-4">
                  <h3 className="text-lg font-bold text-ink transition-colors group-hover:text-mp-blue dark:text-white dark:group-hover:text-mp-blue">
                    vulpea
                  </h3>
                  <span className="font-mono text-xs text-ink-muted dark:text-zinc-500">
                    ★ {vulpeaStars ? formatStarCount(vulpeaStars.stargazers_count) : '—'}
                  </span>
                </div>
                <p className="text-sm leading-relaxed text-ink-muted dark:text-zinc-400">
                  Emacs toolkit for note-taking based on org and org-roam.
                </p>
              </a>

              <a
                href="https://barberry.io"
                target="_blank"
                rel="noopener noreferrer"
                className="group block border-l-4 border-xp-orange bg-paper p-6 transition-all hover:shadow-lg dark:bg-zinc-900/50"
              >
                <div className="mb-2">
                  <h3 className="text-lg font-bold text-ink transition-colors group-hover:text-mp-blue dark:text-white dark:group-hover:text-mp-blue">
                    Barberry Garden
                  </h3>
                </div>
                <p className="text-sm leading-relaxed text-ink-muted dark:text-zinc-400">
                  Wine documentation and cellar management. Powered by vino.
                </p>
              </a>
            </div>
          </section>
        </div>
      </div>
    </>
  )
}
