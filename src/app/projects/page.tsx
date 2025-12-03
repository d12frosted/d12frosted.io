import { fetchGitHubRepo, formatStarCount } from '@/lib/github'

// Enable ISR - page regenerates every 24 hours
export const revalidate = 86400

type Project = {
  name: string
  repo: string
  description: string
  tags: string[]
  color: string
}

const projects: Project[] = [
  {
    name: 'homebrew-emacs-plus',
    repo: 'homebrew-emacs-plus',
    description: 'GNU Emacs formulae for macOS Homebrew package manager with additional features, patches, and customizations. Used by thousands of Emacs users on macOS.',
    tags: ['Homebrew', 'Emacs', 'macOS', 'Ruby'],
    color: 'mp-blue',
  },
  {
    name: 'vulpea',
    repo: 'vulpea',
    description: 'Emacs Lisp toolkit for note-taking based on org and org-roam. Provides a collection of functions for flexible, extensible note management and knowledge graphs.',
    tags: ['Emacs Lisp', 'Org Mode', 'Knowledge Management'],
    color: 'mp-blue',
  },
  {
    name: 'vui.el',
    repo: 'vui.el',
    description: 'Declarative, component-based UI library for Emacs. React-like components with state, hooks, reconciliation, and layouts—rendered using native Emacs widgets.',
    tags: ['Emacs Lisp', 'UI Framework', 'Developer Tools'],
    color: 'mp-blue',
  },
  {
    name: 'flyspell-correct',
    repo: 'flyspell-correct',
    description: 'Distraction-free spell-checking interface for Emacs. Provides an intuitive way to correct spelling mistakes without leaving your workflow.',
    tags: ['Emacs Lisp', 'Spell Checking', 'Productivity'],
    color: 'hp-green',
  },
  {
    name: 'environment',
    repo: 'environment',
    description: 'Personal development environment configuration. Complete dotfiles setup featuring Emacs, Fish shell, macOS system settings, and development tools.',
    tags: ['Dotfiles', 'Emacs', 'Fish', 'macOS'],
    color: 'hp-green',
  },
  {
    name: 'elpa-mirror',
    repo: 'elpa-mirror',
    description: 'Create local mirrors of Emacs package archives (ELPA, MELPA). Useful for offline development, CI/CD pipelines, or air-gapped environments.',
    tags: ['Emacs Lisp', 'Package Management', 'DevOps'],
    color: 'xp-orange',
  },
  {
    name: 'vino',
    repo: 'vino',
    description: 'Wine cellar management and tasting note system for Emacs. Track your collection, record detailed tasting notes, and rate wines—all within org-mode.',
    tags: ['Emacs Lisp', 'Wine', 'Personal Database'],
    color: 'xp-orange',
  },
]

export default async function Projects() {
  // Fetch star counts from GitHub API
  const projectsWithStars = await Promise.all(
    projects.map(async (project) => {
      const repoData = await fetchGitHubRepo('d12frosted', project.repo)
      return {
        ...project,
        github: `https://github.com/d12frosted/${project.repo}`,
        stars: repoData ? formatStarCount(repoData.stargazers_count) : '—',
      }
    })
  )

  return (
    <>
      {/* Bold page header */}
      <div className="mb-16 lg:mb-24">
        <div className="h-1 w-32 bg-hp-green" />
        <h1 className="mt-8 text-5xl font-bold tracking-tight text-ink lg:text-6xl dark:text-white">
          Projects
        </h1>
        <p className="mt-6 text-xl leading-relaxed text-ink-muted dark:text-zinc-400">
          Open source tools and libraries for Emacs, package management, and personal productivity. Maintained with love and used by thousands.
        </p>
      </div>

      {/* Projects grid */}
      <div className="grid gap-8 lg:grid-cols-2 lg:gap-12">
        {projectsWithStars.map((project) => (
          <article
            key={project.name}
            className="group relative overflow-hidden bg-canvas transition-all hover:shadow-2xl dark:bg-zinc-900"
          >
            {/* Bold color accent */}
            <div className={`h-2 bg-${project.color}`} />

            <div className="p-8 lg:p-10">
              {/* Project name, stars, and link */}
              <div className="mb-4 flex items-start justify-between gap-4">
                <div>
                  <h2 className="text-2xl font-bold tracking-tight text-ink transition-colors group-hover:text-mp-blue dark:text-white dark:group-hover:text-mp-blue">
                    {project.name}
                  </h2>
                  <div className="mt-1 font-mono text-xs text-ink-muted dark:text-zinc-500">
                    ★ {project.stars}
                  </div>
                </div>
                <a
                  href={project.github}
                  target="_blank"
                  rel="noopener noreferrer"
                  className="shrink-0 font-mono text-xs uppercase tracking-wider text-ink-muted transition-colors hover:text-mp-blue dark:text-zinc-500 dark:hover:text-mp-blue"
                >
                  GitHub →
                </a>
              </div>

              {/* Description */}
              <p className="mb-6 leading-relaxed text-ink-muted dark:text-zinc-400">
                {project.description}
              </p>

              {/* Tags */}
              <div className="flex flex-wrap gap-2">
                {project.tags.map((tag) => (
                  <span
                    key={tag}
                    className="bg-paper px-3 py-1 font-mono text-xs uppercase tracking-wider text-ink-muted dark:bg-zinc-800 dark:text-zinc-500"
                  >
                    {tag}
                  </span>
                ))}
              </div>
            </div>
          </article>
        ))}
      </div>

      {/* Call to action sections */}
      <div className="mt-20 grid gap-8 lg:mt-24 lg:grid-cols-2">
        <div className="border-l-4 border-hp-green bg-paper p-8 lg:p-10 dark:bg-zinc-900/50">
          <h3 className="mb-4 text-2xl font-bold tracking-tight text-ink dark:text-white">
            More on GitHub
          </h3>
          <p className="mb-6 leading-relaxed text-ink-muted dark:text-zinc-400">
            These are the highlights. Check out my GitHub profile for more projects, contributions, and experiments.
          </p>
          <a
            href="https://github.com/d12frosted"
            target="_blank"
            rel="noopener noreferrer"
            className="inline-flex items-center gap-2 font-mono text-sm uppercase tracking-wider text-ink transition-colors hover:text-mp-blue dark:text-white dark:hover:text-mp-blue"
          >
            Visit GitHub →
          </a>
        </div>

        <div className="border-l-4 border-xp-orange bg-paper p-8 lg:p-10 dark:bg-zinc-900/50">
          <h3 className="mb-4 text-2xl font-bold tracking-tight text-ink dark:text-white">
            Barberry Garden
          </h3>
          <p className="mb-6 leading-relaxed text-ink-muted dark:text-zinc-400">
            My wine documentation project. Tasting notes, ratings, and cellar management—powered by vino and org-mode.
          </p>
          <a
            href="https://barberry.io"
            target="_blank"
            rel="noopener noreferrer"
            className="inline-flex items-center gap-2 font-mono text-sm uppercase tracking-wider text-ink transition-colors hover:text-mp-blue dark:text-white dark:hover:text-mp-blue"
          >
            Visit Barberry.io →
          </a>
        </div>
      </div>
    </>
  )
}
