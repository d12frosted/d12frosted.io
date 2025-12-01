import Link from 'next/link'

export default function NotFound() {
  return (
    <div className="flex min-h-[60vh] flex-col items-center justify-center text-center">
      {/* Dramatic 404 display */}
      <div className="relative mb-8">
        {/* Decorative bars - jRPG status bar inspired */}
        <div className="absolute -left-8 top-1/2 h-1 w-6 -translate-y-1/2 bg-critical-red" />
        <div className="absolute -right-8 top-1/2 h-1 w-6 -translate-y-1/2 bg-critical-red" />

        <h1 className="font-mono text-8xl font-bold tracking-tighter text-ink sm:text-9xl">
          404
        </h1>
      </div>

      {/* Persona-inspired dramatic text */}
      <div className="mb-8 space-y-2">
        <p className="font-mono text-sm font-bold uppercase tracking-widest text-critical-red">
          Quest Failed
        </p>
        <h2 className="text-2xl font-bold tracking-tight text-ink sm:text-3xl">
          The page you seek does not exist
        </h2>
        <p className="text-ink-muted">
          Perhaps it was lost to the void, or maybe it never was.
        </p>
      </div>

      {/* Status box - jRPG menu style */}
      <div className="mb-8 border-4 border-ink bg-canvas p-6">
        <div className="mb-4 flex items-center gap-4">
          <div className="h-3 w-3 bg-critical-red" />
          <span className="font-mono text-xs uppercase tracking-widest text-ink-muted">
            Status: Page Not Found
          </span>
        </div>
        <div className="space-y-2 text-left font-mono text-sm">
          <div className="flex justify-between gap-8">
            <span className="text-ink-muted">Location</span>
            <span className="text-ink">Unknown</span>
          </div>
          <div className="flex justify-between gap-8">
            <span className="text-ink-muted">HP</span>
            <span className="text-critical-red">0 / ???</span>
          </div>
          <div className="flex justify-between gap-8">
            <span className="text-ink-muted">Recommended</span>
            <span className="text-mp-blue">Return to safety</span>
          </div>
        </div>
      </div>

      {/* Action buttons */}
      <div className="flex flex-col gap-4 sm:flex-row">
        <Link
          href="/"
          className="group flex items-center gap-2 border-4 border-ink bg-ink px-10 py-3 font-mono text-sm font-bold uppercase tracking-wider text-white transition-colors hover:bg-canvas hover:text-ink"
        >
          <span className="inline-block transition-transform group-hover:-translate-x-1">&larr;</span>
          Return Home
        </Link>
        <Link
          href="/posts"
          className="group flex items-center gap-2 border-4 border-ink bg-canvas px-10 py-3 font-mono text-sm font-bold uppercase tracking-wider text-ink transition-colors hover:bg-ink hover:text-white"
        >
          Browse Posts
          <span className="inline-block transition-transform group-hover:translate-x-1">&rarr;</span>
        </Link>
      </div>

      {/* Decorative bottom bar */}
      <div className="mt-12 flex items-center gap-2">
        <div className="h-1 w-4 bg-mp-blue" />
        <div className="h-1 w-8 bg-xp-orange" />
        <div className="h-1 w-4 bg-hp-green" />
      </div>
    </div>
  )
}
