import type { Metadata } from 'next'
import {
  HeartIcon,
  StarIcon,
  CreditCardIcon,
  QrCodeIcon,
  BanknotesIcon,
} from '@heroicons/react/24/outline'

export const metadata: Metadata = {
  title: 'Support',
  description: 'Ways to support d12frosted',
}

type AccentKey = 'mp-blue' | 'critical-red'

const accentClasses: Record<AccentKey, { bar: string; border: string; text: string }> = {
  'mp-blue': {
    bar: 'bg-mp-blue',
    border: 'border-mp-blue',
    text: 'text-mp-blue',
  },
  'critical-red': {
    bar: 'bg-critical-red',
    border: 'border-critical-red',
    text: 'text-critical-red',
  },
}

type Platform = {
  name: string
  href: string
  description: string
  icon: typeof StarIcon
  accent: AccentKey
}

const platforms: Platform[] = [
  {
    name: 'GitHub Sponsors',
    href: 'https://github.com/sponsors/d12frosted',
    description: 'Recurring or one-time sponsorship through GitHub. Best if you already use GitHub.',
    icon: StarIcon,
    accent: 'mp-blue',
  },
  {
    name: 'Patreon',
    href: 'https://www.patreon.com/d12frosted',
    description: 'Monthly patronage with a familiar checkout and card support.',
    icon: CreditCardIcon,
    accent: 'critical-red',
  },
]

export default function SupportPage() {
  return (
    <>
      {/* Page header */}
      <div className="mb-16 lg:mb-24">
        <div className="h-1 w-32 bg-xp-orange" />
        <h1 className="mt-8 text-5xl font-bold tracking-tight text-ink lg:text-6xl dark:text-white">
          Support
        </h1>
        <p className="mt-6 max-w-2xl text-xl leading-relaxed text-ink-muted dark:text-zinc-400">
          If you enjoy the writing, the Emacs packages, or any of the other things I put out here, there are a few ways
          to chip in. Pick whichever is easiest where you are — every bit helps keep the lights on.
        </p>
      </div>

      {/* Platforms */}
      <section className="mb-20 lg:mb-24">
        <div className="mb-8">
          <div className="mb-4 h-1 w-16 bg-mp-blue" />
          <h2 className="text-3xl font-bold tracking-tight text-ink dark:text-white">Platforms</h2>
          <p className="mt-3 font-mono text-xs uppercase tracking-wider text-ink-muted dark:text-zinc-500">
            International · Recurring or one-time
          </p>
        </div>

        <div className="grid gap-8 lg:grid-cols-2 lg:gap-12">
          {platforms.map((platform) => {
            const Icon = platform.icon
            const classes = accentClasses[platform.accent]
            return (
              <a
                key={platform.name}
                href={platform.href}
                target="_blank"
                rel="noopener noreferrer"
                className="group relative block overflow-hidden bg-canvas transition-all hover:shadow-2xl dark:bg-zinc-900"
              >
                <div className={`h-2 ${classes.bar}`} />

                <div className="p-8 lg:p-10">
                  <div className="mb-6 flex items-start justify-between gap-4">
                    <div
                      className={`flex h-12 w-12 items-center justify-center border-2 ${classes.border} ${classes.text}`}
                    >
                      <Icon className="h-6 w-6" strokeWidth={2} />
                    </div>
                    <span className="shrink-0 font-mono text-xs uppercase tracking-wider text-ink-muted transition-colors group-hover:text-mp-blue dark:text-zinc-500">
                      Visit →
                    </span>
                  </div>

                  <h3 className="text-2xl font-bold tracking-tight text-ink transition-colors group-hover:text-mp-blue dark:text-white dark:group-hover:text-mp-blue">
                    {platform.name}
                  </h3>
                  <p className="mt-3 leading-relaxed text-ink-muted dark:text-zinc-400">
                    {platform.description}
                  </p>
                </div>
              </a>
            )
          })}
        </div>
      </section>

      {/* Direct transfers */}
      <section className="mb-20 lg:mb-24">
        <div className="mb-8">
          <div className="mb-4 h-1 w-16 bg-hp-green" />
          <h2 className="text-3xl font-bold tracking-tight text-ink dark:text-white">Direct transfers</h2>
          <p className="mt-3 font-mono text-xs uppercase tracking-wider text-ink-muted dark:text-zinc-500">
            Local · No platform fees
          </p>
        </div>

        <div className="grid gap-8 lg:grid-cols-2 lg:gap-12">
          {/* Ukraine */}
          <div className="border-l-4 border-xp-orange bg-paper p-8 lg:p-10 dark:bg-zinc-900/50">
            <div className="mb-6 flex items-center gap-4">
              <div className="flex h-12 w-12 shrink-0 items-center justify-center border-2 border-xp-orange text-xp-orange">
                <BanknotesIcon className="h-6 w-6" strokeWidth={2} />
              </div>
              <div>
                <h3 className="text-2xl font-bold tracking-tight text-ink dark:text-white">Ukraine</h3>
                <p className="font-mono text-xs uppercase tracking-wider text-ink-muted dark:text-zinc-500">
                  UAH · Monobank
                </p>
              </div>
            </div>

            <div className="space-y-4">
              <a
                href="https://send.monobank.ua/jar/2Bd1NZxSmq"
                target="_blank"
                rel="noopener noreferrer"
                className="group flex items-center justify-between border border-border-soft bg-canvas p-4 transition-all hover:border-xp-orange dark:border-zinc-800 dark:bg-zinc-900"
              >
                <span className="font-bold text-ink dark:text-white">Monobank jar</span>
                <span className="font-mono text-xs uppercase tracking-wider text-ink-muted transition-colors group-hover:text-xp-orange dark:text-zinc-500">
                  Open →
                </span>
              </a>

              <div className="border border-border-soft bg-canvas p-4 dark:border-zinc-800 dark:bg-zinc-900">
                <div className="font-mono text-xs uppercase tracking-wider text-ink-muted dark:text-zinc-500">
                  Card number
                </div>
                <div className="mt-2 font-mono text-base tracking-wider text-ink tabular-nums dark:text-white">
                  4874 1000 2442 9345
                </div>
              </div>
            </div>
          </div>

          {/* Moldova */}
          <div className="border-l-4 border-hp-green bg-paper p-8 lg:p-10 dark:bg-zinc-900/50">
            <div className="mb-6 flex items-center gap-4">
              <div className="flex h-12 w-12 shrink-0 items-center justify-center border-2 border-hp-green text-hp-green">
                <QrCodeIcon className="h-6 w-6" strokeWidth={2} />
              </div>
              <div>
                <h3 className="text-2xl font-bold tracking-tight text-ink dark:text-white">Moldova</h3>
                <p className="font-mono text-xs uppercase tracking-wider text-ink-muted dark:text-zinc-500">
                  MDL · MIA / Phone
                </p>
              </div>
            </div>

            <div className="space-y-4">
              <a
                href="https://mia-qr.bnm.md/1/m/BNM/AGR1cadd5f4573744e28d367c3c8a53e952"
                target="_blank"
                rel="noopener noreferrer"
                className="group flex items-center justify-between border border-border-soft bg-canvas p-4 transition-all hover:border-hp-green dark:border-zinc-800 dark:bg-zinc-900"
              >
                <span className="font-bold text-ink dark:text-white">MIA QR payment</span>
                <span className="font-mono text-xs uppercase tracking-wider text-ink-muted transition-colors group-hover:text-hp-green dark:text-zinc-500">
                  Open →
                </span>
              </a>

              <div className="border border-border-soft bg-canvas p-4 dark:border-zinc-800 dark:bg-zinc-900">
                <div className="font-mono text-xs uppercase tracking-wider text-ink-muted dark:text-zinc-500">
                  Phone transfer
                </div>
                <div className="mt-2 font-mono text-base tracking-wider text-ink tabular-nums dark:text-white">
                  +373 (69) 896645
                </div>
              </div>
            </div>
          </div>
        </div>
      </section>

      {/* Thank you callout */}
      <div className="border-l-4 border-mp-blue bg-paper p-8 lg:p-10 dark:bg-zinc-900/50">
        <div className="flex items-start gap-6">
          <div className="flex h-12 w-12 shrink-0 items-center justify-center border-2 border-mp-blue text-mp-blue">
            <HeartIcon className="h-6 w-6" strokeWidth={2} />
          </div>
          <div>
            <h3 className="text-2xl font-bold tracking-tight text-ink dark:text-white">Thank you</h3>
            <p className="mt-3 leading-relaxed text-ink-muted dark:text-zinc-400">
              Writing, maintaining open source, and running this site is something I do in the margins of the day. Any
              support — a star, a share, a sponsorship, a kind message — genuinely helps me keep doing it.
            </p>
          </div>
        </div>
      </div>
    </>
  )
}
