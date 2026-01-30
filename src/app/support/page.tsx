import type { Metadata } from 'next'

export const metadata: Metadata = {
  title: 'Support',
  description: 'Ways to support d12frosted',
}

const linkClasses = 'underline decoration-mp-blue/30 hover:text-mp-blue hover:decoration-mp-blue/60'
const monoClasses = 'font-mono text-sm'

export default function SupportPage() {
  return (
    <div>
      {/* Geometric accent bar */}
      <div className="mb-6 h-1 w-16 bg-mp-blue" />

      <h1 className="text-4xl font-bold tracking-tighter">Support</h1>

      <div className="mt-8 space-y-8 text-ink leading-relaxed-plus">
        <p className="text-ink-muted">
          If you enjoy the writing here and want to help keep it going, there are several ways to contribute depending on
          where you are.
        </p>

        <section>
          <h2 className="font-mono text-xs font-bold uppercase tracking-wider text-ink-muted">International</h2>
          <ul className="mt-3 space-y-2">
            <li>
              <a href="https://github.com/sponsors/d12frosted" target="_blank" rel="noopener noreferrer" className={linkClasses}>
                GitHub Sponsors
              </a>
            </li>
            <li>
              <a href="https://www.patreon.com/d12frosted" target="_blank" rel="noopener noreferrer" className={linkClasses}>
                Patreon
              </a>
            </li>
          </ul>
        </section>

        <section>
          <h2 className="font-mono text-xs font-bold uppercase tracking-wider text-ink-muted">Ukraine</h2>
          <ul className="mt-3 space-y-2">
            <li>
              <a
                href="https://send.monobank.ua/jar/2Bd1NZxSmq"
                target="_blank"
                rel="noopener noreferrer"
                className={linkClasses}
              >
                Monobank jar
              </a>
            </li>
            <li>
              Card number: <span className={monoClasses}>4874 1000 2442 9345</span>
            </li>
          </ul>
        </section>

        <section>
          <h2 className="font-mono text-xs font-bold uppercase tracking-wider text-ink-muted">Moldova</h2>
          <ul className="mt-3 space-y-2">
            <li>
              <a
                href="https://mia-qr.bnm.md/1/m/BNM/AGR1cadd5f4573744e28d367c3c8a53e952"
                target="_blank"
                rel="noopener noreferrer"
                className={linkClasses}
              >
                MIA QR payment
              </a>
            </li>
            <li>
              Phone transfer: <span className={monoClasses}>+373 (69) 896645</span>
            </li>
          </ul>
        </section>
      </div>
    </div>
  )
}
