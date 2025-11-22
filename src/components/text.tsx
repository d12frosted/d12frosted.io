import clsx from 'clsx'
import { Link } from './link'

export function Text({ className, ...props }: React.ComponentPropsWithoutRef<'p'>) {
  return (
    <p
      data-slot="text"
      {...props}
      className={clsx(className, 'text-base/6 text-ink-muted sm:text-sm/6 dark:text-zinc-400')}
    />
  )
}

export function TextLink({ className, ...props }: React.ComponentPropsWithoutRef<typeof Link>) {
  return (
    <Link
      {...props}
      className={clsx(
        className,
        'text-mp-blue underline decoration-mp-blue/30 data-hover:text-ink data-hover:decoration-ink/50 dark:text-mp-blue dark:decoration-mp-blue/30 dark:data-hover:text-white dark:data-hover:decoration-white/50'
      )}
    />
  )
}

export function Strong({ className, ...props }: React.ComponentPropsWithoutRef<'strong'>) {
  return <strong {...props} className={clsx(className, 'font-medium text-ink dark:text-white')} />
}

export function Code({ className, ...props }: React.ComponentPropsWithoutRef<'code'>) {
  return (
    <code
      {...props}
      className={clsx(
        className,
        'border border-border-soft bg-code-bg px-0.5 font-mono text-sm font-medium text-ink sm:text-[0.8125rem] dark:border-zinc-700 dark:bg-zinc-800 dark:text-white'
      )}
    />
  )
}
