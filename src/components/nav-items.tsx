'use client'

import { usePathname } from 'next/navigation'
import { NavbarItem } from './navbar'

export function NavItems({ items }: { items: { label: string; url: string }[] }) {
  const pathname = usePathname()

  return (
    <>
      {items.map(({ label, url }) => {
        const current = url === '/' ? pathname === '/' : pathname.startsWith(url)
        return (
          <NavbarItem key={label} href={url} current={current}>
            {label}
          </NavbarItem>
        )
      })}
    </>
  )
}
