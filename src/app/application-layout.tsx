import { HeartIcon } from '@/components/icons/heart'
import { Navbar, NavbarDivider, NavbarItem, NavbarLabel, NavbarSection, NavbarSpacer } from '@/components/navbar'
import { Sidebar, SidebarBody, SidebarHeader, SidebarItem, SidebarSection } from '@/components/sidebar'
import { StackedLayout } from '@/components/stacked-layout'
import { ThemeToggle } from '@/components/theme-toggle'
import { siteConfig } from '@/config/config'
import { SiGithub, SiRss } from '@icons-pack/react-simple-icons'
import { Analytics } from '@vercel/analytics/react'
import Image from 'next/image'
import React from 'react'

const navItems = [
  { label: 'Home', url: '/' },
  { label: 'Posts', url: '/posts' },
  { label: 'Projects', url: '/projects' },
]

export function ApplicationLayout({
  // events,
  children,
}: {
  // events: Awaited<ReturnType<typeof getEvents>>
  children: React.ReactNode
}) {
  // let pathname = usePathname()

  return (
    <div className="relative isolate mx-auto flex min-h-svh w-full max-w-7xl flex-col bg-canvas overflow-x-hidden lg:bg-paper">
      {/* Bold navbar with stark contrast */}
      <header className="border-b-4 border-ink bg-canvas px-4 py-4 lg:px-8">
        <div className="min-w-0 flex-1">
          <Navbar>
            <div className="flex items-center">
              <Image
                src="/d12frosted.png"
                width={40}
                height={40}
                alt="d12frosted icon"
                className="mr-2 size-10 shrink-0 sm:mr-3"
              />
              <NavbarLabel className="max-sm:hidden">{siteConfig.name}</NavbarLabel>
            </div>
            <NavbarDivider className="max-sm:hidden" />
            <NavbarSection className="">
              {navItems.map(({ label, url }) => (
                <NavbarItem key={label} href={url}>
                  {label}
                </NavbarItem>
              ))}
            </NavbarSection>
            <NavbarSpacer />
            <NavbarSection>
              <ThemeToggle />
              <NavbarItem href="/feed.xml" aria-label="RSS Feed">
                <SiRss />
              </NavbarItem>
              <NavbarItem href={siteConfig.links.github} aria-label="GitHub">
                <SiGithub />
              </NavbarItem>
              <NavbarItem href="/support" aria-label="Support">
                <HeartIcon />
              </NavbarItem>
            </NavbarSection>
          </Navbar>
        </div>
      </header>

      {/* Spacious content area */}
      <main className="flex flex-1 flex-col">
        <div className="grow px-4 pt-16 pb-16 lg:px-16 lg:pt-20 lg:pb-20">
          <div className="mx-auto max-w-6xl">{children}</div>
        </div>
      </main>

      {/* Footer */}
      <footer className="border-t border-border-soft px-4 py-8 lg:px-16">
        <div className="mx-auto max-w-6xl font-mono text-xs uppercase tracking-wider text-ink-muted">
          <a
            href="/support"
            className="underline decoration-ink-muted/30 hover:text-ink hover:decoration-ink/30"
          >
            Support
          </a>
          <span className="mx-2">·</span>
          <a
            href="https://creativecommons.org/licenses/by/4.0/"
            target="_blank"
            rel="noopener noreferrer"
            className="underline decoration-ink-muted/30 hover:text-ink hover:decoration-ink/30"
          >
            CC BY 4.0
          </a>
          <span className="mx-2">·</span>
          <span>2015 - 2026 Boris Buliga</span>
        </div>
      </footer>

      {/* Analytics */}
      <Analytics />
      <script data-goatcounter="https://d12frosted.goatcounter.com/count" async src="//gc.zgo.at/count.js"></script>
    </div>
  )
}

export function ApplicationLayoutWide({
  // events,
  children,
}: {
  // events: Awaited<ReturnType<typeof getEvents>>
  children: React.ReactNode
}) {
  // let pathname = usePathname()

  return (
    <StackedLayout
      navbar={
        <Navbar>
          <div className="max-lg:hidden">
            <Image
              src="/d12frosted.png"
              width={128}
              height={128}
              alt="d12frosted icon"
              className="mr-2 inline-grid size-10 shrink-0 align-middle *:col-start-1 *:row-start-1 sm:size-8"
            />
            <NavbarLabel>{siteConfig.name}</NavbarLabel>
          </div>
          <NavbarDivider className="max-lg:hidden" />
          <NavbarSection className="max-lg:hidden">
            {navItems.map(({ label, url }) => (
              <NavbarItem key={label} href={url}>
                {label}
              </NavbarItem>
            ))}
          </NavbarSection>
          <NavbarSpacer />
          <NavbarSection>
            <ThemeToggle />
            <NavbarItem href="/feed.xml" aria-label="RSS Feed">
              <SiRss />
            </NavbarItem>
            <NavbarItem href={siteConfig.links.github} aria-label="GitHub">
              <SiGithub />
            </NavbarItem>
          </NavbarSection>
        </Navbar>
      }
      sidebar={
        <Sidebar>
          <SidebarHeader>
            <div>
              <Image
                src="/d12frosted.png"
                width={128}
                height={128}
                alt="d12frosted icon"
                className="mr-2 inline-grid size-10 shrink-0 align-middle *:col-start-1 *:row-start-1 sm:size-8"
              />
              <NavbarLabel>{siteConfig.name}</NavbarLabel>
            </div>
          </SidebarHeader>
          <SidebarBody>
            <SidebarSection>
              {navItems.map(({ label, url }) => (
                <SidebarItem key={label} href={url}>
                  {label}
                </SidebarItem>
              ))}
            </SidebarSection>
          </SidebarBody>
        </Sidebar>
      }
    >
      {children}
    </StackedLayout>
  )
}
