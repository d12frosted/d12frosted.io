import { WineIcon } from '@/components/icons/wine'
import { Navbar, NavbarDivider, NavbarItem, NavbarLabel, NavbarSection, NavbarSpacer } from '@/components/navbar'
import { Sidebar, SidebarBody, SidebarHeader, SidebarItem, SidebarSection } from '@/components/sidebar'
import { StackedLayout } from '@/components/stacked-layout'
import { ThemeToggle } from '@/components/theme-toggle'
import { siteConfig } from '@/config/config'
import { SiGithub } from '@icons-pack/react-simple-icons'
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
    <div className="relative isolate mx-auto flex min-h-svh w-full max-w-7xl flex-col overflow-x-hidden bg-canvas lg:bg-paper dark:bg-canvas-dark dark:lg:bg-paper-dark">
      {/* Bold navbar with stark contrast */}
      <header className="border-b-4 border-ink bg-canvas px-4 py-4 lg:px-8 dark:border-ink-dark dark:bg-canvas-dark">
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
              <NavbarItem href={siteConfig.links.github} aria-label="GitHub">
                <SiGithub />
              </NavbarItem>
              <NavbarItem href={siteConfig.links.barberry} aria-label="Barberry">
                <WineIcon />
              </NavbarItem>
            </NavbarSection>
          </Navbar>
        </div>
      </header>

      {/* Spacious content area */}
      <main className="flex flex-1 flex-col">
        <div className="grow px-4 py-8 lg:px-16 lg:py-20">
          <div className="mx-auto max-w-6xl">{children}</div>
        </div>
      </main>

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
            <NavbarItem href={siteConfig.links.github} aria-label="GitHub">
              <SiGithub />
            </NavbarItem>
            <NavbarItem href={siteConfig.links.barberry} aria-label="Barberry">
              <WineIcon />
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
