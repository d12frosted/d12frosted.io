import { WineIcon } from '@/components/icons/WineIcon'
import { Navbar, NavbarDivider, NavbarItem, NavbarLabel, NavbarSection, NavbarSpacer } from '@/components/navbar'
import { Sidebar, SidebarBody, SidebarHeader, SidebarItem, SidebarSection } from '@/components/sidebar'
import { StackedLayout } from '@/components/stacked-layout'
import { siteConfig } from '@/config/config'
import { SiGithub } from '@icons-pack/react-simple-icons'
import Image from 'next/image'
import React from 'react'

const navItems = [
  { label: 'Home', url: '/' },
  { label: 'Posts', url: '/posts' },
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
    <div className="relative isolate flex min-h-svh w-full flex-col bg-white lg:bg-zinc-100 dark:bg-zinc-900 dark:lg:bg-zinc-950">
      {/* Navbar */}
      <header className="flex items-center px-4">
        <div className="min-w-0 flex-1">
          <Navbar>
            <div className="">
              <Image
                src="/d12frosted.png"
                width={128}
                height={128}
                alt="d12frosted icon"
                className="mr-2 inline-grid size-10 shrink-0 align-middle outline outline-1 -outline-offset-1 *:col-start-1 *:row-start-1 sm:size-8"
              />
              <NavbarLabel className="max-sm:hidden">{siteConfig.name}</NavbarLabel>
            </div>
            <NavbarDivider className="" />
            <NavbarSection className="">
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
        </div>
      </header>

      {/* Content */}
      <main className="flex flex-1 flex-col pb-2 lg:px-2">
        <div className="grow p-6 lg:rounded-lg lg:bg-white lg:p-10 lg:ring-1 lg:shadow-xs lg:ring-zinc-950/5 dark:lg:bg-zinc-900 dark:lg:ring-white/10">
          <div className="mx-auto max-w-6xl">{children}</div>
        </div>
      </main>

      {/* Analytics */}
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
              className="mr-2 inline-grid size-10 shrink-0 align-middle outline outline-1 -outline-offset-1 *:col-start-1 *:row-start-1 sm:size-8"
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
                className="mr-2 inline-grid size-10 shrink-0 align-middle outline outline-1 -outline-offset-1 *:col-start-1 *:row-start-1 sm:size-8"
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
