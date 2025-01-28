import { Navbar, NavbarDivider, NavbarItem, NavbarLabel, NavbarSection, NavbarSpacer } from '@/components/navbar'
import { Sidebar, SidebarBody, SidebarHeader, SidebarItem, SidebarSection } from '@/components/sidebar'
import { StackedLayout } from '@/components/stacked-layout'
import Image from 'next/image'
import {SiGithub} from "@icons-pack/react-simple-icons";
import {WineIcon} from "@/components/icons/WineIcon";
import {siteConfig} from "@/app/config/config";
import React from "react";

const navItems = [
  { label: 'Home', url: '/' },
  { label: 'Posts', url: '/posts' },
]

function OpenMenuIcon() {
  return (
    <svg data-slot="icon" viewBox="0 0 20 20" aria-hidden="true">
      <path d="M2 6.75C2 6.33579 2.33579 6 2.75 6H17.25C17.6642 6 18 6.33579 18 6.75C18 7.16421 17.6642 7.5 17.25 7.5H2.75C2.33579 7.5 2 7.16421 2 6.75ZM2 13.25C2 12.8358 2.33579 12.5 2.75 12.5H17.25C17.6642 12.5 18 12.8358 18 13.25C18 13.6642 17.6642 14 17.25 14H2.75C2.33579 14 2 13.6642 2 13.25Z" />
    </svg>
  )
}

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
              <NavbarLabel className="max-sm:hidden">d12frosted</NavbarLabel>
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
            <NavbarLabel>d12frosted</NavbarLabel>
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
              <NavbarLabel>d12frosted</NavbarLabel>
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
