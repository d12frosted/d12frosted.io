import { getImage } from '@/components/content/images'
import { SyntaxHighlighter } from '@/components/markdown/syntax-highlighter'
import { BlogPost } from '@/lib/posts'
import '@/styles/blog.css'
import clsx from 'clsx'
import 'katex/dist/katex.min.css' // `rehype-katex` does not import the CSS for you
import Image from 'next/image'
import { Children, ReactNode } from 'react'
import Markdown from 'react-markdown'
import rehypeKatex from 'rehype-katex'
import rehypeRaw from 'rehype-raw'
import remarkGfm from 'remark-gfm'
import remarkMath from 'remark-math'
import YAML from 'yaml'
import { CustomMarkdownProps, isBlogPostContext } from './common'
import { Donut, DonutProps } from './d3/v0/donut'
import { PlotFigure, PlotFigureProps } from './d3/v0/plot'

const isElement = (child: React.ReactNode): child is React.ReactElement =>
  (child as React.ReactElement)?.props !== undefined

function findChildThat(
  { children }: { children?: ReactNode },
  predicate: (child: React.ReactElement) => boolean
): React.ReactElement | undefined {
  if (!children) {
    return undefined
  }
  return Children.toArray(children).find((child) => {
    if (isElement(child)) {
      return predicate(child)
    }
    return false
  }) as React.ReactElement | undefined
}

export function CustomMarkdown(props: CustomMarkdownProps): JSX.Element {
  const { context } = props
  return (
    <Markdown
      remarkPlugins={[remarkMath, remarkGfm]}
      rehypePlugins={[rehypeKatex, rehypeRaw]}
      components={{
        ...props.components,
        pre(props) {
          const { children, node, ...rest } = props
          const nonPreElement = findChildThat(
            props,
            (child) =>
              child.props.node.tagName === 'code' &&
              ['language-d3_v0_donut', 'language-d3_v0_plot', 'language-related_posts'].includes(
                child.props.className ?? ''
              )
          )
          if (nonPreElement) {
            return nonPreElement
          }
          return <pre className="bg-white p-0" {...rest}>{children}</pre>
        },

        code(props) {
          const { children, className, node, ...rest } = props

          const match = /language-(\w+)/.exec(className || '')
          if (match) {
            const lang = match[1]
            if (lang === 'd3_v0_donut') {
              const cfg: DonutProps = YAML.parse(String(children))
              return <Donut {...cfg} />
            }
            if (lang === 'd3_v0_plot') {
              const cfg: PlotFigureProps = YAML.parse(String(children))
              return <PlotFigure {...cfg} />
            }
            if (lang === 'related_posts' && isBlogPostContext(context)) {
              function describePost(post: BlogPost): string {
                if (post.description === undefined) {
                  return `- [${post.title}](${post.slug})`
                }
                return `- [${post.title}](${post.slug}) – ${post.description}`
              }

              const ids = context.post.related ?? []
              const posts = context.allPosts.filter((post) => ids.includes(post.id))
              const md = posts.map(describePost).join('\n')
              if (ids.length === 0) {
                return <></>
              }
              return (
                <div className="my-12 border-l-4 border-mp-blue bg-code-bg p-8 lg:my-16">
                  <h2 className="!mb-6 !mt-0 text-2xl font-bold tracking-tight text-ink dark:text-white">Related posts</h2>
                  <div className="prose prose-stone max-w-none [&>ul]:mt-0 [&>ul]:list-['\2013\20'] [&>ul]:pl-5 [&_a]:font-normal [&_a]:text-mp-blue [&_a]:underline [&_a]:decoration-mp-blue/30 [&_a]:underline-offset-2 [&_a]:transition-colors [&_a:hover]:text-ink [&_a:hover]:decoration-ink/50">
                    <CustomMarkdown>{md}</CustomMarkdown>
                  </div>
                </div>
              )
            }
            function t(l: string): string {
              // prism doesn't know about emacs-lisp and commonlisp
              if (l === "emacs-lisp") return "lisp"
              if (l === "commonlisp") return "lisp"
              // close enough, right?
              if (l === "fish") return "bash"
              return l
            }
            return (
              <SyntaxHighlighter {...rest} language={t(lang)}>
                {String(children).replace(/\n$/, '')}
              </SyntaxHighlighter>
            )
          }
          return (
            <code {...rest} className={className}>
              {children}
            </code>
          )
        },

        img(props) {
          const { src, alt, width, height, ref, node, ...rest } = props
          if (src && src.endsWith('.mp4')) {
            return (
              // @ts-ignore
              <video autoPlay muted playsInline preload="auto" loop {...props} >
                <source src={src} type="video/mp4" />
                Your browser does not support the video tag.
              </video>
            )
          }
          if (src && src.startsWith('/images')) {
            const classNames = (props.className ?? '').split(' ')
            return (
              <Image
                src={getImage(src)}
                alt={alt ?? 'some random image you can find on barberry garden'}
                {...rest}
                className={props.className}
              />
            )
          }
          throw new Error(`Image src must start with /images: ${src}`)
        },

        div(props) {
          const { className, ...rest } = props
          const classNames = (className ?? '').split(' ')
          if (classNames.indexOf('compare-images-block') >= 0) {
            const images = Children.toArray(props.children).filter((child) => isElement(child)) as React.ReactElement[]
            return (
              <div
                className={clsx(
                  'grid gap-x-2 gap-y-2 md:gap-y-0 lg:gap-x-4',
                  classNames.indexOf('grid-cols-1') >= 0 ? 'grid-cols-1' : 'grid-cols-1 md:grid-cols-2'
                )}
              >
                {images.map((image) => (
                  <div key={image.key} className="group relative">
                    <h3 className="mt-2 text-sm text-gray-700">
                      <span className="absolute inset-0" />
                      {image.props.alt}
                    </h3>
                    <div
                      className={clsx(
                        'w-full rounded-md border-slate-300 group-hover:opacity-75',
                        classNames.indexOf('border-0') >= 0 ? 'border-0' : 'border-4'
                      )}
                    >
                      <Image
                        className={clsx(classNames.indexOf('border-0') >= 0 ? '' : 'p-2')}
                        src={getImage(image.props.src)}
                        alt={image.props.alt}
                      />
                    </div>
                  </div>
                ))}
              </div>
            )
          }
          return <div {...props} />
        },

        section(props) {
          const { children, className, node, ...rest } = props
          const classNames = (className ?? '').split(' ')
          if (classNames.indexOf('footnotes') >= 0) {
            return (
              <section {...rest} className={className}>
                <hr className="my-8 border-gray-200" />
                {children}
              </section>
            )
          }
          return (
            <section className={className} {...rest}>
              {children}
            </section>
          )
        },

        table(props) {
          return (
            <div className="overflow-auto">
              <table {...props} />
            </div>
          )
        },
        td(props) {
          const strong = findChildThat(props, (child) => child.type === 'strong')
          if (strong) {
            return (
              <td className="bg-successful" {...props}>
                {strong.props.children}
              </td>
            )
          }

          const del = findChildThat(props, (child) => child.type === 'del')
          if (del) {
            return (
              <td className="bg-critical" {...props}>
                {del.props.children}
              </td>
            )
          }
          return <td {...props} />
        },
      }}
    >
      {props.children}
    </Markdown>
  )
}
