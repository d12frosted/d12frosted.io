import { Convive } from '@/lib/convives'
import { BlogPost } from '@/lib/posts'
import { Producer } from '@/lib/producers'
import { Rating } from '@/lib/ratings'
import { Wine } from '@/lib/wines'
import { Components } from 'react-markdown'

export type HasAllPosts = {
  allPosts: BlogPost[]
}

export type BlogPostContext = {
  post: BlogPost
} & HasAllPosts

export type ProducerContext = {
  producer: Producer
} & HasAllPosts

export type Context = BlogPostContext | Wine | Rating | Convive | ProducerContext | HasAllPosts | null | undefined

export function hasAllPosts(context: Context): context is HasAllPosts {
  return context !== null && context !== undefined && 'allPosts' in context
}

export function isBlogPostContext(context: Context): context is BlogPostContext {
  return (
    context !== null &&
    context !== undefined &&
    'post' in context &&
    'allPosts' in context
  );
}

export type CustomMarkdownProps = {
  components?: Components
  children?: string | null | undefined
  className?: string | null | undefined
  context?: Context
}

export type MarkdownComponentProps = React.DetailedHTMLProps<
  React.HTMLAttributes<HTMLElement>,
  HTMLElement
> & {
  context?: Context
}