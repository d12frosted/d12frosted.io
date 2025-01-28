import { BlogPost } from '@/lib/posts'
import { Components } from 'react-markdown'

export type HasAllPosts = {
  allPosts: BlogPost[]
}

export type BlogPostContext = {
  post: BlogPost
} & HasAllPosts

export type Context = BlogPostContext | HasAllPosts | null | undefined

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