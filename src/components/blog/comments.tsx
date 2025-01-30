'use client'

export function Comments( props: Omit<React.ComponentProps<'section'>, 'ref' | 'style'> ) {
  return (
    <section
      {...props}
      style={{ width: '100%' }}
      ref={
        element => {
          if (!element) {
            return
          }

          const scriptElement = document.createElement('script')
          scriptElement.setAttribute('src', 'https://utteranc.es/client.js')
          scriptElement.setAttribute('repo', 'd12frosted/d12frosted.io')
          scriptElement.setAttribute('issue-term', 'pathname')
          scriptElement.setAttribute('theme', 'github-light')
          scriptElement.setAttribute('crossorigin', 'anonymous')
          scriptElement.setAttribute('async', 'true')
          element.replaceChildren(scriptElement)
        }
      }
    />
  )
}
