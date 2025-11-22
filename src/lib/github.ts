export interface GitHubRepo {
  name: string
  description: string
  stargazers_count: number
  html_url: string
}

/**
 * Fetch GitHub repository data including star count
 * Uses the public GitHub API (60 requests/hour unauthenticated)
 */
export async function fetchGitHubRepo(owner: string, repo: string): Promise<GitHubRepo | null> {
  try {
    const response = await fetch(`https://api.github.com/repos/${owner}/${repo}`, {
      headers: {
        'Accept': 'application/vnd.github.v3+json',
      },
      // Cache for 1 hour to avoid hitting rate limits during dev
      next: { revalidate: 3600 },
    })

    if (!response.ok) {
      console.error(`Failed to fetch ${owner}/${repo}: ${response.status}`)
      return null
    }

    const data = await response.json()
    return data as GitHubRepo
  } catch (error) {
    console.error(`Error fetching ${owner}/${repo}:`, error)
    return null
  }
}

/**
 * Format star count for display (e.g., 2700 -> "2.7k")
 */
export function formatStarCount(count: number): string {
  if (count >= 1000) {
    return `${(count / 1000).toFixed(1)}k`
  }
  return count.toString()
}

/**
 * Fetch multiple repos in parallel
 */
export async function fetchGitHubRepos(repos: Array<{ owner: string; repo: string }>) {
  const promises = repos.map(({ owner, repo }) => fetchGitHubRepo(owner, repo))
  return Promise.all(promises)
}
