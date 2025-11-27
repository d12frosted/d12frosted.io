Let me be clear about my motivations. I did not build this script to celebrate productivity or to gamify my work with green squares. I built it to establish correlations between commit frequency and periods of questionable mental stability.

The hypothesis: there exists a relationship between "days where I made 47 commits" and "days where I forgot to eat and argued with a compiler until 3am." The script provides data. I provide the interpretation. You provide the concern.

But also, while building this tool for self-incrimination, I discovered something genuinely frustrating about GitHub.

# GitHub Lies to You

Your contribution graph - that comforting grid of green squares - is incomplete. It only shows commits that land on default branches. If you spend two weeks building a feature on a branch, making thoughtful atomic commits, responding to review feedback, refactoring twice… none of that appears until the PR merges.

For those of us who work on long-running branches, or contribute to repositories where PRs sit in review for weeks, the contribution graph becomes fiction. A flat week might actually be your most productive one. The graph shows *merged* work, not *done* work.

I wanted to see the real picture. Even if - especially if - that picture suggests I should take a walk outside.

# Three APIs Walk Into a Repository

Here's what I learned while building this: getting complete commit data from GitHub requires three different approaches. This surprised me, and it might be useful if you're building GitHub tooling.

## The Search API (What You'd Try First)

The obvious approach - use GitHub's commit search:

``` bash
gh api search/commits \
  -X GET \
  --raw-field "q=author:USERNAME committer-date:2024-01-01..2024-01-07" \
  --raw-field "per_page=100"
```

This searches GitHub's commit index. The response includes repository info, commit message, SHA, and URLs:

``` json
{
  "items": [
    {
      "sha": "abc123...",
      "commit": {
        "message": "Fix the thing that was broken",
        "committer": { "date": "2024-01-05T14:32:00Z" }
      },
      "html_url": "https://github.com/owner/repo/commit/abc123",
      "repository": { "full_name": "owner/repo" }
    }
  ]
}
```

The problem: this only includes commits that GitHub has indexed, which generally means commits reachable from default branches. Your work on `feature/rewrite-everything-again` doesn't appear here until it's merged into `main`.

There's also pagination to handle. The search API returns max 100 items per page, and you'll need to loop:

``` bash
page=1
while true; do
  page_data=$(gh api search/commits \
    -X GET \
    --raw-field "q=author:${USER} committer-date:${DATE_FROM}..${DATE_TO}" \
    --raw-field "per_page=100" \
    --raw-field "page=$page")

  item_count=$(echo "$page_data" | jq '.items | length')

  # Process items...

  # Stop if we got less than a full page, or hit page limit
  if [[ "$item_count" -lt 100 ]] || [[ "$page" -ge 10 ]]; then
    break
  fi
  ((page++))
done
```

## GraphQL for Branch Discovery

To find commits on PR branches, you first need to know which branches exist. The REST API doesn't give you an easy way to find "all branches where I have open PRs." You could list all branches for all repositories you contribute to, but that's expensive and wasteful.

GraphQL solves this elegantly. One query returns every open PR you've authored, with head branch information:

``` graphql
query {
  search(query: "author:USERNAME is:pr is:open", type: ISSUE, first: 100) {
    nodes {
      ... on PullRequest {
        headRefName
        headRepository {
          nameWithOwner
        }
      }
    }
  }
}
```

With `gh` CLI:

``` bash
gh api graphql -f query='
query {
  search(query: "author:USERNAME is:pr is:open", type: ISSUE, first: 100) {
    nodes {
      ... on PullRequest {
        headRefName
        headRepository {
          nameWithOwner
        }
      }
    }
  }
}'
```

The response gives you repository and branch pairs:

``` json
{
  "data": {
    "search": {
      "nodes": [
        {
          "headRefName": "feature/new-thing",
          "headRepository": { "nameWithOwner": "owner/repo" }
        },
        {
          "headRefName": "fix/bug-123",
          "headRepository": { "nameWithOwner": "other/project" }
        }
      ]
    }
  }
}
```

Extract these into `owner/repo:branch` pairs:

``` bash
pr_branches=$(echo "$response" | jq -r '
  .data.search.nodes[] |
  select(.headRepository != null and .headRefName != null) |
  "\(.headRepository.nameWithOwner):\(.headRefName)"
' | sort -u)
```

Now you know exactly where to look.

## REST API for Specific Branches

With branch names in hand, fetch commits from each one using the repository commits endpoint:

``` bash
gh api "repos/OWNER/REPO/commits?sha=BRANCH&author=USERNAME&since=2024-01-01T00:00:00Z&until=2024-01-07T23:59:59Z&per_page=100"
```

Key parameters:

- `sha` - despite the name, this accepts branch names, not just commit SHAs
- `author` - filter by GitHub username
- `since` / `until` - ISO 8601 timestamps (note: these filter by committer date, not author date)
- `per_page` - up to 100

This gives you commits that won't appear in search until (if ever) the branch merges.

## Deduplication

A commit might appear in multiple sources. If your PR branch is based on `main`, some commits from `main` might show up when you query the branch. Commits might also appear in both search results and branch queries if the branch was recently merged.

The solution: collect everything, then deduplicate by SHA:

``` bash
# Collect from all sources into a temp file (one JSON object per line)
echo "$search_results" | jq -c '.items[]' >> "$temp_file"
echo "$branch_commits" | jq -c '.[]' >> "$temp_file"

# Dedupe by SHA, keep first occurrence, sort by date
cat "$temp_file" | jq -s '
  group_by(.sha) |
  map(.[0]) |
  sort_by(.date) |
  reverse
'
```

The `jq -s` (slurp) flag is essential - it reads all JSON objects into an array so you can group and sort them.

## The Full Picture

The complete strategy:

1.  Search API for indexed commits (default branches, merged work)
2.  GraphQL to discover your open PR branches
3.  REST API to fetch commits from each discovered branch
4.  Optionally: REST API for manually specified branches (forks, branches without PRs)
5.  Deduplicate by SHA, sort by date

It's more machinery than you'd expect for answering "what did I actually do this week," but now you have the complete picture.

# The Script

Yes, this is a bash script. Yes, bash is where you go when you want to do arithmetic, statistical analysis, and complex JSON manipulation. This is well known.

In my defence, it started as five lines to fetch commits. Then it needed date ranges. Then caching. Then analytics. At some point I looked up and realised I had nearly 1000 lines of bash doing median calculations, and the sunk cost fallacy had won.

I could rewrite it in Python, but Python and I have an understanding: we leave each other alone. Node.js? The Fremen called the sandworm Shai-Hulud - "Old Man of the Desert" - and learned to ride it, but I lack their courage. So bash it remains, a monument to stubbornness and `jq`.

I packaged this into a script called `gh-contributions`. Basic usage:

``` bash
gh-contributions today
gh-contributions last7
gh-contributions month
```

The default output looks like this:

``` example
═══════════════════════════════════════════════════════════════
  GitHub Contributions for d12frosted
═══════════════════════════════════════════════════════════════
Period: Today (2025-11-27)

── Pull Requests ──
  [merged] fix: add server signing to EmployeeLookupService for RPC calls
           private-org/private-repo  -  https://github.com/private-org/private-repo/pull/12345

  Total: 1 PRs

── Issues Opened ──
  (none)

── Commits ──

  ┌─ d12frosted/d12frosted.io (1 commits)
  │  cc66db8 hide heading level indicators on mobile

  ┌─ private-org/private-repo (1 commits)
  │  c94b032 fix: an issue that was bothering me in the morning (#12345)

  Total: 2 commits

═══════════════════════════════════════════════════════════════
  Summary: 1 PRs, 0 issues, 2 commits
═══════════════════════════════════════════════════════════════

── Analytics ──
  vs yesterday:      PRs -8, Commits -75
  vs Thu median:     PRs +1 (med 0), Commits -1 (med 3)  [n=52]
  vs 30-day median:  PRs -1 (med 2), Commits -3 (med 5)  [n=30]
```

Notice the analytics section. Yesterday I made 77 commits and 9 PRs. Today, 2 commits. The median comparison tells me today is actually normal - slightly below my typical Thursday, but nothing alarming. The "vs yesterday" line, on the other hand, tells me yesterday was… not normal. The script provides data. I provide the interpretation.

For work on branches that aren't associated with open PRs (maybe you're working on a feature branch that doesn't have a PR yet, or contributing to a fork), you can specify extra branches to check:

``` bash
gh-contributions -e d12frosted/vulpea:v2 -e myorg/project:feature last7
```

Commits from non-default branches show a marker in the output - `← pr:feature-branch` or `← extra:v2` - so you can see which commits would have been invisible without those sources.

## Caching and Analytics

Because I wanted historical comparisons (and because I don't want to hit GitHub's rate limits repeatedly), the script caches daily data in `~/.cache/gh-contributions/`. Each day gets a JSON file with PRs, issues, and commits.

The cache structure is simple - one file per user per day:

``` example
~/.cache/gh-contributions/
├── d12frosted-2024-01-01.json
├── d12frosted-2024-01-02.json
├── d12frosted-2024-01-03.json
└── ...
```

Each file contains the full day's data:

``` json
{
  "date": "2024-01-15",
  "user": "d12frosted",
  "prs_count": 2,
  "issues_count": 0,
  "commits_count": 14,
  "prs": [...],
  "issues": [...],
  "commits": [...]
}
```

The caching logic:

- **Today**: always fetch fresh (the day isn't complete yet)
- **Past days**: use cache if exists, otherwise fetch and cache
- **Force mode** (`-F`): bypass cache, fetch fresh

``` bash
get_day_data() {
  local target_date="$1"
  local today=$(date -u +%Y-%m-%d)

  if [[ "$target_date" == "$today" ]] || [[ "$FORCE_FETCH" == "true" ]]; then
    # Always fetch fresh for today or when forced
    data=$(fetch_day_data "$target_date")
    write_cache "$target_date" "$data"
    echo "$data"
  elif cache_exists "$target_date"; then
    read_cache "$target_date"
  else
    data=$(fetch_day_data "$target_date")
    write_cache "$target_date" "$data"
    echo "$data"
  fi
}
```

You can backfill historical data:

``` bash
gh-contributions --backfill 365
```

This iterates through the last 365 days, fetching and caching each one. It takes a while (rate limits, politeness delays), but afterwards you have a year of data locally.

With historical data cached, the script calculates comparisons. But here's where I learned something about statistics that I probably should have known already.

## Why Mean is Lazy (and Often Wrong)

My first version used arithmetic mean - sum divided by count. Simple, obvious, wrong.

After backfilling a year of data, I ran the numbers:

``` example
Mean commits/day:   5.2
Median commits/day: 2
Zero-activity days: 23%
Outlier days (>30): 9 days (max: 79)
```

The mean (5.2) is 2.6× the median (2). Why? Because of those nine days where I apparently lost contact with reality and made 30-79 commits. Those outliers drag the mean up so much that a perfectly reasonable 4-commit day looks "below average" - when it's actually twice my typical output. I am a senior manager, after all, I commit while others don't look. Sneaking in code like it's contraband

Mean tells you "your average day." Median tells you "what does a normal day actually look like." For data with outliers - which is most real-world data - median is almost always more useful.

## Same-Weekday Matters

The data also revealed something obvious in hindsight:

``` example
Day     Median  Mean
Mon       2     7.1
Tue       3     5.5
Wed       3     7.0
Thu       3     5.0
Fri       3     5.8
Sat       1     3.4
Sun       1     2.9
```

Weekday median (2-3) is roughly 2-3× weekend median (1). Comparing a Sunday to the overall average is unfair - I'm not slacking, I'm just not working on Sunday. Same-weekday comparison is apples to apples.

The updated analytics output:

``` example
── Analytics ──
  vs yesterday:      PRs +0, Commits +5
  vs Thu median:     PRs +1 (med 0), Commits +3 (med 3)  [n=52]
  vs 30-day median:  PRs +0 (med 1), Commits +4 (med 2)  [n=30]
```

This tells you:

- Raw delta from yesterday
- How today compares to the same day of week (all Thursdays in your history)
- How today compares to your recent baseline

The `n=` shows sample size. After a year, you have ~52 data points for each weekday, which is enough to trust the median.

None of this requires a statistics degree. It just requires not being lazy about averaging.

## Org-Mode Output

Because I live in Emacs, there's an org-mode output format:

``` bash
gh-contributions --format org today >> ~/org/contributions.org
```

This produces proper org structure with headings, states, and property drawers:

``` example
* GitHub Contributions for d12frosted
** Pull Requests
*** DONE Add caching layer to API client
:PROPERTIES:
:REPO: myorg/project
:URL: https://github.com/myorg/project/pull/142
:END:
*** TODO Fix race condition in worker pool
:PROPERTIES:
:REPO: myorg/project
:URL: https://github.com/myorg/project/pull/145
:END:
** Commits
*** myorg/project (8 commits)
:PROPERTIES:
:URL: https://github.com/myorg/project
:END:
**** a1b2c3d Implement retry logic with exponential backoff
:PROPERTIES:
:URL: https://github.com/myorg/project/commit/a1b2c3d
:END:
```

PR and issue states map to org TODO states: `OPEN` → `TODO`, `MERGED=/=CLOSED` → `DONE`.

Useful if you want to integrate contribution data into your existing org workflow - weekly reviews, time tracking, or just keeping a log of what you shipped.

# Practical Patterns

A few implementation details that might save you time if you're building similar tooling.

## BSD vs GNU Date

Date arithmetic in bash is a nightmare because macOS ships BSD date and Linux ships GNU date. They have completely different syntax for relative dates:

``` bash
# GNU date: human-readable relative strings
date -d "7 days ago" +%Y-%m-%d
date -d "yesterday" +%Y-%m-%d
date -d "last monday" +%Y-%m-%d

# BSD date: cryptic -v flags
date -v-7d +%Y-%m-%d
date -v-1d +%Y-%m-%d
# "last monday" requires calculating days since monday manually
```

This problem is so common that there's a Homebrew package (`coreutils`) whose entire purpose is to bring GNU utilities to macOS. It installs everything with a `g` prefix - `gdate`, `gsed`, `gawk` - so you can pretend you're on a sensible system. I didn't use it here because I wanted the script to work without extra dependencies, but I thought about it. Often.

The script detects which you have by checking if `date --version` succeeds (GNU) or fails (BSD):

``` bash
if date --version >/dev/null 2>&1; then
  # GNU date
  DATE_FROM=$(date -u -d "7 days ago" +%Y-%m-%d)
else
  # BSD date (macOS)
  DATE_FROM=$(date -u -v-7d +%Y-%m-%d)
fi
```

For "start of current week" (Monday), BSD requires manual calculation:

``` bash
if date --version >/dev/null 2>&1; then
  DATE_FROM=$(date -u -d "last monday" +%Y-%m-%d)
else
  days_since_monday=$(( $(date +%u) - 1 ))
  DATE_FROM=$(date -u -v-${days_since_monday}d +%Y-%m-%d)
fi
```

The `-u` flag is important throughout - it forces UTC, avoiding timezone-related surprises.

## Date Iteration

Iterating through a date range in bash is surprisingly annoying. You need to increment a date by one day repeatedly, which again differs between BSD and GNU:

``` bash
date_range() {
  local start="$1"
  local end="$2"
  local current="$start"

  while [[ "$current" < "$end" ]] || [[ "$current" == "$end" ]]; do
    echo "$current"
    if date --version >/dev/null 2>&1; then
      current=$(date -u -d "$current + 1 day" +%Y-%m-%d)
    else
      current=$(date -u -j -f "%Y-%m-%d" -v+1d "$current" +%Y-%m-%d)
    fi
  done
}
```

The BSD version uses `-j` (don't set system date) with `-f` (input format) to parse the current date string, then `-v+1d` to add a day.

## Rate Limit Handling

GitHub's API has rate limits: 5,000 requests per hour for authenticated users. If you're backfilling a year of data, you'll hit them. Each day requires multiple API calls (search, GraphQL, potentially several branch queries), so 365 days can easily burn through your quota.

The script checks remaining quota periodically and sleeps until reset if needed:

``` bash
check_rate_limit() {
  local rate_info
  rate_info=$(gh api rate_limit)

  local remaining reset_at
  remaining=$(echo "$rate_info" | jq -r '.resources.core.remaining')
  reset_at=$(echo "$rate_info" | jq -r '.resources.core.reset')

  if [[ "$remaining" -lt 50 ]]; then
    local now reset_in
    now=$(date +%s)
    reset_in=$((reset_at - now))

    if [[ "$reset_in" -gt 0 ]]; then
      printf "Rate limit low (%d remaining). Waiting %d seconds..." "$remaining" "$reset_in"
      sleep "$reset_in"
      echo " resumed."
    fi
  fi
}
```

The script also adds small delays between requests during backfill (a `sleep 1` after each day) to be polite to the API.

## jq Patterns

The script leans heavily on jq for JSON manipulation. A few patterns worth highlighting:

**Extracting and transforming fields:**

``` bash
echo "$response" | jq -r '.items[] | {
  sha: .sha,
  short_sha: .sha[0:7],
  message: (.commit.message | split("\n")[0]),
  url: .html_url,
  repo: .repository.full_name,
  date: .commit.committer.date
}'
```

The `.sha[0:7]` gives you a short SHA. The `split("\n")[0]` extracts only the first line of multi-line commit messages.

**Slurp mode for aggregation:**

``` bash
# Without -s: processes each JSON object independently
# With -s: reads all objects into an array first

cat "$temp_file" | jq -s '
  group_by(.sha) |    # Group array elements by SHA
  map(.[0]) |         # Take first element from each group (dedupe)
  sort_by(.date) |    # Sort by date field
  reverse             # Newest first
'
```

**Parameterised queries with `--arg`:**

``` bash
gh api "repos/${repo}/commits" | jq -r \
  --arg repo "$repo" \
  --arg branch "$branch" \
  '.[] | {
    sha: .sha,
    repo: $repo,
    source: ("branch:" + $branch)
  }'
```

Variables passed via `--arg` are available as `$varname` in the jq expression.

**Calculating median in bash:**

Yes, I'm calculating medians in bash. No, I'm not proud. But here it is:

``` bash
calc_median() {
  local sorted
  sorted=$(echo "$1" | sort -n)
  local count
  count=$(echo "$sorted" | wc -l | tr -d ' ')
  if [[ "$count" -eq 0 ]]; then
    echo "0"
    return
  fi

  if (( count % 2 == 1 )); then
    # Odd: return middle element
    local mid=$(( (count + 1) / 2 ))
    echo "$sorted" | sed -n "${mid}p"
  else
    # Even: return average of two middle elements
    local mid1=$(( count / 2 ))
    local mid2=$(( count / 2 + 1 ))
    local val1 val2
    val1=$(echo "$sorted" | sed -n "${mid1}p")
    val2=$(echo "$sorted" | sed -n "${mid2}p")
    # Integer division is fine for commit counts
    echo $(( (val1 + val2) / 2 ))
  fi
}
```

You pass it a newline-separated list of numbers, it sorts them and picks the middle one (with a small caveat). Not elegant, but it works, and now you can tell people you've done statistical analysis in bash.

**Tab-separated output for shell consumption:**

``` bash
echo "$json" | jq -r '.[] | [.state, .title, .url, .repo] | @tsv' | \
while IFS=$'\t' read -r state title url repo; do
  # Process each field
done
```

The `@tsv` formatter produces tab-separated values, which `read` can parse cleanly.

## Extracting Help Text from Script Header

A minor but nice pattern - the script's help text lives in the header comments, and `--help` extracts it:

``` bash
show_help() {
  sed -n '2,/^$/p' "$0" | sed 's/^# \?//'
  exit 0
}
```

This prints lines from line 2 until the first empty line, stripping the =# = prefix. Documentation stays in one place, no duplication.

# Conclusions

The script is available at [github.com/d12frosted/environment](https://github.com/d12frosted/environment/blob/master/bin/gh-contributions). It requires `gh` (GitHub CLI) authenticated and `jq` installed.

After running this for a while, I can confirm the hypothesis. The correlation between high commit counts and deteriorating judgment is robust. Days with 40+ commits align suspiciously well with days where I subsequently found bugs introduced by "quick fixes," or where I woke up unsure what half the code does.

The analytics don't lie. A commit count significantly above your 30-day median might indicate productive flow. Or it might indicate something else entirely. The script provides data. You provide the interpretation. Your colleagues provide the concern.

------------------------------------------------------------------------

*If you build something similar or extend this script, I'd be curious to hear about it. Especially if you discover your own correlations.*
