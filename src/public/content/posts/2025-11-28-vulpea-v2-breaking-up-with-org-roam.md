In the summer of 2020 I wrote my [first post](https://d12frosted.io/posts/2020-06-23-task-management-with-roam-vol1.html) in a series about task management with org-roam. Nearly five years later, that stack is still central to how I work. But things have shifted. Today I'm announcing vulpea v2 – a complete rewrite that no longer depends on org-roam.

To be clear: org-roam remains one of the most important additions to the Emacs ecosystem in the past decade. Jethro's idea of bringing a Roam Research-like graph to Emacs, backed by SQLite, changed how many of us work. I'm grateful for everything it gave me and the community.

But vulpea and org-roam were always moving in different directions. This post is about how I eventually accepted that.

# The early days

I found org-roam when it was still young. The core idea – caching note metadata with SQLite through EmacsQL – solved a real problem for me. I was building wine-tracking tools (which eventually became [vino](https://github.com/d12frosted/vino)), and relying on file-based search was both slow and fragile.

But it also became clear that org-roam was designed as an application, not a library. Extending it was hard. There was no straightforward way to add custom tables, influence the schema, or reuse parsed data cleanly. None of this was a "fault" – the project's goal was always explicit: replicate Roam Research in Emacs. It does that very well.

I needed something more like a foundation: a stable API layer, not a UI. So I wrote vulpea. The idea was simple: treat org-roam as an implementation detail and expose cleaner abstractions. `vulpea-note` is an example – a struct that doesn't leak internals and keeps the contract stable, regardless of the backend.

# Contributing to org-roam v2

When Jethro began working on v2, I hoped the new architecture would allow for more extension points. I got involved early – reviewing designs, testing, contributing patches.

One of those patches became what I still consider the worst contribution of my career: [org-roam-node-list](https://github.com/org-roam/org-roam/blame/f4ba41cf3d59084e182a5186d432afc9aa3fc423/org-roam-node.el#L355-L428). That SQL query is mine. And it's still there.

``` sql
SELECT
  title,
  aliases,
  id,
  file,
  filetitle,
  "level",
  todo,
  pos,
  priority,
  scheduled,
  deadline,
  properties,
  olp,
  atime,
  mtime,
  '(' || group_concat(tags, ' ') || ')' as tags,
  refs
FROM
  (
  SELECT
    id,
    file,
    filetitle,
    "level",
    todo,
    pos,
    -- ... nested subqueries continue ...
```

This query is a monster. Three levels of nested subqueries, multiple `GROUP BY` operations, string concatenation to build s-expressions in SQL. It "works" in the sense that it returns correct results. But it's slow, hard to maintain, and frankly embarrassing.

The real issue wasn't the SQL itself – it was the underlying schema. Constructing a full node meant joining several tables on every query, and doing that for thousands of nodes was always going to be expensive.

But that monster taught me something important.

# The awakening: materialised views

After some wrestling with query performance, I tried a different idea: store fully-populated notes in a single table. Do the joins at write time, not at read time.

I documented this in a [GitHub discussion](https://github.com/d12frosted/vulpea/discussions/106#discussioncomment-1601429) and implemented it in [vulpea v0.3](https://github.com/d12frosted/vulpea/pull/116). The results:

| Implementation  | Query 100 notes | Speedup |
|-----------------|-----------------|---------|
| regular DB      | 2280ms          | 1x      |
| specialised SQL | 23ms            | 120x    |

General queries improved 4–5x. Targeted ones up to 150x. On my wine database (8k+ notes at the time), that was the difference between "wait two seconds" and "it's already done".

I [proposed the idea upstream](https://github.com/org-roam/org-roam/issues/1997), but it didn't quite land. Too many moving parts, too much that would need to be rethought. And my own priorities shifted elsewhere.

# Growing apart

Since then, there have been [many discussions](https://github.com/org-roam/org-roam/issues/2474) about org-roam performance. Some suggested exposing SQL tables directly as the API. I understand the appeal – raw SQL is fast – but I think it's the wrong direction.

If your database schema becomes your API, you lose any ability to evolve it. Every table, column, and index becomes public and untouchable without breaking users. That's not sustainable.

In vulpea, the schema is an implementation detail. The public API should remain stable regardless of whether the backend is SQLite, PostgreSQL, or something stranger. The moment people depend on table names instead of functions, that stability disappears.

# Practical friction

Beyond architecture, there were everyday frustrations:

1.  **Blocking synchronisation.** org-roam updates its database on save. If you type quickly and save often, especially in huge files, Emacs stutters.

2.  **No awareness of external changes.** After a `git pull` or Dropbox/Syncthing/iCloud sync, org-roam doesn't detect changed files unless I run `org-roam-db-sync` manually.

3.  **Double parsing.** Extensions needed data org-roam didn't provide, which meant parsing files twice.

4.  **Version fragility.** vulpea relied on advice around org-roam internals. Every update risked subtle breakage.

These weren't dealbreakers individually, but together they accumulated into a constant low-level drag.

# The conversation

About a year ago, I talked with John Wiegley about all of this – the performance issues, the architectural mismatches, the maintenance burden. I said something like: "I think I should try rewriting vulpea without org-roam."

Then life intervened: war, personal matters, the general chaos of existence. Open source fell to the bottom of my priorities. A year went by.

Only recently did I find myself with enough focus and time to revisit the idea. I'd also been experimenting with tools outside Emacs – Tana, Capacities – and came back with ideas I wanted to implement properly. For that, I needed a foundation I could trust.

So I finally sat down and wrote it.

# Vulpea v2

Vulpea v2 is a full rewrite with several core principles.

## No org-roam dependency

Vulpea now maintains its own database, file watchers, and indexing pipeline. You can still run org-roam in parallel if you want; they use separate databases and don't interfere.

## Async-first architecture

No more blocking save hooks. Vulpea relies on file watchers:

- **filenotify** for changes made in Emacs
- **fswatch** (optional) for external changes (git pulls, Dropbox sync)

Updates run in the background. Emacs stays responsive, even when hundreds of files change at once.

``` elisp
(setq vulpea-db-sync-directories '("~/org/"))
(vulpea-db-autosync-mode +1)
```

## Plugin/extractor system

This is probably the part I'm most proud of. Vulpea parses each file once and hands a parse context to all extractors. Each extractor can define its own schema, version, and extraction function.

``` elisp
(vulpea-db-register-extractor
 (make-vulpea-extractor
  :name 'my-citations
  :version 1
  :schema '((citations [(note-id :not-null) (citekey :not-null)]))
  :extract-fn #'my-extract-citations
  :priority 100))
```

Want to track citations, attachments, custom metadata, links of a specific type? Register an extractor. Vulpea handles migrations, ordering, and database updates.

## Performance

Benchmarked on synthetic datasets:

| Files | Parse method          | Time    | Throughput |
|-------|-----------------------|---------|------------|
| 100K  | single-temp-buffer    | 1.6 min | 1050/s     |
| 100K  | temp-buffer (default) | 30 min  | 56/s       |
| 1M    | single-temp-buffer    | 21 min  | 795/s      |

`single-temp-buffer` is fastest but skips per-file hooks. The default mode respects file-local settings, which trades some performance for correctness.

Query performance benefits from the hybrid schema (materialised view + normalised tables). Detailed benchmarks are still pending, but performance should match or exceed v1.

## Heading-level control

If you don't use heading-level notes, you can disable them for a speed boost:

``` elisp
(setq vulpea-db-index-heading-level nil)
```

Or enable selectively:

``` elisp
(setq vulpea-db-index-heading-level
      (lambda (path)
        (string-prefix-p "/path/to/projects/" path)))
```

# Migration

If you're coming from vulpea v1, migration is simple. Files remain the same; only the database changes. The main APIs stay familiar:

``` elisp
(vulpea-db-query
 (lambda (note)
   (seq-contains-p (vulpea-note-tags note) "project")))

(vulpea-note-meta-get note "status" 'string)

(vulpea-select "Note")
```

The main adjustments are configuration (use `vulpea-db-sync-directories`) and enabling autosync. The [migration guide](https://github.com/d12frosted/vulpea/blob/v2-rewrite/docs/migration.org) covers the details.

My own migration (13k+ notes, several custom extensions and lots of code relying on vulpea) took about an evening – most of which I fixing old code that relied on org-roam directly.

# What's next

Vulpea v2 is the base layer I needed. On top of it:

## vulpea-journal

A journaling module with a widget system. The core works; I'm polishing the UX. Will push my work soon™.

## vulpea-view (working title)

A browsing and filtering interface. Interactive predicates, live results. Something I've wanted for years.

## Updated articles

My earlier task-management posts need updating for v2. I'll either revise them or write new ones.

# Closing thoughts

Five years is a long time in software. When I started with org-roam, I didn't imagine I'd end up rewriting the foundation. But sometimes the right path forward means letting go of what got you here.

To Jethro and everyone who contributed to org-roam: thank you. Your work inspired mine, and org-roam continues to serve thousands of users well. We just have different goals, and that's okay.

To vulpea users: v2 is stable enough for daily use. I've been running it for months. Try it out, report issues, and let me know what you build.

The code is at [github.com/d12frosted/vulpea](https://github.com/d12frosted/vulpea), with v2 on the [v2-rewrite branch](https://github.com/d12frosted/vulpea/tree/v2-rewrite). Documentation is in `docs/`.

Safe travels!
