Ever since the v2 rewrite, vulpea's documentation has said that database synchronisation is asynchronous. That was, let's be honest, a lie. A well-intentioned lie - the *scheduling* was asynchronous - but when the timer fired, all the actual work ran on the main thread, and Emacs froze for as long as your file took to index. For most notes that's a few milliseconds and nobody notices. Then someone showed up with a 1.2MB file and a one-second freeze on every save, and the lie stopped being comfortable.

This post is about fixing that properly. The synchronous path got roughly twice as fast for everyone, with no configuration changes. And there's a new opt-in mode where parsing, extraction, and even the database write happen in a separate Emacs process - saving a 100MB file now blocks the UI for about a millisecond. I'll show how to enable it, what the caveats are, and then go as deep into the machinery as you have patience for, because the interesting part isn't the result - it's how many confident assumptions turned out to be wrong along the way.

<!--more-->

# What vulpea is, in one paragraph

[Vulpea](https://github.com/d12frosted/vulpea) is a note-taking foundation for Emacs: org files as the source of truth, SQLite as a fast queryable cache, and a stable API on top. You write org, vulpea watches your directories, extracts titles, tags, links, and metadata into the database, and everything from note selection to graph queries reads from there. Since [v2](/posts/vulpea-v2-breaking-up-with-org-roam) it stands on its own foundation, and packages like [vino](https://github.com/d12frosted/vino) and [vulpea-para](https://github.com/d12frosted/vulpea-para) build on it. The critical invariant is that the database always catches up with your files - and the critical experience is that it does so without getting in your way.

That second part is what this post is about.

# The problem: scheduled is not the same as async

The report that started this ([\#359](https://github.com/d12frosted/vulpea/issues/359), thanks `@dcao`) described an unusual but perfectly legitimate setup: most of the collection in one big org file, about 1.2MB and 180k words. Every save froze Emacs for roughly a second.

Here's why. When you save a file, the sync system doesn't process it immediately - it enqueues it and sets a timer. That's the "async" part, and it's real: rapid consecutive saves get batched, nothing happens in the save hook itself. But Emacs timers run on the main thread, because in Emacs there is only one thread that matters. So when the timer fired, this all ran with the UI frozen:

1.  Read the file and hash it (change detection).
2.  Parse it with `org-element-parse-buffer`.
3.  Extract notes, links, tags, metadata from the AST.
4.  Write everything to SQLite.

For a big file, that's seconds. Batching hides this cost when you have many small files; for one large file, there's nothing to hide behind. The fix had to come from two directions: make the work cheaper, and move it off the main thread entirely.

# What you get without doing anything

The synchronous path - what everyone runs today - got systematically cheaper. Save-path freeze, measured on generated but realistic files (headings, links, metadata):

| file size | before | after |
|-----------|--------|-------|
| 100KB     | 85ms   | 63ms  |
| 1MB       | 631ms  | 364ms |
| 10MB      | 6.27s  | 3.45s |
| 100MB     | ~71s   | 35.8s |

Scaling is linear either way (org parsing is linear in file size, there's no way around that), but the constant is roughly halved. Bulk syncs benefit equally, since the costs are per-note and per-file: a full sync of 5000 small notes went from 14s to 10.9s.

Two knobs push it further if you want:

``` commonlisp
;; Bracketed links only - skip the expensive scan for plain
;; https://... links in running text. id: links are practically
;; always bracketed, so the note graph is unaffected.
(setq vulpea-db-index-plain-links nil)

;; Already existed: skip org-mode-hook during parsing.
(setq vulpea-db-parse-method 'single-temp-buffer)
```

With both, the 1MB save is at 255ms and the 10MB save at 2.34s. Still a freeze, though. Which brings us to the actual point.

# Honest async: how to enable it

``` commonlisp
(setq vulpea-db-async-extraction t)     ; parse in a worker, write in main
;; or
(setq vulpea-db-async-extraction 'full) ; parse AND write in the worker
```

With `t`, changed files are sent to a persistent `emacs --batch` subprocess. The main thread does a cheap mtime/size check, ships the path over a pipe, and gets extraction results back as plain data. The only thing left on the main thread is writing those results to the database.

With `'full`, even that write happens in the worker, through its own SQLite connection. The main thread registers note IDs with org-id - about a millisecond of work - and that's it.

The numbers, main-thread blocked time on save:

| file size | sync (new) | async `t` | async `'full` |
|-----------|------------|-----------|---------------|
| 1MB       | 364ms      | 47ms      | ~1ms          |
| 10MB      | 3.45s      | 493ms     | ~1ms          |
| 100MB     | 35.8s      | 6.8s      | ~1ms          |

That last column is not a typo. In `'full` mode the freeze is gone, and it does not depend on file size. I've been running it on my own collection: saving a 100MB stress-test file is imperceptible, and the initial full-database rebuild - the "re-indexing all files" pass after an upgrade, historically the worst freeze of all - now streams through the worker while you keep typing. Even force re-indexing (parser upgrades, settings migrations) goes through the worker. There's a dedicated measurement of this below, because it deserves one.

The obvious worry: does shipping everything through a subprocess make bulk operations slower overall? Measured on a 5000-note full sync:

| configuration | wall time | main-thread CPU |
|---------------|-----------|-----------------|
| before (v2.5) | 14.0s     | 13.0s           |
| sync, new     | 10.9s     | 10.2s           |
| async `t`     | 9.2s      | 5.3s            |
| async `'full` | 12.3s     | 0.9s            |

Async `t` is actually the *fastest* way to complete a bulk sync - worker parsing overlaps main-thread writes, a pipeline you get for free. `'full` trades a little wall time for a main thread that's essentially idle. Nothing regresses.

## The initial scan, or: rebuilding without a brick

Wall time isn't what you feel, though. What you feel is how long Emacs stops responding, and in what chunks. So here's the measurement that matters most for the worst case - a full database rebuild of 10,000 notes through autosync (what happens on first activation, or after a parser upgrade forces a re-index):

| configuration | wall  | UI blocked (total) | worst single freeze |
|---------------|-------|--------------------|---------------------|
| before (v2.5) | 38.9s | 38.8s              | 767ms               |
| sync, new     | 32.8s | 32.6s              | 829ms               |
| async `t`     | 22.1s | 19.8s              | 138ms               |
| async `'full` | 26.6s | 9.7s               | 48ms                |

Read the first row carefully: blocked time equals wall time. During a rebuild, old vulpea was a brick for the whole 39 seconds - the work ran in 700ms slabs with 10ms gaps between them, which your fingers experience as one continuous freeze. The new synchronous path is faster but has the same shape.

The async rows change the shape, not just the total. With `'full`, the remaining 9.7s of main-thread work is dispatch bookkeeping - stat calls, pipe sends - chopped into slices that never exceed 48ms. A 48ms pause is below what you notice while typing. In practice: the rebuild message appears, you keep editing, and a while later it tells you it's done. I've watched my own collection do a full forced re-index while I typed into the very buffer being indexed, and the honest report is that there is nothing to report.

## The caveats

This is opt-in for a reason. Read these before enabling:

- The worker does not run your `org-mode-hook`. It mirrors an explicit allowlist of settings (link types, tag inheritance, org-attach configuration, all the `vulpea-db-*` extraction options - kept in sync even when you change them mid-session), but hook-based per-file magic won't happen there. This is the same trade-off as the existing `single-temp-buffer` parse method.
- `'full` switches the database to WAL journaling. That's persistent, it creates `-wal` and `-shm` sidecar files next to your database, and it's not suitable for databases on network filesystems. If WAL can't be enabled, vulpea detects it and degrades to `t` automatically (without WAL, a worker write transaction would block your reads - the exact freeze we're trying to kill).
- Files the worker can't handle faithfully fall back to the synchronous path automatically, per file: encrypted files (decryption may need interaction), a function-valued `vulpea-db-index-heading-level`, and extractor plugins that read the AST - more on those below.
- Worker-extracted files become queryable a moment after the save rather than at queue time. Programmatic APIs (`vulpea-create`, `vulpea-meta-set` and friends) are untouched - they remain synchronous and read-your-writes, guarded by tests. If you have code that saves a small file and queries it in the same breath, `vulpea-db-async-extraction-threshold` can keep small files synchronous.
- Vulpea now requires Emacs 29.1. In truth it already did - the v2 database layer needs the built-in SQLite - the header just still claimed 27.2.

If something feels off, `M-x vulpea-db-worker-diagnose` runs an end-to-end health check and tells you exactly what your configuration gets: whether your files actually use the worker (and if not, why), journal mode, round-trip timing, the worker's stderr. And `(setq vulpea-db-worker-debug t)` logs the whole protocol to a buffer. These two tools earned their keep during development - more on that in the war stories.

# The gritty details

Everything above is what you need as a user. What follows is for people who enjoy knowing why. It's a long way down.

## Where the time actually went

Before optimising anything I built a benchmark (`bench/vulpea-bench-file.el`, in the repo) that measures exactly what the user feels: wall time of the save path at 100KB, 1MB, 10MB, 100MB. The first profile of a 10MB save looked like this: org-element parsing 45%, extraction 22%, database write 23%, the rest noise. No single villain - which meant no single fix.

The profiling itself deserves a confession. Emacs's `elp` instrumentation adds per-call overhead, and on functions called 30,000 times per file that overhead *is* the measurement. It burned me twice: once inflating `org-attach-dir` to look like the main cost, once inflating my own link scanner - I "optimised" the latter with a clever prescan and made the benchmark *slower*, because the cost I was chasing didn't exist. The method that actually worked was ablation: surgically no-op one component with an advice, re-run the real benchmark, measure the delta. Ground truth, no instrumentation bias. If you profile Elisp, remember this paragraph.

## The database write wasn't SQLite

The single best find of the constants work: 85% of the insert time was spent in `emacsql-compile` - emacsql escaping every value into SQL text, in Elisp, per statement. SQLite itself was nearly free. A comparison on 4000 rows: per-row emacsql inserts 308ms, one giant multi-row emacsql insert 269ms (batching barely helps - the escaping is per-value), raw `sqlite-execute` with `?` placeholders and native binding on the same connection: 30ms.

Vulpea's hot insert path now uses native binding, encoding values byte-identically to emacsql so existing databases remain compatible - locked by a test that compares the stored bytes. To be clear, this isn't a knock on emacsql: its s-expression query language is what makes the rest of vulpea's database layer pleasant, and every query still goes through it. It's just that a hot loop writing thousands of rows wants a prepared statement, not a compiler.

This also uncovered an actual bug: `emacsql-sqlite-builtin` silently swallows constraint violations - the statement just doesn't happen. One malformed link row used to silently discard *all* links of that note. The new path uses `INSERT OR IGNORE`: same tolerance for messy data, but at row granularity.

## Parsing less of the file

`org-element-parse-buffer` spends most of its time recognising inline objects - bold, timestamps, entities, subscripts - that extraction never reads. Parsing at `element` granularity instead of the default object granularity is 2.6x faster (3.0s to 1.1s on 10MB).

The catch: element granularity doesn't parse links either, and links are the whole point. The solution is to do what org's own font-lock does - scan the textual elements with a regexp and hand each candidate to `org-element-link-parser`, org's own anchored parser, so the semantics (types, positions, descriptions) match a full parse exactly. Links in source blocks, drawers, and comments are naturally excluded because we only scan textual elements.

I did not trust myself here, so the equivalence is enforced by an adversarial corpus test: a file with links in tables, quote blocks, verse blocks, metadata values, unbalanced brackets spanning elements, links inside inline `~code~` and `{{{macros}}}` (which must NOT be indexed - that one I got wrong first and the test caught it), unicode, timestamps in metadata. The test requires the element-granularity output to be plist-identical to the object-granularity output. `vulpea-db-parse-granularity` defaults to `'element`; `'object` remains as an escape hatch, and the one honest known difference is radio links, which only object mode picks up.

While decomposing link costs I found something worth its own knob: locating *unbracketed* links (plain `https://...` in running text) means scanning all body text against `org-link-plain-re`, a huge alternation over every registered link type - about 600ms of a 3.3s 10MB re-index. Bracketed links can be found with a literal `[[` search, 15x cheaper. Since `id:` links are essentially always bracketed, `vulpea-db-index-plain-links nil` buys another ~20% if you don't query plain links.

The rest of the constants work is smaller: deriving attachment directories from the note ID when the file has no `DIR` properties (instead of three property-tree walks per heading), hashing the buffer directly instead of copying 10MB to a string first, and batching org-id registration per file instead of per note (which quietly did a linear scan of `org-id-files` per note - quadratic over a rebuild).

## The worker: only data crosses the boundary

Emacs has no shared-memory threads for Lisp, so "off the main thread" means another process. The design constraints:

1.  An org-element AST is full of buffer references and circular structure - it cannot cross a process boundary. So the worker runs the *entire* extraction and ships back only the results: plain plists of note data, one per line.
2.  The wire format is printed with `print-escape-nonascii` - pure ASCII on the pipe, so process coding systems can never corrupt it. Each protocol message is one line; a large file's results stream as many lines, so the main process never blocks on one giant read.
3.  Results flow through *exactly* the same write function as synchronous updates. There is one code path that writes notes to the database, whether the parse happened here or there. This single decision prevented an entire category of divergence bugs.

The protocol is small: the main process sends `settings` (the allowlist, org link types, extractor specs, and - importantly, see below - the database schema version), then `parse` or `parse-and-write` requests. The worker answers with `begin` / `file-node` / `heading-node` / `done` streams, or `written` / `stamped` / `stale` in full-write mode.

Correctness rests on a stamp-based freshness discipline. Every result carries the mtime/size/hash of the file as the worker read it. If the file changed while the worker was parsing - you saved again, git pulled, a sync tool wrote - the result is stale: discarded, file re-queued. In full-write mode there's a second guard, because the main process can also write the database directly (programmatic APIs like `vulpea-meta-set` are synchronous by design - you read your own writes): the worker re-reads the stored stamp *inside its write transaction* and backs off if it moved. Compare-and-swap, in SQLite. A test replays the exact interleaving.

## Backpressure, or: why the initial scan still lagged

With the worker in place, saves were instant - but a full-database rebuild still stuttered. Profiling the queue showed main-thread stalls of up to 961ms during bulk dispatch. The worker was innocent. The culprit was `process-send-string`: the queue was stuffing file paths into the worker's stdin faster than the worker could drain them, and once the OS pipe buffer filled, `process-send-string` *blocks the calling thread* until there's room. The main thread was freezing on a pipe write.

The fix is flow control: at most 128 requests in flight (`vulpea-db-worker-max-in-flight`); the queue holds the rest and feeds the worker as completions free the window. Stalls dropped to 2.4ms mean, 48ms max. There's something pleasing about a 40-year-old Unix lesson - bounded queues or blocked writers - showing up in an Emacs package.

Two companions landed with it: a watchdog that kills a worker that's been silent too long with work in flight (a single pathological file can no longer wedge background indexing forever - after two consecutive hangs it falls back to synchronous processing), and honest progress reporting, because the old messages counted a *dispatch* as an *update* and cheerfully printed "Sync complete - 0.00s" before anything had happened. Now "background sync complete" fires when the last file has actually landed.

## Extractor plugins, and being wrong on my own machine

Here's an embarrassing one. After all of this worked in tests and benchmarks, I enabled `'full` on my own config and… Emacs still froze. Every save. The diagnostics I then built (`vulpea-db-worker-diagnose`) found it in one screenful: my config registers an extractor plugin - a small thing that records attachment files per note - and the async gate conservatively refused *any* file when *any* extractor was registered, because extractors receive the parse context including the AST, and the AST can't cross the process boundary. My entire collection had been silently taking the synchronous path. Worse, registered extractors also forced the slow object-granularity parse. I had been benchmarking a configuration I wasn't running.

The fix is a declaration. Most extractors - certainly mine - never touch the AST; they work from the already-extracted note data:

``` commonlisp
(vulpea-db-register-extractor
 (make-vulpea-extractor
  :name 'my-attachments
  :requires-ast nil   ; works purely from note data
  :worker-safe t      ; may run inside the worker (full mode)
  :worker-lib 'my-lib ; library the worker loads to find the function
  :schema '((attachments [(note-id :not-null) (file :not-null)]))
  :extract-fn #'my-attachments-fn))
```

`:requires-ast nil` keeps element-granularity parsing and worker eligibility - the extractor runs in the main process when results arrive. `:worker-safe t` goes further: the worker loads your library and runs the extractor itself, writing its tables through its own connection, so `'full` mode stays freeze-free even with plugins. If the worker can't resolve the function, it degrades gracefully - results stream back and the extractor runs in the main process. Nothing is ever lost, you just pay the write on the main thread. Defaults are conservative: an undeclared extractor behaves exactly as before.

## The hardening pass

Before merging I did a deep adversarial review of the whole thing - systematically trying to construct interleavings and failure states that break each component. It confirmed 28 issues. Most were small; three were the kind that justify the whole exercise:

**Process identity.** Emacs reports a dead child process through `process-live-p` immediately, but runs its *sentinel* only at the next wait point - and pending timers run first. So during a bulk sync, the queue timer routinely observes a dead worker before its sentinel has fired. The old code respawned in that window, wiping the dead worker's in-flight bookkeeping (those files would silently never be indexed), and when the stale sentinel finally ran, it clobbered the *replacement* worker's state - orphaning the live process and, in the worst interleaving, letting two workers' output interleave into one stream-assembly buffer: mixed notes written under the wrong path. The fix: salvage is one idempotent path used by everyone, and both the sentinel and the process filter verify their process is still the current one. If you maintain anything with Emacs subprocesses that can respawn, check this window - I suspect this bug exists in more packages than mine.

**The version guard.** Vulpea rebuilds its database when the schema version changes - by deleting the file. Now imagine the full-write worker running *different* vulpea code than the main session (you upgraded mid-session; stale byte-code on the load path). Its first database connection would happily "migrate" - destroying the live database out from under the running session. The settings message now carries the schema version and parser epoch; a worker whose constants differ refuses to ever open the database and streams results instead. This one made me sweat.

**Match data is global state.** `replace-regexp-in-string` iterates using the global match data. If the replacement function runs its own regexps - say, `org-timestamp-from-string` to normalise a timestamp - it corrupts the iteration. I hit this *twice* in one branch: once as an infinite loop in the link scanner (a rejected candidate left point able to move backwards - a hard Emacs hang), once as mangled timestamp splicing. `save-match-data` around anything that parses inside a replacement loop. Tattoo it somewhere.

The rest of the pass: ghost-note cleanup for files deleted mid-commit, surfacing deterministic write failures instead of retrying them forever, repairing org-id registrations lost to a crashed reply, settings changes reaching a live worker via variable watchers, and a handful of counter leaks. Every fix carries a test that replays its exact failure scenario.

# Trying it

Upgrade, and you get the faster synchronous path with zero changes. When you're ready:

``` commonlisp
(setq vulpea-db-async-extraction 'full)
```

Then run `M-x vulpea-db-worker-diagnose` and read what it says - it will tell you whether your configuration actually uses the worker, and why not if it doesn't. If you have extractor plugins, add the `:requires-ast` / `:worker-safe` declarations. If anything behaves strangely, `vulpea-db-worker-debug` gives you a full protocol log to attach to an issue.

And while you're at it, install [vulpea-ui](https://github.com/d12frosted/vulpea-ui). Vulpea itself is deliberately headless - a library, not an application - but vulpea-ui is where the database becomes something you can see: a sidebar with backlinks and unlinked mentions, dashboards, schema health. I recommend it without hesitation, and it pairs particularly well with async sync: background indexing means the data behind those views refreshes without ever costing you a freeze.

One note for package authors: `vulpea-extractor` gained struct slots, so packages defining extractors need a byte-recompile against the new version. Your package manager likely does this for you on update.

# What's next

This release closes the gap between what the documentation claimed and what the code did. Sync is async now - actually, measurably, at any file size. It's the last big piece of what I envisioned when I started the v2 rewrite: org files as truth, a database that keeps up invisibly, and an API you can build real applications on.

The async mode will stay opt-in for at least a release or two while it collects mileage on collections that aren't mine. If it holds up - and on my machine it has been flawless in a way that still surprises me when I save that 100MB test file - I'd like it to become the default. If you enable it, I genuinely want to hear how it goes, good or bad: [the issue tracker](https://github.com/d12frosted/vulpea/issues) is open, and `vulpea-db-worker-diagnose` output makes any report actionable.

Thanks to `@dcao` for the report that started all of this. Sometimes the best thing that can happen to a package is a user whose setup breaks your assumptions.
