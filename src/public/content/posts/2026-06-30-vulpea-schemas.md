In the [v2.4 post](/posts/vulpea-v2-4) I gave schema validation a quick tour and said it deserved a post of its own. Here it is. But I want to be honest about why it exists at all: not because I promised it, but because the [v2.4 announcement](https://www.reddit.com/r/emacs/comments/1ug8rkh/) got far more engagement than I expected, and the people in that thread, asking how schemas would work for the way *they* think, are the reason the feature grew the way it has. The best parts of what follows began as someone else's question.

Two things happened since. The engine grew the parts I deferred - conditional rules, reference checks, composition. And, more fun, schemas got a UI in [vulpea-ui](https://github.com/d12frosted/vulpea-ui): a per-note health widget, in-buffer linting, and a collection-wide dashboard. The first time I pointed the dashboard at my wine notes it told me, cheerfully, that 176 of my 3,531 wines were invalid. That stung. But for the first time I could actually see it, and fix it without leaving the buffer.

<!--more-->

# A schema, in one breath

The recap, for anyone who skipped the tour. A schema is a predicate (which notes does this apply to?) and a list of field specs (what should they carry?). Validation doesn't return yes/no, it returns structured `vulpea-violation` records, each naming the note, the field, and what's wrong: `missing-required`, `wrong-type`, `invalid-reference`, `invalid-target`, `disallowed-value`, `invalid-value`.

Nothing about this is enforced at the org level. Your files stay plain org. A schema is a lens you choose to apply, not a wall you build. You can have notes a schema flags as broken and use them perfectly happily; the schema just tells you, when you ask, that they don't match the shape you said you wanted.

# The depth I deferred

Here's the wine schema I actually run, and it leans on every piece the tour skipped:

``` commonlisp
(vulpea-schema-define 'base-thing
  :predicate (lambda (n) (member "thing" (vulpea-note-tags n)))
  :fields '((:key "summary" :required t)))

(vulpea-schema-define 'wine
  :predicate (lambda (n) (member "wine" (vulpea-note-tags n)))
  :include 'base-thing
  :fields
  '((:key "name"     :type string :required t)
    (:key "producer" :type note   :required t :target-tags ("producer"))
    (:key "colour"   :type symbol :required t :one-of (red white rose orange))
    (:key "vintage"  :type number)
    (:key "carbonation method" :type symbol
          :required (lambda (n)
                      (eq (vulpea-note-meta-get n "carbonation" 'symbol)
                          'sparkling)))))
```

Three things are happening there.

**Composition.** `wine` includes `base-thing`, so every wine inherits the `summary` requirement without me repeating it. Producers include `base-thing` too. The fields merge at definition time, later ones win on a key clash, and inheritance is transitive. It's the DRY you'd want: a handful of base shapes, then specific things that extend them.

**Conditional rules.** `:required` and `:one-of` both accept a function of the note, not just a constant. So "carbonation method" is required only for sparkling wines, and I could just as easily make the allowed sweetness levels depend on the carbonation method. The rules can be as smart as the note is.

**Reference checks.** `producer` is a `note` field with `:target-tags ("producer")`. It's not enough for it to point at *a* note; it has to point at a note tagged `:producer:`. Point it at a region by mistake and you get an `invalid-target` violation naming the missing tag. Point it at a note that's since been deleted and you get `invalid-reference`. This used to be a hand-written `:validate` function in every schema; now it's declarative.

And, genuinely, thank you. Almost none of that shape was on my roadmap; it came from people who took the time to engage on the [v2.4 release thread](https://www.reddit.com/r/emacs/comments/1ug8rkh/) on r/emacs. `u/calebc42-official` simply wanting schema validation is what moved the whole arc up my list. `u/edenworky` pushed on composition and on cross-note rules (one schema enforcing things *across* related notes, not just within one), which became `:include` ([\#327](https://github.com/d12frosted/vulpea/issues/327)) and the still-open [\#328](https://github.com/d12frosted/vulpea/issues/328). `u/pereira_alex` asked whether a link field could be restricted to targets with particular tags, and about authoring, which became `:target-tags` ([\#329](https://github.com/d12frosted/vulpea/issues/329)) and schema-driven field insertion ([\#330](https://github.com/d12frosted/vulpea/issues/330)). I came in with one use case and left with a much better feature, purely because people who use notes nothing like I do showed up and shared how they think. That kind of engagement is the best part of building in the open, and I'm grateful for it.

That's the engine. It's entirely headless - functions in, `vulpea-violation` structs out - which matters more than it sounds, and I'll come back to it.

# Seeing it, and fixing it, in the buffer

A list of violation structs is not a feeling. To actually *live* with schemas I needed to see them where I work, and the sidebar widget in vulpea-ui is where that happens. Open a note, and the schema health widget answers one question: is this note healthy?

<img src="/images/2026-06-30-vulpea-schemas/schema-health-widget.webp" class="d12-image" />

A green `healthy` line when the note conforms, or its violations otherwise - one row each, with a severity-coloured bullet (red for structural problems like a missing field or a bad reference, amber for value problems like a disallowed colour), the field name, and a terse reason. Click the field and it jumps to that line in the note. For something even more live, `vulpea-schema-flymake-mode` surfaces the same violations as flymake diagnostics while you type, so a wrong value is underlined the moment you write it.

Then there's the part I'm fondest of: every violation has a `fix` button. Click it and it prompts for a corrected value the right way for that field - completion over the `:one-of` set, a note picker (filtered to the right `:target-tags`) for a reference, multi-selection for a multi-valued field - and writes it back.

<img src="/images/2026-06-30-vulpea-schemas/schema-quick-fix.webp" class="d12-image" />

The fix for a bad `colour` is just picking from `red / rose / white / orange`. No remembering the allowed set, no retyping the metadata syntax. The schema already knows what's valid, so the fix can offer it.

# The whole collection at a glance

The widget answers "is *this* note healthy?" The dashboard answers it for everything at once. `M-x vulpea-ui-schema-dashboard` opens a dedicated buffer listing every registered schema with how many notes it covers and how many are invalid, sorted so the schemas that need attention float to the top and unused ones sink to the bottom. `:include` relationships are annotated both ways.

<img src="/images/2026-06-30-vulpea-schemas/schema-dashboard.gif" class="d12-image" />

Each invalid note expands to its individual violations - the same `fix` and jump affordances as the widget, one level deeper - so the dashboard isn't just a scoreboard, it's a worklist. Expand a wine, see exactly which fields are wrong, fix the ones you want right there, and the note drops off the list once it's clean.

It is, I'll admit, a slightly humbling object to point at your own collection. It is also the single most effective thing I've built for actually keeping that collection tidy.

# How it's wired

A few decisions I'm happy with, for the people who like the seams.

**The engine is headless, and stays that way.** vulpea core knows nothing about vulpea-ui, sidebars, or `vui.el`. It exposes one per-note entry point, `vulpea-schema-note-violations`, and one collection-wide aggregator, `vulpea-schema-collection-health`, both of which return plain data. That one per-note function is what powers the sync-time warning, the flymake backend, *and* the sidebar widget. Three very different surfaces, one source of truth, zero duplicated validation logic. The dashboard is just a renderer for the aggregator.

**You decide when broken data is a problem.** As notes are indexed, `vulpea-db-schema-validation-action` controls what happens to one that fails: `silent` indexes it quietly, `warning` (the default) indexes it and tells you, `error` keeps it out of the database entirely until you fix it. Files are always the source of truth, so even `error` never touches your file - it just refuses to let the broken note into the index.

**The dashboard is synchronous, on purpose.** My first instinct was to make the scan async, then I measured. The whole thing is one database query plus an in-memory pass over the note structs: about 50 microseconds per note, a quarter second for my collection, comfortably sub-second into the tens of thousands. Pure-Elisp async would have been real complexity (there's no subprocess to lean on, unlike the ripgrep-backed widgets) for a problem I don't have. So it stays sync, cached, and re-scanned on `g`. If anyone ever does hit a wall, [there's a ticket](https://github.com/d12frosted/vulpea-ui/issues/46) with the numbers and an escalation plan.

# Try it

Honestly, these two updates, the engine and the UI sitting on top of it, are the thing I quietly wanted vulpea to be able to do the day I started it. I'm not sure I had the words for it back then. Years later, I do, and I have it.

It's all shipped. The engine is in [vulpea](https://github.com/d12frosted/vulpea), the UI in [vulpea-ui](https://github.com/d12frosted/vulpea-ui). Define a schema for a class of notes you care about, open the sidebar on one of them, and run `M-x vulpea-ui-schema-dashboard` to see the whole picture.

Start small. One schema, a couple of required fields, and the lightest `warning` action. The point was never to police your notes into a rigid shape - it's to write down the expectations you already carry in your head, and then have somewhere to look when you want to know how close reality is. For me that somewhere is a dashboard that gently reminds me I have nineteen wines to tidy up. I'm down to seventeen.

Two last things. I have, honestly, one serious use case for this (wine, obviously), so if you lean on structured notes in a way I haven't imagined, I'd love to hear about it - the v2.4 feedback shaped half of this post, and I'd happily do that again. And what's next, in order: first a release across the whole vulpea ecosystem, every package picked up something good this cycle, and then the one I'm most excited about, a filterable **table view** for collections of notes ([vulpea-ui#15](https://github.com/d12frosted/vulpea-ui/issues/15)), the Dataview / Notion-style "show me everything matching this shape" view a few people have already asked for. Schemas know what a class of notes should look like; a table is the natural next way to look at a whole shelf of them at once.
