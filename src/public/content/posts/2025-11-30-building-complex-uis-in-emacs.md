In my [previous article](/posts/2025-11-26-emacs-widget-library), I shared frustrations with `widget.el` and sketched a React-inspired alternative. That sketch has since become [vui.el](https://github.com/d12frosted/vui.el) - a ~2.5k line library that I now want to use for every UI I write in Emacs.

To be clear: I have a problem. I build too many custom tools. Wine databases, event planners, note-taking extensions, inventory systems. Each needs an interface. Each interface starts simple and grows complicated. At some point I decided the right response to this affliction was to write a UI framework.

This post isn't primarily about vui.el though. It's about what makes Emacs UI development hard and what patterns help - regardless of which approach you choose. I'll use a real project and three implementations as a lens, but the insights should transfer.

# What makes an Emacs UI "complex"?

Before diving into code, it's worth asking: when does UI development in Emacs actually get hard?

Here's a checklist I've found useful:

1.  **Multiple data sources**: Does your UI aggregate data from different places - files, databases, external processes?

2.  **Derived state**: Do you compute values from raw data (totals, summaries, rankings)?

3.  **Cascading updates**: Does changing X affect the display of Y and Z?

4.  **Multiple views of shared state**: Tabs, panels, or modes showing the same underlying data differently?

5.  **Inline editing**: Can users modify data directly in the display?

6.  **Persistence requirements**: Do edits need to save to specific backends?

If you tick one or two boxes, standard Emacs patterns work fine. Tick several and your approach starts to matter. Tick all six and you'll feel the pain.

My event planner for wine tastings ([brb-event-plan](https://github.com/d12frosted/brb)) ticks all six. It pulls event metadata from org files, detailed data from a separate `.data.el` file, and participant balances from a ledger system. It computes financial statements, score summaries, and QPR calculations. Changing the event price affects the forecast, the finances display, and every participant's fee. Five tabs show different views of the same event. Users edit wine prices, scores, and orders inline. Changes persist to different backends depending on what was modified.

You don't need to understand brb-event-plan to follow this article. Just know that it's the kind of UI where your approach has consequences.

I've implemented it three times, each with a different strategy. Here's what I learned.

## The cursor problem

There's something that makes Emacs UI fundamentally different from web UI: the cursor.

In a browser, your mouse pointer floats above the page. The DOM can rebuild entirely and your cursor stays where it is - hovering over whatever now happens to be under it. Annoying if a button moves, but survivable.

In Emacs, point is *the* position. It serves two masters simultaneously:

1.  **For the user**: point is where you are. It's your focus, your context, your place in the document. Every navigation command moves it, every visual cue (the cursor, the highlighted line) follows it.

2.  **For code**: point is where operations happen. `insert` adds text at point. `delete-char` removes from point. `looking-at` checks what's at point. Any code that modifies the buffer does so relative to point - and moves point as a side effect.

This dual role creates tension. When your UI code runs, it needs to move point around to make edits: go here, insert this, go there, delete that. But when it's done, point must return to where the *user* expects it - not where the code left it.

The obvious solution is `save-excursion`:

``` commonlisp
(save-excursion
  (goto-char (point-min))
  (insert "header\n")
  ;; ... more edits
  )
;; point restored! ...right?
```

But `save-excursion` saves the *byte position*, not the *logical position*. If you insert text before the saved position, you'll restore to the wrong place - the byte offset is now pointing at different content. The user was on row 5; now they're on row 6, or in the middle of the header you just inserted.

It gets worse with full redraws. If you `erase-buffer` and rebuild, there's no meaningful position to restore. The old byte offset points into completely new content. You're left with heuristics: maybe save the line number and column, rebuild, then try to go to that line and column. But what if the number of lines changed? What if the table structure is different?

Imagine if web pages moved your mouse cursor to the top-left corner of the screen whenever anything changed in the DOM. That's what naive `erase-buffer` / `insert` cycles do in Emacs.

## The scroll problem

Related but distinct: window scroll position.

You're viewing rows 20-40 of a table. You edit a value in row 25. The buffer redraws. If the redraw changes content above row 20 - even by a single character - Emacs may adjust the window start. Suddenly you're looking at rows 1-20 and your edit target has scrolled off screen.

Even if point is technically preserved, if the viewport jumps, you've lost context. The visual discontinuity breaks flow.

These aren't edge cases. They're the normal experience with naive update strategies. Every interaction causes flicker and displacement. Users learn to distrust the interface.

A good Emacs UI framework must solve both: keep point at the logical same position, and keep the viewport stable when content changes don't structurally require scrolling.

# Approach 1: Imperative rendering

The most direct approach: when something changes, redraw everything.

``` commonlisp
(defun my-ui-refresh (state)
  (let ((pos (point)))
    (erase-buffer)
    (my-ui-render-header state)
    (my-ui-render-content state)
    (goto-char (min pos (point-max)))))
```

Simple. No framework. Full control.

## When this works

- Read-mostly displays (logs, reports, dashboards)
- Infrequent updates
- UIs where cursor position doesn't matter much

## When this breaks down

The first casualty is cursor position. That `goto-char` is a guess. As discussed above, `save-excursion` doesn't help - it preserves byte position, not logical position. If content above point changed length, you're in the wrong place. If a table row was added, you might jump to a completely different row. Saving line and column numbers is better but still fragile: what if the number of lines changed?

The scroll position is even harder. You can save `window-start` and restore it, but if any content before that position changed length, you'll be showing different lines. Some people try `(save-window-excursion ...)` but it doesn't survive buffer content changes. You end up with elaborate heuristics that work sometimes.

The result: every interaction causes visual flicker. Edit a score, the whole buffer rebuilds, and you're looking at a different part of the table. Users learn to expect it, but they shouldn't have to.

The second problem is callback organisation. Every interactive element needs access to state and a way to trigger refresh:

``` commonlisp
(cl-flet ((set-host (&rest _)
            (let* ((candidates (or participants
                                   (vulpea-db-query-by-tags-some '("people"))))
                   (host (vulpea-select-from "Host" candidates :require-match t)))
              (vulpea-utils-with-note event
                (vulpea-buffer-meta-set "host" host 'append)
                (save-buffer))
              (my-ui-refresh state)))
          (set-price (&rest _)
            (let* ((price (read-number "Price: ")))
              (vulpea-utils-with-note event
                (vulpea-buffer-meta-set "price" price 'append)
                (save-buffer))
              (my-ui-refresh state))))
  (insert
   (string-table
    :data `(("Host:" ,(buttonize (or (note-title host) "_____") #'set-host))
            ("Price:" ,(buttonize (format-price price) #'set-price))))))
```

Notice how every callback ends with `my-ui-refresh`. The UI doesn't know *what* changed, only that *something* changed. Also notice the nesting - these `cl-flet` blocks grow large as you add more interactions. In brb-event-plan, some tab renderers exceeded 300 lines of nested callback definitions.

The third problem is efficiency. Redrawing a complex buffer on every keystroke adds noticeable latency. Acceptable for a button click, painful for interactive editing.

I used this approach for years. It works until it doesn't.

# Approach 2: Partial updates with zones

The obvious optimisation: if you know *what* changed, redraw only the affected parts.

The pattern looks appealing:

``` commonlisp
(defvar my-ui-zone-deps
  '((price . (forecast finances participants))
    (wines . (wines-table finances scores-summary scores-personal))
    (participants . (participants-table finances scores-personal invoices))))

(defun my-ui-update (state key value)
  (setf (alist-get key state) value)
  (save-state-to-disk state)
  (dolist (zone (alist-get key my-ui-zone-deps))
    (my-ui-redraw-zone state zone)))
```

Now changing the price only redraws three zones instead of the entire buffer. Efficient!

## The hidden costs

I tried this with `widget.el`. The implementation grew complex enough that I documented its problems in the source:

``` commonlisp
;; Known Issues (widget.el limitations):
;;
;; 1. Cursor jumps on edit - when editing quantity, the entire
;;    orders-personal zone redraws and cursor position is lost.
;;
;; 2. No fine-grained updates - can't update single table cell,
;;    must redraw entire zone.
;;
;; 3. Lambda capture in loops - must use backquoted lambdas with
;;    `,var` to properly capture loop variables.
```

The dependency tracking was the worst part. Miss a dependency and you have stale UI - a participant's score shows old values because you forgot that `scores` depends on `participants`. Add too many dependencies "just in case" and you're back to redrawing everything.

Zone boundaries are tricky too. Make zones too coarse (whole tabs) and partial updates don't help much. Make them too fine (individual cells) and the tracking overhead dominates.

And you still haven't solved the cursor problem - you've just made it more localised. If point is in the zone being redrawn, you need to figure out where it should end up. I wrote a subsystem just for this:

``` commonlisp
(defun my-ui-redraw-zone (state zone &optional skip-cursor-restore)
  (when-let ((bounds (my-ui-find-zone zone)))
    (let* ((zone-start (car bounds))
           (zone-end (cdr bounds))
           (point-in-zone (and (>= (point) zone-start)
                               (<= (point) zone-end)))
           (delta (when point-in-zone (- (point) zone-start))))
      (save-excursion
        (delete-region zone-start zone-end)
        (goto-char zone-start)
        (my-ui-render-zone state zone))
      (when (and point-in-zone (not skip-cursor-restore))
        (let ((new-bounds (my-ui-find-zone zone)))
          (when new-bounds
            (goto-char (min (+ (car new-bounds) delta)
                            (cdr new-bounds)))))))))
```

At this point I realised: I was building a bad reconciliation algorithm. The kind React does well. The kind I was doing poorly.

I abandoned this implementation incomplete.

## The lesson

If you find yourself building dependency tracking infrastructure for your UI, step back. You might need a different paradigm, not more infrastructure.

# Approach 3: Declarative components

The key insight: instead of telling Emacs *how* to update the UI, describe *what* the UI should look like given the current state. Let something else figure out the updates.

``` commonlisp
(defcomponent my-general-settings ()
  :render
  (let ((event (use-my-event))
        (host (use-my-host))
        (actions (use-my-actions)))
    (vui-vstack
     (vui-text "General" :face 'org-level-2)
     (vui-newline)
     (vui-table
      :rows (list
             (list
              (vui-text "Host:")
              (vui-button (if host (vulpea-note-title host) "_____")
                :on-click (lambda ()
                            (when-let ((new-host (select-host)))
                              (funcall (plist-get actions :set-host) new-host)))))
             (list
              (vui-text "Price:")
              (vui-button (brb-price-format (vulpea-note-meta-get event "price" 'number))
                :on-click (lambda ()
                            (let ((price (read-number "Price: ")))
                              (funcall (plist-get actions :set-event-meta) "price" price))))))))))
```

No explicit refresh calls. No dependency tracking. No cursor management code.

The component describes a relationship between state and UI. When state changes (via `vui-set-state`), the framework determines what needs updating. And critically - it preserves point position and scroll state. Edit a value in row 25, and you're still looking at row 25 with point in the same logical place.

## Pattern: Context for shared state

A common problem in complex UIs: deeply nested components need access to shared data.

The bad solution is prop drilling - passing state through every intermediate component:

``` commonlisp
;; Don't do this
(defcomponent my-app (event data participants wines host)
  :render
  (vui-vstack
   (vui-component 'my-header :event event)
   (vui-component 'my-content :event event :data data
                  :participants participants :wines wines :host host)))

(defcomponent my-content (event data participants wines host)
  :render
  (vui-component 'my-deeply-nested-thing :event event :host host))
```

Every component in the chain must know about and pass through data it doesn't use.

Context solves this:

``` commonlisp
;; Define what state exists
(defcontext my-event nil "The event being edited.")
(defcontext my-data nil "Event data from .data.el file.")
(defcontext my-host nil "The event host.")

;; Provide at the root
(defcomponent my-app ()
  :state ((event (load-event))
          (data (load-data event))
          (host (get-host event)))
  :render
  (my-event-provider event
    (my-data-provider data
      (my-host-provider host
        (vui-component 'my-content)))))

;; Consume anywhere, no matter how deep
(defcomponent my-deeply-nested-thing ()
  :render
  (let ((event (use-my-event))
        (host (use-my-host)))
    (vui-text (format "Host of %s: %s"
                      (vulpea-note-title event)
                      (vulpea-note-title host)))))
```

Components access exactly what they need. Intermediate components don't need to know or care.

## Pattern: Actions for mutations

State changes in a complex UI often need to:

- Update in-memory state
- Persist to appropriate backend (org file? data file? external system?)
- Possibly trigger side effects

Scattering this logic across components leads to inconsistency. Centralising it in action functions keeps things predictable:

``` commonlisp
(defcomponent my-app ()
  :state ((event ...)
          (data ...)
          (host ...))
  :render
  (let ((actions
         (list
          :set-host
          (lambda (new-host)
            (vui-set-state :host new-host)
            (vulpea-utils-with-note event
              (vulpea-buffer-meta-set "host" new-host 'append)
              (save-buffer)))

          :set-event-meta
          (lambda (key value)
            (vulpea-utils-with-note event
              (vulpea-buffer-meta-set key value 'append)
              (save-buffer))
            (vui-set-state :event (reload-event event)))

          :update-wine-data
          (lambda (wine-id key value)
            (let ((new-data (update-wine-in-data data wine-id key value)))
              (vui-set-state :data new-data)
              (save-data-file event new-data))))))

    (my-actions-provider actions
      (my-event-provider event
        ;; ... rest of UI
        ))))
```

Now any component can call `(funcall (plist-get actions :set-host) new-host)` without knowing how persistence works or what else needs updating.

## Pattern: Composition over monoliths

In the imperative version, tab renderers were 200-300 line functions with nested `cl-flet` blocks. Modifying one part risked breaking another.

With components, the structure is explicit:

``` commonlisp
(defcomponent my-plan-tab ()
  :render
  (vui-vstack
   (vui-component 'my-general-settings)
   (vui-component 'my-forecast)
   (vui-component 'my-finances)
   (vui-component 'my-wines-table)
   (vui-component 'my-shared-spending)
   (vui-component 'my-participants-table)
   (vui-component 'my-waiting-list)))
```

Each component handles one concern. Want to add a new section? Add one line here and implement one component. The finances component doesn't need to change when you modify the wines table.

This paid off when I added "pays for" functionality - allowing one participant to cover another's fee. In the imperative version, this would have touched callback closures, rendering logic, financial calculations, and invoice generation. With components, I modified the relevant pieces in isolation: the invoices component got a new section, the actions got two new functions, done.

## The practical benefit

Adding new functionality is less fragile. Components are isolated. State flows predictably through contexts. Actions centralise mutation logic. The framework handles the tedious parts - diffing, cursor preservation, efficient updates.

The vui.el implementation of brb-event-plan is actually shorter than the imperative version (~1100 lines vs ~1200 lines) despite having more features.

# Trade-offs and honest limitations

No approach is universally best.

## When imperative rendering is fine

- Read-mostly displays (logs, compilation output, search results)
- Simple interactions (one or two buttons, occasional refresh)
- Prototyping before you understand the full requirements
- UIs unlikely to grow more complex

The imperative approach has zero dependencies and maximum flexibility. Don't over-engineer a simple problem.

## When declarative pays off

- Frequent edits with immediate visual feedback
- Complex state relationships with cascading updates
- UIs that evolve over time (composition benefits compound)
- Multiple developers touching the same codebase

The upfront investment in structure returns dividends as complexity grows.

## Current vui.el limitations

Being honest about where things stand:

**Working well**:

- Component composition with props and state
- Context/provider pattern for shared state
- Lifecycle hooks (`on-mount`, `on-update`, `on-unmount`)
- Tables with borders and alignment
- Cursor preservation across re-renders
- Viewport/scroll stability
- Batched updates with `vui-batch`
- Error boundaries for graceful failure handling

**Still evolving**:

- No `use-reducer` yet (useful for complex state machines)
- Reconciliation uses clear-and-rerender rather than fine-grained diffing (correct but not maximally efficient for very large UIs)

**Planned**:

- Lazy loading with placeholders (brb-event-plan takes ~2 seconds to load because it fetches everything upfront - this should be deferrable)
- Fine-grained diffing if performance requires it

These are solvable. The architecture supports them; implementation is a matter of need and time.

# Choosing your approach

The three approaches represent a progression in how much the framework handles:

| Approach    | You manage                  | Framework handles                |
|-------------|-----------------------------|----------------------------------|
| Imperative  | Everything                  | Nothing                          |
| Zone-based  | Dependencies, zones, cursor | Rendering within zones           |
| Declarative | State→UI mapping            | Diffing, updates, cursor, scroll |

The more boxes you tick from the complexity checklist, the more the third column matters.

If you're building something with multiple data sources, cascading updates, or complex editing workflows - the declarative approach is worth the learning curve. If you're displaying a log file with a refresh button, just use `erase-buffer` and move on with your life.

# Try it

vui.el is available at <https://github.com/d12frosted/vui.el>. It's already handling production-complexity UIs. The API is stabilising but may still evolve based on real-world use.

The brb-event-plan implementation discussed here: <https://github.com/d12frosted/brb>

I'd welcome feedback - particularly on missing primitives, awkward patterns, or use cases that don't fit the current design. File issues or reach out directly.

And if you, like me, have a problem with building too many custom Emacs tools… at least now there's a reasonable way to give them interfaces.

Safe travels!
