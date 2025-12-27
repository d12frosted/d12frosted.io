[The previous article](/posts/2025-12-04-vui-hooks-deep-dive) covered lifecycle hooks - `on-mount`, `on-unmount`, `use-effect`, and `use-async`. Those hooks manage *when* things happen.

This article covers a different family: hooks that help you *avoid unnecessary work*. They're about performance and correctness when dealing with references and expensive computations.

# The Optimisation Hooks

| Hook               | Purpose                                      |
|--------------------|----------------------------------------------|
| `vui-use-ref`      | Mutable value that doesn't trigger re-render |
| `vui-use-callback` | Stable function reference across renders     |
| `vui-use-memo`     | Cached computation result                    |

Plus their variants `use-callback*` and `use-memo*` for custom comparison modes.

# Mutable References - `vui-use-ref`

Sometimes you need to store a value that:

- Persists across renders (unlike a local `let` binding)
- Doesn't trigger re-render when changed (unlike state)

That's what `vui-use-ref` provides.

## How It Works

`vui-use-ref` returns a cons cell. The `car` holds your value:

``` elisp
(let ((my-ref (vui-use-ref "initial")))
  ;; Read the value
  (car my-ref)              ; => "initial"

  ;; Update the value (no re-render!)
  (setcar my-ref "updated"))
```

Think of it as a single-slot container that vui.el preserves between renders but otherwise ignores.

## When to Use

### Storing Timer or Process References

When you create a timer in `use-effect` with dependencies, you need `vui-use-ref` to share the timer handle between the effect body and its cleanup:

``` elisp
(vui-defcomponent polling-fetcher (url)  ; url is a dependency!
  :state ((data nil))
  :render
  (let ((timer-ref (vui-use-ref nil)))
    (vui-use-effect (url)  ; Re-runs when url changes
      ;; Store the timer so cleanup can find it
      (setcar timer-ref
              (run-with-timer 0 5
                (vui-with-async-context
                  (vui-set-state :data (fetch-url url)))))
      ;; Cleanup reads from the ref
      (lambda ()
        (when (car timer-ref)
          (cancel-timer (car timer-ref)))))

    (vui-text (format "Data from %s: %s" url data))))
```

Why not just use a `let` binding? It works when the effect runs only once. But here, when `url` changes:

1.  Cleanup runs (needs to cancel the *old* timer)
2.  Effect runs again (creates a *new* timer)

The cleanup was defined when the previous timer was created, so it captured that `let` binding. With `vui-use-ref`, both the old cleanup and the new effect share the same mutable cell.

**When you don't need use-ref:** If your timer setup runs exactly once, `:on-mount` with a local `let` is simpler:

``` elisp
(vui-defcomponent simple-timer ()
  :state ((seconds 0))
  :on-mount
  (let ((timer (run-with-timer 1 1
                 (vui-with-async-context
                   (vui-set-state :seconds #'1+)))))
    ;; Cleanup returned from on-mount
    (lambda () (cancel-timer timer)))

  :render
  (vui-text (format "Elapsed: %d seconds" seconds)))
```

This works because `:on-mount` runs once, and the cleanup captures the one-and-only timer. No ref needed.

### Tracking Previous Values

The `:on-update` hook gives you `prev-props` and `prev-state` automatically. But what if you want to track the previous value of a *computed* result - something derived during render?

``` elisp
(vui-defcomponent filtered-count-tracker (items filter)
  :render
  (let ((prev-count-ref (vui-use-ref nil))
        ;; Computed value: not directly in props or state
        (filtered-count (length (seq-filter filter items))))

    ;; Compare current computed value to previous
    (when (and (car prev-count-ref)
               (/= (car prev-count-ref) filtered-count))
      (message "Filtered count changed: %d -> %d"
               (car prev-count-ref) filtered-count))

    ;; Update ref for next render (after comparison!)
    (setcar prev-count-ref filtered-count)

    (vui-text (format "Showing %d items" filtered-count))))
```

With `:on-update` you'd have to reconstruct the previous computed value from `prev-props`. With `vui-use-ref`, you simply remember whatever you want.

The trade-off: `:on-update` is automatic and declarative; `vui-use-ref` is manual but works for any value.

### Counting Renders (Debugging)

``` elisp
(vui-defcomponent render-counter ()
  :state ((count 0))
  :render
  (let ((renders (vui-use-ref 0)))
    ;; Increment on every render
    (setcar renders (1+ (car renders)))

    (vui-fragment
     (vui-text (format "Rendered %d times" (car renders)))
     (vui-newline)
     (vui-button "Force re-render"
                 :on-click (lambda ()
                             (vui-set-state :count (1+ count)))))))
```

## Try It

Evaluate this, then click the button a few times. Notice the render count increases but changing the ref itself doesn't cause re-renders - only clicking the button (which changes state) does.

``` elisp
(vui-defcomponent ref-demo ()
  :state ((trigger 0))
  :render
  (let ((clicks (vui-use-ref 0)))
    (vui-vstack
     (vui-text (format "State changes: %d" trigger))
     (vui-text (format "Total clicks: %d" (car clicks)))
     (vui-hstack
      (vui-button "Change state"
                  :on-click (lambda ()
                              (setcar clicks (1+ (car clicks)))
                              (vui-set-state :trigger (1+ trigger))))
      (vui-button "Only update ref"
                  :on-click (lambda ()
                              (setcar clicks (1+ (car clicks)))
                              (message "Ref is now %d (no re-render)"
                                       (car clicks))))))))

(vui-mount (vui-component 'ref-demo) "*ref-demo*")
```

> **What you learned:** `vui-use-ref` gives you a mutable container that persists across renders without triggering re-renders. Use it for timer handles, previous values, and any data that shouldn't affect the UI directly.

# Stable Callbacks - `vui-use-callback`

Every time a component renders, any `lambda` in the render body creates a *new* function object:

``` elisp
:render
(vui-button "Click"
            :on-click (lambda ()  ; <- New function every render!
                        (do-something)))
```

Usually this doesn't matter. But it becomes a problem when:

1.  You pass callbacks to child components
2.  Those children use `:should-update` to skip re-renders
3.  The "new" callback makes the props look different

## The Problem

``` elisp
(vui-defcomponent child-button (label on-click)
  :should-update
  ;; Skip re-render if props unchanged
  (not (equal props prev-props))

  :render
  (vui-button label :on-click on-click))

(vui-defcomponent parent ()
  :state ((count 0))
  :render
  (vui-component 'child-button
                 :label "Click me"
                 ;; This is a NEW function every render
                 ;; So child's :should-update always sees "changed" props
                 :on-click (lambda ()
                             (vui-set-state :count (1+ count)))))
```

The child's optimisation is defeated because `on-click` is always a fresh function.

## The Solution

`vui-use-callback` returns a stable function reference that only changes when its dependencies change:

``` elisp
(vui-defcomponent parent ()
  :state ((count 0))
  :render
  (let ((handle-click (vui-use-callback (count)
                        (lambda ()
                          (vui-set-state :count (1+ count))))))
    (vui-component 'child-button
                   :label "Click me"
                   :on-click handle-click)))  ; Same reference if count unchanged
```

Now `handle-click` is the *same function object* across renders, as long as `count` hasn't changed.

## Dependency List

Like `use-effect`, the first argument is a list of values to watch:

``` elisp
;; Stable forever (empty deps)
(vui-use-callback ()
  (lambda () (message "Hello")))

;; Changes when item-id changes
(vui-use-callback (item-id)
  (lambda () (delete-item item-id)))

;; Changes when any dep changes
(vui-use-callback (a b c)
  (lambda () (process a b c)))
```

## When to Use

Use `vui-use-callback` when:

- Passing callbacks to child components with `:should-update`
- Callbacks are used as dependencies in child `use-effect` calls
- You're creating many similar callbacks in a loop

Don't bother when:

- The callback goes directly to a primitive like `vui-button`
- There's no child optimisation that would benefit
- The component rarely re-renders anyway

## Try It

This demo shows the problem and solution. We have two child buttons - one receives a stable callback, one receives an unstable callback. Each child uses `:should-update` to skip re-renders when props haven't changed, and tracks its render count.

``` elisp
;; Child that skips re-render if props unchanged
(vui-defcomponent render-tracking-button (label on-click)
  :should-update
  ;; Re-render only if props changed (checks both label AND on-click)
  (or (not (equal (plist-get props :label)
                  (plist-get prev-props :label)))
      (not (eq (plist-get props :on-click)
               (plist-get prev-props :on-click))))

  :render
  (let ((renders (vui-use-ref 0)))
    (setcar renders (1+ (car renders)))
    (vui-button (format "%s (renders: %d)" label (car renders))
                :on-click on-click)))

(vui-defcomponent callback-demo ()
  :state ((other 0))
  :render
  (let ((stable-click (vui-use-callback ()
                        (lambda () (message "Stable clicked"))))
        (unstable-click (lambda ()
                          (message "Unstable clicked"))))
    (vui-vstack
     (vui-text (format "Unrelated state: %d" other))
     (vui-text "Click the button below to change unrelated state.")
     (vui-text "Watch which child re-renders:")
     (vui-newline)
     (vui-hstack
      (vui-component 'render-tracking-button
                     :label "Stable"
                     :on-click stable-click)
      (vui-component 'render-tracking-button
                     :label "Unstable"
                     :on-click unstable-click))
     (vui-newline)
     (vui-button "Change unrelated state"
                 :on-click (lambda ()
                             (vui-set-state :other (1+ other)))))))

(vui-mount (vui-component 'callback-demo) "*callback-demo*")
```

Click "Change unrelated state" several times:

- **Stable**: stays at "renders: 1" - same callback reference, `:should-update` returns nil, skips re-render
- **Unstable**: increments to 2, 3, 4… - new callback each time, `:should-update` returns t, re-renders

The unstable child re-renders wastefully because every parent render creates a *new* lambda object. Even though the lambda does the same thing, it's not `eq` to the previous one.

> **What you learned:** `vui-use-callback` returns a stable function reference that only changes when dependencies change. This prevents unnecessary re-renders in optimised child components.

# Cached Computations - `vui-use-memo`

`vui-use-memo` caches the result of an expensive computation, only recomputing when dependencies change.

## Basic Usage

``` elisp
(vui-defcomponent filtered-list ()
  :state ((items (generate-large-list))
          (filter-text ""))
  :render
  (let ((filtered (vui-use-memo (items filter-text)
                    ;; Only runs when items or filter-text changes
                    (seq-filter
                     (lambda (item)
                       (string-match-p filter-text item))
                     items))))
    (vui-vstack
     (vui-field :value filter-text
                :on-change (lambda (v)
                             (vui-set-state :filter-text v)))
     (vui-list filtered #'vui-text))))
```

Without `vui-use-memo`, the filter would run on *every* render - even if neither `items` nor `filter-text` changed.

## How It Works

1.  First render: compute the value, cache it along with deps
2.  Subsequent renders: compare new deps to cached deps
3.  If equal: return cached value (skip computation)
4.  If different: recompute, update cache, return new value

## When to Use

Use `vui-use-memo` for:

- Filtering or sorting large lists
- Complex transformations (parsing, formatting)
- Derived data that's expensive to compute

Don't use for:

- Simple computations (the overhead isn't worth it)
- Values that always change anyway
- Side effects (use `use-effect` instead)

## The Difference from `vui-use-callback`

| Hook               | Returns    | Caches                 |
|--------------------|------------|------------------------|
| `vui-use-callback` | A function | The function reference |
| `vui-use-memo`     | Any value  | The computation result |

You *could* implement `vui-use-callback` with `vui-use-memo`:

``` elisp
;; These are equivalent:
(vui-use-callback (x) (lambda () (process x)))
(vui-use-memo (x) (lambda () (process x)))
```

But `vui-use-callback` makes the intent clearer.

## Try It

``` elisp
(vui-defcomponent memo-demo ()
  :state ((numbers (number-sequence 1 1000))
          (threshold 500)
          (unrelated 0))
  :render
  (let ((filtered (vui-use-memo (numbers threshold)
                    (message "Filtering... (expensive!)")
                    (seq-filter (lambda (n) (> n threshold)) numbers))))

    (vui-vstack
     (vui-text (format "Numbers > %d: %d items" threshold (length filtered)))
     (vui-hstack
      (vui-button "Raise threshold"
                  :on-click (lambda ()
                              (vui-set-state :threshold (+ threshold 100))))
      (vui-button "Inc unrelated"
                  :on-click (lambda ()
                              (vui-set-state :unrelated (1+ unrelated)))))
     (vui-text (format "(unrelated=%d)" unrelated)))))

(vui-mount (vui-component 'memo-demo) "*memo-demo*")
```

Watch `*Messages*`. "Filtering…" appears when you click "Raise threshold" but *not* when you click "Inc unrelated" - even though both cause re-renders.

> **What you learned:** `vui-use-memo` caches computation results, only recomputing when dependencies change. Use it for expensive operations like filtering large lists.

# Custom Comparison Modes

Both `vui-use-callback` and `vui-use-memo` have starred variants that accept a `:compare` option:

- `use-callback*`
- `use-memo*`

## Comparison Modes

| Mode               | Comparison          | Speed  | Use When                      |
|--------------------|---------------------|--------|-------------------------------|
| `'equal` (default) | Structural equality | Slower | Lists, plists, nested data    |
| `'eq`              | Identity            | Fast   | Symbols, numbers, same object |
| Function           | Custom              | Varies | Special comparison logic      |

## Using `eq` for Speed

When you know your deps are symbols or you're comparing the same object reference:

``` elisp
(vui-defcomponent mode-display (mode)  ; mode is a symbol like 'edit or 'view
  :render
  (let ((mode-config (vui-use-memo* (mode)
                       :compare 'eq
                       (compute-mode-config mode))))
    (render-with-config mode-config)))
```

With `'eq`, vui.el skips the full structural comparison and just checks if it's the exact same object.

## Custom Comparison

For special cases where you control what "changed" means:

``` elisp
(vui-defcomponent length-sensitive (items)
  :render
  (let ((processed (vui-use-memo* (items)
                     :compare (lambda (old-deps new-deps)
                                ;; Only recompute if length changed
                                (= (length (car old-deps))
                                   (length (car new-deps))))
                     (expensive-process items))))
    (render-result processed)))
```

The comparison function receives the old and new dependency *lists* and should return non-nil if they're "equal" (meaning: don't recompute).

## Try It

``` elisp
(vui-defcomponent compare-demo ()
  :state ((symbol 'alpha)
          (counter 0))
  :render
  (let ((with-eq (vui-use-memo* (symbol)
                   :compare 'eq
                   (message "eq memo computed")
                   (format "Symbol: %s" symbol)))
        (with-equal (vui-use-memo* (symbol)
                      :compare 'equal
                      (message "equal memo computed")
                      (format "Symbol: %s" symbol))))

    (vui-vstack
     (vui-text with-eq)
     (vui-text with-equal)
     (vui-hstack
      (vui-button "Toggle symbol"
                  :on-click (lambda ()
                              (vui-set-state :symbol
                                             (if (eq symbol 'alpha) 'beta 'alpha))))
      (vui-button "Inc counter"
                  :on-click (lambda ()
                              (vui-set-state :counter (1+ counter))))))))

(vui-mount (vui-component 'compare-demo) "*compare-demo*")
```

Both memos recompute when you toggle the symbol. Neither recomputes when you increment the counter. In this case `'eq` and `'equal` behave the same for symbols - but `'eq` is faster for large dependency lists.

> **What you learned:** `use-callback*` and `use-memo*` accept `:compare` for faster or custom equality checks. Use `'eq` for symbols and numbers; use a function for special semantics.

# Decision Guide

When should you reach for these hooks?

## Use `vui-use-ref` When…

- You need to store a timer/process handle for later cleanup
- You want to track a previous value without re-rendering
- You're debugging render counts
- You need mutable state that doesn't affect the UI

## Use `vui-use-callback` When…

- You pass callbacks to child components that use `:should-update`
- A callback identity matters to downstream code
- You're creating callbacks in a loop for list items

## Use `vui-use-memo` When…

- You're filtering, sorting, or transforming large data
- A computation takes noticeable time
- The result is used multiple times in render

## Don't Optimise When…

- The computation is trivial
- The component rarely re-renders
- Profiling shows no actual bottleneck

Premature optimisation is still the root of all evil. Profile first with `vui-timing-enabled` and `vui-report-timing` before adding memoisation everywhere.

# Hook Rules Reminder

These hooks follow the same rules as `use-effect` (covered in the previous article):

1.  **Call unconditionally** - Don't put hooks inside `if` or `when`
2.  **Same order every render** - Don't change which hooks are called based on conditions
3.  **Complete dependencies** - Include all values used inside the hook body

``` elisp
;; BAD: conditional hook
(when show-details
  (vui-use-memo (data) (process data)))  ; Don't do this!

;; GOOD: always call, conditionally use result
(let ((processed (vui-use-memo (data) (process data))))
  (when show-details
    (vui-text processed)))
```

# Exercise

Build a `search-panel` component that:

1.  Has a text field for entering search terms
2.  Has a list of 500 items (strings)
3.  Filters the list based on the search term
4.  Uses `vui-use-memo` to avoid re-filtering on unrelated state changes
5.  Tracks total keystrokes with `vui-use-ref` (for analytics, without re-rendering)
6.  Shows both the filtered count and keystroke count

``` elisp
(vui-defcomponent search-panel ()
  :state ((query "")
          (items (mapcar (lambda (n) (format "Item %d" n))
                         (number-sequence 1 500))))
  :render
  ;; Your implementation here
  ;; Hints:
  ;; - use-ref for keystroke counter
  ;; - use-memo for filtered list
  ;; - increment ref in :on-change, before vui-set-state
  )
```

# What's Next

You now have the complete hooks toolkit:

| From Hooks Deep Dive     | From this article  |
|--------------------------|--------------------|
| `on-mount`, `on-unmount` | `vui-use-ref`      |
| `use-effect`             | `vui-use-callback` |
| `use-async`              | `vui-use-memo`     |
| `vui-with-async-context` | Custom comparison  |

For profiling and debugging your components, see the Developer Tools article.
