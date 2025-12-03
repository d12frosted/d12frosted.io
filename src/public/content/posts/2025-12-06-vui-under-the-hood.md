<div class="info">

**What you'll learn:**

- The difference between virtual nodes (ephemeral descriptions) and component instances (persistent state)
- How the render cycle works: state change → render → reconcile → commit → effects
- Why vui.el re-renders the full buffer instead of patching in place
- How to use debugging tools: `vui-inspect`, debug logging, and timing instrumentation
- Performance patterns: keys, memoisation, `should-update`

</div>

Understanding how vui.el works internally helps you write better components, debug issues, and avoid performance pitfalls. This article explains the core concepts: virtual nodes, instances, reconciliation, and the render cycle.

# The Big Picture

vui.el follows the virtual DOM pattern:

``` text
                             State Change
                                  │
                                  ▼
┌──────────────────────────────────────────────────────────┐
│                     Component Tree                       │
│                                                          │
│   ┌─────────┐                                            │
│   │  Root   │                                            │
│   └────┬────┘                                            │
│        │                                                 │
│   ┌────┴────┐                                            │
│   ▼         ▼                                            │
│ ┌───┐    ┌───┐                                           │
│ │ A │    │ B │  ◄─── Component Instances                 │
│ └─┬─┘    └─┬─┘       (state, lifecycle)                  │
│   │        │                                             │
│   ▼        ▼                                             │
│  ...      ...                                            │
└──────────────────────────────────────────────────────────┘
                                  │
                                  │ Render
                                  ▼
┌──────────────────────────────────────────────────────────┐
│                      Virtual Tree                        │
│                                                          │
│         vui-vnode-vstack                                 │
│              │                                           │
│    ┌────────┼────────┐                                   │
│    ▼        ▼        ▼                                   │
│  text    button    field   ◄─── Virtual Nodes            │
│                              (data structures)           │
└──────────────────────────────────────────────────────────┘
                                  │
                                  │ Reconcile + Commit
                                  ▼
┌──────────────────────────────────────────────────────────┐
│                      Emacs Buffer                        │
│                                                          │
│  Hello World  [Click Me]  [___________]                  │
│                                                          │
│  ◄─── Actual text, widgets, faces                        │
└──────────────────────────────────────────────────────────┘
```

The flow works like this:

1.  **Components** hold state and define how to render
2.  **Virtual nodes** describe the desired UI structure
3.  **Reconciliation** diffs old and new virtual trees
4.  **Commit** updates the buffer with minimal changes

# Virtual Nodes (vnodes)

Virtual nodes are lightweight data structures describing UI elements:

``` elisp
;; These are all vnode structs
(vui-text "Hello")           ; vui-vnode-text
(vui-button "Click")         ; vui-vnode-button
(vui-vstack child1 child2)   ; vui-vnode-vstack
(vui-fragment child1 child2) ; vui-vnode-fragment
```

Each vnode type is a `cl-defstruct`:

``` elisp
(cl-defstruct (vui-vnode-text (:include vui-vnode))
  "Virtual node representing plain text."
  content    ; The string
  face       ; Optional face
  properties) ; Text properties

(cl-defstruct (vui-vnode-button (:include vui-vnode))
  "Virtual node representing a clickable button."
  label
  on-click
  face
  disabled-p
  max-width)
```

Virtual nodes are:

- **Immutable** - they're created fresh each render
- **Cheap** - just data, no buffer operations
- **Comparable** - the reconciler can diff them

## The Key Property

Every vnode has an optional `key` for stable identity:

``` elisp
(vui-list items
  (lambda (item)
    (vui-text (plist-get item :name)
      :key (plist-get item :id))))  ; Stable key
```

Keys help the reconciler match nodes across renders. Without keys, list items are matched by position - reordering looks like updates rather than moves.

# Component Instances

While vnodes are ephemeral, component instances persist across renders:

``` elisp
(cl-defstruct (vui-instance)
  "A live instance of a component in the tree."
  id            ; Unique identifier
  def           ; Reference to vui-component-def
  props         ; Current props plist
  state         ; Current state plist (mutable)
  vnode         ; The vui-vnode-component that created this
  parent        ; Parent vui-instance or nil for root
  children      ; Child vui-instances
  buffer        ; Buffer this instance is rendered into
  cached-vtree  ; Last rendered vtree (for should-update)
  mounted-p     ; Has this been mounted?
  mount-cleanup ; Cleanup function from on-mount
  effects       ; Alist: (effect-id . (deps . cleanup-fn))
  refs          ; Hash table: ref-id -> (value . nil)
  callbacks     ; Hash table: callback-id -> (deps . fn)
  memos         ; Hash table: memo-id -> (deps . value)
  asyncs        ; Hash table: async-id -> state plist
  prev-props    ; Props from previous render
  prev-state)   ; State from previous render
```

Instances track:

- The component's current state
- Parent/child relationships
- Hook state (effects, refs, memos, callbacks, asyncs)
- Previous props/state for `on-update` and `should-update`
- Whether lifecycle hooks have run

## Instance Tree vs Virtual Tree

These are different structures:

- **Instance tree**: Persistent, tracks state and lifecycle
- **Virtual tree**: Ephemeral, describes desired output

A component instance renders to a virtual tree each cycle. The instance persists; the virtual tree is recreated.

``` text
Instance Tree:                Virtual Tree (from one render):

  Root Instance                vui-vnode-vstack
       │                            │
   ┌───┴───┐                    ┌───┼───┐
   ▼       ▼                    ▼   ▼   ▼
 Child   Child               text btn field
  Inst    Inst
```

# The Render Cycle

When state changes, vui.el executes a render cycle:

## 1. State Update Triggered

``` elisp
(vui-set-state :count (1+ count))
```

This schedules a re-render. If inside `vui-batch`, the render is deferred until the batch completes.

## 2. Find Root, Re-render

The system finds the root instance and calls `vui--rerender-instance`:

``` elisp
(defun vui--rerender-instance (instance)
  "Re-render INSTANCE and update the buffer."
  ;; 1. Save cursor position (relative to widgets)
  ;; 2. Save window-start for all windows showing this buffer
  ;; 3. Clear widget tracking
  ;; 4. Remove widget overlays (preserve others like hl-line)
  ;; 5. Erase buffer
  ;; 6. Render the instance tree
  ;; 7. Setup widgets
  ;; 8. Restore cursor position
  ;; 9. Restore window-start positions
  ;; 10. Run pending effects
  ...)
```

## 3. Render Phase

Each instance's render function is called, producing a virtual tree:

``` elisp
;; Simplified rendering
(defun vui--render-instance (instance)
  (let* ((def (vui-instance-def instance))
         (props (vui-instance-props instance))
         (state (vui-instance-state instance))
         (render-fn (vui-component-def-render-fn def)))
    ;; Reset hook counters for this component
    ;; Call the render function
    (funcall render-fn props state)))
```

This is the "virtual" phase - no buffer changes yet.

## 4. Reconciliation

The reconciler compares old and new virtual trees:

``` text
Old Tree:          New Tree:          Actions:

  vstack             vstack
    │                  │
  ┌─┼─┐              ┌─┼─┐
  ▼ ▼ ▼              ▼ ▼ ▼
  A B C              A B D             - Keep A, B
                                       - Remove C
                                       - Add D
```

Reconciliation determines:

- Which nodes are unchanged (skip)
- Which nodes are updated (modify in place)
- Which nodes are added (create)
- Which nodes are removed (destroy)

## 5. Commit Phase

The commit phase applies changes to the actual buffer:

``` elisp
(defun vui--render-vnode (vnode)
  "Render VNODE into the current buffer at point."
  (pcase vnode
    ((pred vui-vnode-text-p)
     (insert (propertize (vui-vnode-text-content vnode)
                         'face (vui-vnode-text-face vnode))))
    ((pred vui-vnode-button-p)
     (widget-create 'push-button
                    :notify ...
                    (vui-vnode-button-label vnode)))
    ...))
```

This is where `widget.el` comes in - vui.el uses Emacs widgets for interactive elements like buttons and fields.

## 6. Lifecycle Hooks

After commit, lifecycle hooks run:

- First render: `on-mount` called (return value stored as cleanup)
- Re-render: `use-effect` callbacks run if deps changed
- Component removed: cleanup functions called, then `on-unmount`

# Widget Integration

vui.el builds on `widget.el` for interactive elements:

``` elisp
;; A vui button becomes a widget
(widget-create 'push-button
  :notify (lambda (&rest _)
            (funcall on-click-handler))
  "Button Label")

;; A vui field becomes an editable field widget
(widget-create 'editable-field
  :value "initial"
  :notify (lambda (widget &rest _)
            (funcall on-change-handler
                     (widget-value widget)))
  :size 20)
```

Widgets handle:

- Keyboard navigation (TAB between fields)
- Text input and editing
- Click handling
- Visual feedback

vui.el manages the higher-level concerns:

- When to create/update/remove widgets
- State synchronisation
- Layout and composition

# Cursor Preservation

A key feature: cursor position is preserved across re-renders. The algorithm:

1.  Before render: save cursor position relative to the current widget (by index)
2.  Render: clear and redraw buffer
3.  After render: find the widget at the same index, restore cursor with offset

This ensures typing in a field doesn't jump the cursor, and navigating through buttons stays consistent. Window scroll positions are also preserved for all windows showing the buffer.

# Why Not Just Update In Place?

You might wonder: why recreate the buffer each render? Why not just update what changed?

Emacs widgets are stateful and tied to buffer positions. When surrounding content changes (items added/removed from a list), widgets can't easily slide around. Partial updates lead to:

- Widgets pointing to wrong buffer positions
- Overlapping or orphaned widgets
- Complex bookkeeping for every possible change

The full re-render approach is simpler and more reliable. The system still optimises by:

- Skipping render when `should-update` returns nil
- Preserving component state across re-renders
- Caching values with `use-memo` and `use-callback`

# Debugging Tools

vui.el includes tools for understanding what's happening. Let's explore them with a small example.

## Try It: Exploring the Debugger

First, we need a component tree to inspect. We'll wrap `vui-button` in a custom component - not because it's useful (`vui-button` works fine on its own), but because the inspector only shows components defined with `defcomponent`, not primitive vnodes:

``` elisp
;; A thin wrapper just so we have child components to inspect
(defcomponent counter-button (label on-click)
  :render
  (vui-button label :on-click on-click))

(defcomponent debuggable-app ()
  :state ((count 0))
  :on-mount (message "App mounted!")
  :render
  (vui-vstack
   (vui-text (format "Count: %d" count))
   (vui-hstack
    (vui-component 'counter-button
      :label "+"
      :on-click (lambda () (vui-set-state :count (1+ count))))
    (vui-component 'counter-button
      :label "-"
      :on-click (lambda () (vui-set-state :count (1- count)))))))

(vui-mount (vui-component 'debuggable-app))
```

Now let's explore what the debugging tools show.

## Component Inspector

Run `M-x vui-inspect` to see the component tree:

``` text
VUI Component Inspector
============================================================

Buffer: *vui*

Component Tree:
------------------------------------------------------------
[debuggable-app] (id: 336)
  State:
    :count: 0
  Children:
  [counter-button] (id: 337)
    Props:
      :label: "+"
      :on-click: "#<function>"
  [counter-button] (id: 338)
    Props:
      :label: "-"
      :on-click: "#<function>"
```

The inspector shows:

- **Component hierarchy** - parent/child relationships between instances
- **State** - current state values for each component
- **Props** - what was passed to each component (functions display as `#<function>`)

Note that `vui-vstack`, `vui-hstack`, `vui-text`, and `vui-button` don't appear - they're primitive vnodes, not component instances. Only `defcomponent` definitions create instances that appear in the tree.

Click the "+" button, then run `vui-inspect` again:

``` text
[debuggable-app] (id: 336)
  State:
    :count: 1
  Children:
  ...
```

The state updates in real time.

For a focused view of just state across all components:

``` elisp
(vui-inspect-state)
```

## Debug Logging

Enable debug logging to see the render cycle in action:

``` elisp
(setq vui-debug-enabled t)
```

Now mount the component and click a button. Then run `M-x vui-debug-show`:

``` text
[14:16:23.919] render: <debuggable-app> rendering (first=t)
[14:16:23.920] render: <counter-button> rendering (first=t)
[14:16:23.920] mount: <counter-button> mounted
[14:16:23.920] render: <counter-button> rendering (first=t)
[14:16:23.920] mount: <counter-button> mounted
[14:16:23.920] mount: <debuggable-app> mounted
[14:16:44.845] state-change: <debuggable-app> state :count = 1
[14:16:44.857] render: <debuggable-app> rendering (first=nil)
[14:16:44.857] render: <counter-button> rendering (first=nil)
[14:16:44.858] update: <counter-button> updated
[14:16:44.858] render: <counter-button> rendering (first=nil)
[14:16:44.858] update: <counter-button> updated
[14:16:44.858] update: <debuggable-app> updated
```

The log shows:

- **Initial mount** - parent renders first, then children, but mount hooks fire bottom-up (children before parent). This ensures a parent's `on-mount` can safely interact with already-mounted children.
- **State change** - which component's state changed and to what value
- **Re-render cascade** - `(first=nil)` indicates a re-render, not initial mount
- **Update hooks** - `on-update` callbacks firing after re-render (also bottom-up)

Clear the log with `M-x vui-debug-clear`.

## Timing Instrumentation

Enable timing to measure performance:

``` elisp
(setq vui-timing-enabled t)
```

Interact with your UI, then run:

``` elisp
(vui-report-timing)
```

``` text
VUI Timing Report
============================================================

Total entries: 44

Totals by Phase:
  render:  0.0010s
  commit:  0.0022s
  mount:   0.0010s
  update:  0.0095s
  unmount: 0.0000s
  TOTAL:   0.0137s

By Component:
------------------------------------------------------------

debuggable-app (renders: 8)
  render:  0.0003s
  mount:   0.0010s
  update:  0.0000s

counter-button (renders: 16)
  render:  0.0001s
  commit:  0.0007s
  update:  0.0000s
```

This shows:

- **Time per phase** - where time is spent (render vs commit vs lifecycle hooks)
- **Render counts** - how often each component re-renders
- **Per-component breakdown** - identify which components are expensive

Clear timing data with `(vui-clear-timing)`.

## Finding Specific Instances

``` elisp
;; Find instance by ID (shown in inspector)
(vui-get-instance-by-id 337)

;; Find all instances of a component type
(vui-get-component-instances 'counter-button)
```

# Performance Considerations

Understanding internals helps you optimise:

## Minimise Re-renders

State changes trigger re-renders from the root down. Place state close to where it's used:

``` elisp
;; BAD: state at root re-renders everything
(defcomponent app ()
  :state ((hover-id nil))
  :render
  (vui-list items
    (lambda (item)
      (vui-component 'item
        :item item
        :is-hovered (eq (plist-get item :id) hover-id)
        :on-hover (lambda () (vui-set-state :hover-id ...))))))

;; BETTER: each item manages its own hover state
(defcomponent item (item)
  :state ((hovered nil))
  :render
  (vui-hstack
   (vui-text (plist-get item :name)
     :face (if hovered 'highlight 'default))
   ;; hover handlers...
   ))
```

## Use Keys for Lists

Keys let the reconciler match items efficiently:

``` elisp
;; Without keys: matched by position
(vui-list items render-fn)

;; With keys: matched by identity
(vui-list items render-fn #'item-id)
```

## Avoid Anonymous Functions in Render

Each render creates new function objects, defeating callback caching:

``` elisp
;; BAD: new function every render
:render
(vui-button "Click"
  :on-click (lambda () (do-something)))

;; BETTER: stable reference via use-callback
:render
(let ((handler (use-callback () (do-something))))
  (vui-button "Click" :on-click handler))
```

## Memoize Expensive Computations

``` elisp
:render
(let ((filtered (use-memo (items filter)
                  (seq-filter predicate items))))
  (vui-list filtered #'render-item))
```

## Use should-update for Fine Control

Skip re-renders when props/state changes don't affect output:

``` elisp
(defcomponent item (id name)
  :should-update
  (or (not (equal name (plist-get prev-props :name)))
      (not (equal id (plist-get prev-props :id))))
  :render
  (vui-text name))
```

# Summary

vui.el's architecture:

1.  **Components** are definitions; **instances** are live copies with state
2.  **Virtual nodes** describe UI without touching the buffer
3.  **Reconciliation** matches old and new trees to find changes
4.  **Commit** applies changes, using `widget.el` for interactivity
5.  **Lifecycle hooks** run at appropriate times

The pattern enables:

- Declarative code (describe what, not how)
- Efficient updates (skip unchanged subtrees via `should-update`)
- Predictable behaviour (same state → same output)
- Composability (nest components freely)

Understanding these internals helps you debug issues and write performant components. But most of the time, you can just write declarative render functions and let vui.el handle the rest.

<div class="info">

**What you learned:**

- Virtual nodes are ephemeral descriptions; component instances persist with state
- The render cycle: state change → render → reconcile → commit → effects
- Why vui.el re-renders the full buffer (widget positioning complexity)
- Debugging tools: `vui-inspect` for the component tree, `vui-debug-enabled` for render logging, `vui-timing-enabled` for performance measurement
- Performance patterns: keys for lists, `use-callback` and `use-memo` for stability, `should-update` for fine-grained control

</div>
