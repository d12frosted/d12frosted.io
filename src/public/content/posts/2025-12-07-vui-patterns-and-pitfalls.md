<div class="info">

**What you'll learn:**

- Patterns that lead to clean, maintainable components
- Common mistakes and how to avoid them
- Debugging and performance checklists
- Quick reference for do's and don'ts

</div>

After several articles on concepts and internals, let's consolidate practical advice. This is a collection of patterns that work well and pitfalls to avoid when building vui.el applications.

# Patterns That Work

## 1. State Colocation

Keep state as close to its usage as possible:

``` elisp
;; GOOD: Form state lives in the form
(defcomponent contact-form ()
  :state ((name "") (email "") (submitting nil))
  :render
  (vui-vstack
   (vui-field :value name :on-change (lambda (v) (vui-set-state :name v)))
   (vui-field :value email :on-change (lambda (v) (vui-set-state :email v)))
   (vui-button "Submit" ...)))

;; AVOID: Form state lifted to parent unnecessarily
(defcomponent app ()
  :state ((contact-name "") (contact-email "") ...)
  :render
  (vui-component 'contact-form
    :name contact-name
    :email contact-email
    :on-name-change ...
    :on-email-change ...))
```

Colocation makes components self-contained and easier to understand. It also limits the re-render scope - when `name` changes, only `contact-form` re-renders, not the entire `app`.

## 2. Derived State via Computation

Don't store state that can be computed:

``` elisp
;; BAD: Storing derived state
(defcomponent todo-list ()
  :state ((todos '(...))
          (completed-count 0)   ; Redundant!
          (active-count 0))     ; Redundant!
  ...)

;; GOOD: Compute in render (memoize if expensive)
(defcomponent todo-list ()
  :state ((todos '(...)))
  :render
  (let ((completed (seq-count (lambda (t) (plist-get t :done)) todos))
        (active (- (length todos) completed)))
    ...))
```

Derived state can become stale. Computation is always correct. For expensive computations, wrap in `use-memo`.

## 3. Callbacks for Child-to-Parent Communication

Children emit events; parents handle them:

``` elisp
(defcomponent parent ()
  :state ((selected nil))
  :render
  (vui-list items
    (lambda (item)
      (vui-component 'selectable-item
        :item item
        :on-select (lambda ()
                     (vui-set-state :selected (plist-get item :id)))))
    (lambda (item) (plist-get item :id))))

(defcomponent selectable-item (item on-select)
  :render
  (vui-button (plist-get item :name)
    :on-click on-select))
```

Data flows down (props), events flow up (callbacks). This is unidirectional data flow.

## 4. Controlled vs Uncontrolled Inputs

**Controlled**: parent owns the value, re-renders on every change

``` elisp
(defcomponent search-form ()
  :state ((query ""))
  :render
  (vui-hstack
   (vui-field :value query
     :on-change (lambda (v) (vui-set-state :query v)))
   (vui-button "Search"
     :on-click (lambda () (search query)))))
```

**Uncontrolled**: field owns its value, parent reads on demand

``` elisp
(defcomponent search-form ()
  :render
  (vui-hstack
   (vui-field :key 'search-input :size 30)
   (vui-button "Search"
     :on-click (lambda ()
                 (search (vui-field-value 'search-input))))))
```

Use controlled when you need to validate, transform, or react to every keystroke. Use uncontrolled for simpler forms where you only care about the final value (often more performant since no re-render per keystroke).

## 5. Loading/Error/Success Pattern

Handle async states consistently:

``` elisp
(defcomponent data-display (id)
  :render
  (let ((result (use-async (list 'data id)
                  (lambda (resolve reject)
                    (fetch-data id resolve reject)))))
    (pcase (plist-get result :status)
      ('pending
       (vui-text "Loading..." :face 'shadow))
      ('error
       (vui-text (format "Error: %s" (plist-get result :error))
         :face 'error))
      ('ready
       (vui-component 'data-view
         :data (plist-get result :data))))))
```

Extract to a reusable wrapper for consistency across your app:

``` elisp
(defcomponent async-boundary (result on-success)
  :render
  (pcase (plist-get result :status)
    ('pending (vui-text "Loading..." :face 'shadow))
    ('error (vui-text (format "Error: %s" (plist-get result :error))
              :face 'error))
    ('ready (funcall on-success (plist-get result :data)))))
```

## 6. Composition Over Configuration

Prefer composing small components over configuring large ones:

``` elisp
;; AVOID: Mega-component with many props
(vui-component 'data-table
  :data items
  :columns columns
  :sortable t
  :filterable t
  :paginated t
  :page-size 20
  :on-row-click handler
  :empty-message "No items"
  ...)

;; PREFER: Composed from focused pieces
(vui-vstack
 (vui-component 'table-filters
   :on-change filter-handler)
 (vui-component 'table-body
   :data filtered-items
   :columns columns
   :on-row-click handler)
 (vui-component 'pagination
   :total (length items)
   :page page
   :on-change page-handler))
```

Small components are easier to test, reuse, and reason about.

## 7. Batching Multiple State Updates

When updating multiple state variables, use `vui-batch` to avoid intermediate re-renders:

``` elisp
;; WITHOUT batching: 3 separate re-renders
(defcomponent form ()
  :state ((name "") (email "") (valid nil))
  :render
  (vui-button "Reset"
    :on-click (lambda ()
                (vui-set-state :name "")      ; Re-render 1
                (vui-set-state :email "")     ; Re-render 2
                (vui-set-state :valid nil)))) ; Re-render 3

;; WITH batching: 1 re-render
(defcomponent form ()
  :state ((name "") (email "") (valid nil))
  :render
  (vui-button "Reset"
    :on-click (lambda ()
                (vui-batch
                  (vui-set-state :name "")
                  (vui-set-state :email "")
                  (vui-set-state :valid nil)))))
```

This is especially important in handlers that update many values or in loops.

## 8. Skip Re-renders with should-update

For components that re-render frequently but rarely change output, use `:should-update` to short-circuit:

``` elisp
(defcomponent list-item (id name on-click)
  :should-update
  ;; Only re-render if name changed (ignore on-click changes)
  (not (equal name (plist-get prev-props :name)))
  :render
  (vui-button name :on-click on-click))
```

The component still receives new props, but skips the render phase if `:should-update` returns nil. Use this when you know which props actually affect the output.

## 9. Extract Reusable Setup as Functions

For reusable stateful logic, create helper functions:

``` elisp
(defun setup-polling (interval-secs callback)
  "Start polling every INTERVAL-SECS. Returns cleanup function.
CALLBACK should be wrapped with vui-with-async-context."
  (let ((timer (run-with-timer interval-secs interval-secs callback)))
    (lambda () (cancel-timer timer))))

;; Use in any component
(defcomponent live-clock ()
  :state ((time (current-time-string)))
  :on-mount
  (setup-polling 1
    (vui-with-async-context
      (vui-set-state :time (current-time-string))))
  :render
  (vui-text time))
```

The helper returns a cleanup function, which `:on-mount` passes along for automatic cleanup on unmount.

# Common Pitfalls

## 1. Mutating State Directly

``` elisp
;; WRONG: Direct mutation doesn't trigger re-render
:on-click
(lambda ()
  (push new-item items)  ; Mutates, but vui doesn't know!
  (setq count (1+ count)))  ; Same problem

;; RIGHT: Use vui-set-state
:on-click
(lambda ()
  (vui-set-state :items (cons new-item items))
  (vui-set-state :count (1+ count)))
```

`vui-set-state` is the only way to trigger re-renders. Direct mutation silently fails to update the UI.

## 2. Hooks in Conditionals

``` elisp
;; WRONG: Hook call count varies between renders
:render
(progn
  (when show-timer
    (use-effect ()  ; Sometimes called, sometimes not!
      (setup-timer)))
  ...)

;; RIGHT: Condition inside the hook
:render
(progn
  (use-effect (show-timer)
    (when show-timer
      (setup-timer)))
  ...)
```

Hooks rely on call order for identity. If a hook is conditionally skipped, all subsequent hooks get mismatched with the wrong stored state.

## 3. Hooks Inside Loops

``` elisp
;; WRONG: Hook called N times, identity shifts as list changes
(vui-list items
  (lambda (item)
    (let ((handler (use-callback (item)  ; DON'T DO THIS!
                     (delete-item item))))
      (vui-button "Delete" :on-click handler))))

;; RIGHT: Wrap in a component (each instance has its own hook state)
(defcomponent delete-button (item on-delete)
  :render
  (let ((handler (use-callback (item)
                   (funcall on-delete item))))
    (vui-button "Delete" :on-click handler)))

;; Then in the parent:
(vui-list items
  (lambda (item)
    (vui-component 'delete-button
      :item item
      :on-delete #'delete-item))
  (lambda (item) (plist-get item :id)))
```

Hooks inside `vui-list`'s render function are called once per item. As items change, hook identities shift unpredictably. The solution is to move hooks into a child component - each component instance maintains its own hook state.

## 4. Missing Keys in Lists

``` elisp
;; WRONG: No keys  -  items matched by position
(vui-list items
  (lambda (item)
    (vui-component 'item-row :item item)))

;; RIGHT: Stable keys  -  items matched by identity
(vui-list items
  (lambda (item)
    (vui-component 'item-row :item item))
  (lambda (item) (plist-get item :id)))
```

Without keys, reordering items causes components to re-mount with wrong data, losing their state. Always provide a key function for lists that can change.

## 5. Effect Without Cleanup

``` elisp
;; WRONG: Timer keeps running after unmount
:on-mount
(run-with-timer 1 1 (lambda () (message "tick")))

;; RIGHT: Return cleanup function
:on-mount
(let ((timer (run-with-timer 1 1
               (vui-with-async-context
                 (vui-set-state :ticks #'1+)))))
  (lambda () (cancel-timer timer)))  ; Cleanup!
```

Always clean up timers, processes, and subscriptions. Both `:on-mount` and `use-effect` support returning a cleanup function.

## 6. Stale Closures in Async

``` elisp
;; WRONG: count is captured at timer creation, never updates
:on-mount
(let ((timer (run-with-timer 1 1
               (vui-with-async-context
                 (vui-set-state :count (1+ count))))))  ; count is always 0!
  (lambda () (cancel-timer timer)))

;; RIGHT: Use functional update
:on-mount
(let ((timer (run-with-timer 1 1
               (vui-with-async-context
                 (vui-set-state :count #'1+)))))  ; Gets current value
  (lambda () (cancel-timer timer)))
```

When `vui-set-state` receives a function, it calls it with the current value. This avoids stale closure problems in async callbacks.

## 7. Blocking Calls in use-async

``` elisp
;; WRONG: shell-command-to-string blocks Emacs
:render
(let ((result (use-async 'data
                (lambda (resolve _reject)
                  (resolve (shell-command-to-string "slow-command"))))))
  ...)

;; RIGHT: Use async primitives
:render
(let ((result (use-async 'data
                (lambda (resolve reject)
                  (make-process
                   :name "slow"
                   :command '("slow-command")
                   :sentinel (lambda (proc _)
                               (if (zerop (process-exit-status proc))
                                   (resolve (process-output proc))
                                 (reject "Command failed"))))))))
  ...)
```

`use-async` doesn't magically make code async. The loader must use non-blocking primitives like `make-process`, `url-retrieve`, or timers.

## 8. Context Overuse

``` elisp
;; OVERUSE: Everything in context
(user-provider user
  (theme-provider theme
    (items-provider items
      (filter-provider filter
        (sort-provider sort
          ...)))))

;; BETTER: Context for truly global things only
(user-provider user
  (theme-provider theme
    (vui-component 'main-view
      :items items
      :filter filter
      :sort sort)))
```

Context makes data flow implicit and harder to trace. Reserve it for truly global concerns (current user, theme, locale). Pass everything else as props.

## 9. Infinite Update Loops

``` elisp
;; WRONG: Effect updates its own dependency
:render
(progn
  (use-effect (count)
    ;; Triggers re-render, which runs effect, which triggers...
    (vui-set-state :count (1+ count)))
  ...)

;; ALSO WRONG: State update in render body
:render
(progn
  (vui-set-state :rendered-at (current-time))  ; Infinite loop!
  (vui-text "Hello"))
```

State updates trigger re-renders. Effects run after renders. An effect that unconditionally updates its own dependency loops forever. State updates should only happen in response to events or external triggers.

## 10. State Updates After Unmount

``` elisp
;; FRAGILE: Callback may fire after component is gone
:on-mount
(fetch-data-async
  (lambda (result)
    (vui-set-state :data result)))  ; Component might be unmounted!

;; SAFE: vui-with-async-context checks buffer liveness
:on-mount
(fetch-data-async
  (vui-with-async-context
    (vui-set-state :data result)))
```

`vui-with-async-context` automatically checks if the buffer is still alive before executing. Always wrap async callbacks that update state.

# Debugging Checklist

When something isn't working:

1.  **Is state updating?** Add `(message "state: %S" state)` in render
2.  **Is the component re-rendering?** Enable `vui-debug-enabled`, check `*vui-debug*`
3.  **Is the hook running?** Add `message` calls inside effect body
4.  **Is cleanup happening?** Check unmount in debug log
5.  **Is async completing?** Add `message` in resolve/reject callbacks
6.  **Is the buffer still alive?** Ensure `vui-with-async-context` is used
7.  **Are keys stable?** Check that key function returns consistent values

# Performance Checklist

When things are slow:

1.  **Enable timing**: `(setq vui-timing-enabled t)`, then `(vui-report-timing)`
2.  **Check re-render scope**: Is state too high in the tree?
3.  **Check `should-update`**: Can expensive components skip re-render?
4.  **Check list keys**: Are items being unnecessarily re-mounted?
5.  **Check memoization**: Are expensive computations in `use-memo`?
6.  **Check callbacks**: Are handlers stabilised with `use-callback`?
7.  **Check batching**: Are multiple updates wrapped in `vui-batch`?
8.  **Check async**: Is anything blocking the main thread?

# Quick Reference

| Do                                     | Don't                             |
|----------------------------------------|-----------------------------------|
| Use `vui-set-state`                    | Mutate state directly             |
| Return cleanup from =on-mount=/effects | Leak timers/subscriptions         |
| Use keys for dynamic lists             | Match list items by position      |
| Use `use-callback` for event handlers  | Create lambdas in render          |
| Use `use-memo` for expensive ops       | Recompute every render            |
| Put conditions inside hooks            | Put hooks inside conditions/loops |
| Use `vui-batch` for multiple updates   | Trigger many separate re-renders  |
| Use async primitives in `use-async`    | Use blocking calls                |
| Use `vui-with-async-context`           | Update state from raw async       |
| Keep state local when possible         | Over-lift state to ancestors      |
| Use context for global concerns        | Put everything in context         |
| Handle loading/error/success states    | Assume async always succeeds      |

# Summary

vui.el brings powerful patterns to Emacs UI development:

1.  **Declarative**: Describe what you want, not how to get there
2.  **Component-based**: Build complex UIs from simple, focused pieces
3.  **Unidirectional**: Data flows down via props, events flow up via callbacks
4.  **Hooks for effects**: Side effects live in hooks, not render bodies

Master these patterns, avoid the pitfalls, and you'll build maintainable, responsive interfaces in Emacs.

Happy hacking!

<div class="info">

**What you learned:**

- State colocation, derived state, and composition patterns
- Controlled vs uncontrolled inputs and when to use each
- Why hooks can't be called conditionally or in loops
- The importance of cleanup, keys, and async safety
- Debugging and performance checklists for troubleshooting

</div>
