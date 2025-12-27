Hooks let components tap into lifecycle events and manage side effects. [vui.el](https://github.com/d12frosted/vui.el) provides four hooks that cover most needs: `on-mount`, `on-unmount`, `vui-use-effect`, and `vui-use-async`. This article explains each in depth.

# The Lifecycle of a Component

Before diving into hooks, understand when things happen:

``` text
Component Created (vui-component called)
         │
         ▼
  First Render
         │
         ▼
  on-mount called  ◄── Component is now in the tree
         │
         │ (user interactions, state changes)
         ▼
  Re-renders (0 or more times)
         │
         │ (vui-use-effect runs based on deps)
         │
         ▼
  on-unmount called  ◄── Component about to be removed
         │
         ▼
  Component Removed
```

# Hook Rules

Before we dive in, three rules that apply to all hooks:

1.  **Hooks must be called on every render** - don't skip them conditionally
2.  **Hooks must be called in the same order** - vui.el identifies hooks by position
3.  **Don't put hooks inside conditionals** - put the condition inside the hook instead

These rules exist because vui.el tracks hooks by their call order during render. If you call a hook conditionally, the order changes between renders, and vui.el can't match up which hook is which.

# Async Context: A Quick Primer

Before we look at hooks, you need to understand one thing: when you call `vui-set-state` from asynchronous code (timers, process callbacks, hooks), you need to restore the component context. vui.el provides two macros for this:

- `vui-with-async-context` - wraps code that doesn't receive data
- `vui-async-callback` - wraps callbacks that receive arguments

``` elisp
;; Timer - no arguments needed
(run-with-timer 1 1
  (vui-with-async-context
    (vui-set-state :count #'1+)))

;; API callback - receives response data
(fetch-data-async
  (vui-async-callback (data)
    (vui-set-state :items data)))
```

We'll cover these in detail later, but you'll see them throughout the examples.

# Functional Updates: Avoiding Stale Closures

There's another pattern you'll see repeatedly: passing a function to `vui-set-state` instead of a value.

``` elisp
;; Direct value - captures 'count' at definition time
(vui-set-state :count (1+ count))

;; Functional update - receives current value when called
(vui-set-state :count #'1+)
```

Why does this matter? Consider a timer:

``` elisp
(vui-defcomponent broken-timer ()
  :state ((count 0))
  :on-mount
  (run-with-timer 1 1
    (vui-with-async-context
      ;; BUG: 'count' is captured as 0 when the timer is created
      ;; Every tick sets count to (1+ 0) = 1
      (vui-set-state :count (1+ count))))
  :render
  (vui-text (format "Count: %d" count)))  ; Always shows 1!
```

This is the **stale closure** problem. When the timer callback is created, `count` is 0. The callback captures that value. Every time the timer fires, it computes `(1+ 0)` and sets count to 1.

The fix is a **functional update**:

``` elisp
(vui-defcomponent working-timer ()
  :state ((count 0))
  :on-mount
  (run-with-timer 1 1
    (vui-with-async-context
      ;; CORRECT: #'1+ receives the current value each time
      (vui-set-state :count #'1+)))
  :render
  (vui-text (format "Count: %d" count)))  ; 1, 2, 3, 4...
```

When you pass a function, `vui-set-state` reads the current state value and passes it to your function. No stale closure.

**Rule of thumb:** In async callbacks (timers, processes, hooks), always use functional updates when the new value depends on the current value.

# on-mount: First Render Setup

`on-mount` runs once, immediately after the component's first render. Use it for one-time setup that requires the component to exist in the tree.

``` elisp
(vui-defcomponent timer-display ()
  :state ((seconds 0))
  :on-mount
  (let ((timer (run-with-timer 1 1
                 (vui-with-async-context
                   (vui-set-state :seconds #'1+)))))
    ;; Return cleanup function
    (lambda () (cancel-timer timer)))

  :render
  (vui-text (format "Elapsed: %d seconds" seconds)))
```

Try it:

``` elisp
(vui-mount (vui-component 'timer-display))
;; Watch the seconds tick up
;; Kill the buffer to stop (though see note about cleanup below)
```

## Return Value: Cleanup

If `on-mount` returns a function, that function is called during unmount. This is essential for cleanup:

``` elisp
:on-mount
(progn
  ;; Setup
  (add-hook 'post-command-hook #'my-handler nil t)
  ;; Return cleanup
  (lambda ()
    (remove-hook 'post-command-hook #'my-handler t)))
```

Without cleanup, your hooks and timers persist after the component is gone, causing errors or memory leaks.

## Common Uses

- Starting timers or intervals
- Adding buffer-local hooks
- Registering global keybindings
- Fetching initial data (though `vui-use-async` is often better)
- Setting up external subscriptions

# on-unmount: Final Cleanup

`on-unmount` runs right before the component is removed from the tree. It's your last chance to clean up.

## When Does Mount/Unmount Happen?

- **Mount**: When a component is first rendered into the tree
- **Unmount**: When a parent re-renders and no longer includes the child

Important: killing the buffer does **not** trigger unmount - the cleanup function won't run. Components only unmount during reconciliation when their parent removes them.

## Example: Toggle Timer with Cleanup

Let's create a timer that logs to `*Messages*` and a parent that can show/hide it:

``` elisp
(vui-defcomponent noisy-timer ()
  :state ((seconds 0))
  :on-mount
  (progn
    (message ">>> Timer MOUNTED")
    (let ((timer (run-with-timer 1 1
                   (vui-with-async-context
                     (vui-set-state :seconds
                       (lambda (s)
                         (message "Timer tick: %d" s)
                         (1+ s)))))))
      (lambda ()
        (message ">>> Timer UNMOUNTED - cancelling timer")
        (cancel-timer timer))))

  :render
  (vui-text (format "Elapsed: %d seconds" seconds)))

(vui-defcomponent timer-toggle ()
  :state ((show-timer t))
  :render
  (vui-vstack
   (vui-button (if show-timer "Hide Timer" "Show Timer")
     :on-click (lambda ()
                 (vui-set-state :show-timer (not show-timer))))
   (when show-timer
     (vui-component 'noisy-timer))))
```

Try it:

``` elisp
(vui-mount (vui-component 'timer-toggle))

;; Watch *Messages*:
;; 1. ">>> Timer MOUNTED"
;; 2. "Timer tick: 0", "Timer tick: 1", ...

;; Click "Hide Timer" button
;; Watch *Messages*:
;; ">>> Timer UNMOUNTED - cancelling timer"
;; No more ticks!

;; Click "Show Timer" again
;; Timer mounts fresh, starts from 0
```

The cleanup function runs when the parent's `show-timer` becomes `nil` and the `noisy-timer` component is removed from the render output during reconciliation.

## When to Use on-unmount vs on-mount Cleanup

Prefer returning cleanup from `on-mount` when possible - it keeps setup and cleanup together, making the code easier to follow.

Use `on-unmount` when you need to clean up state that accumulated during the component's lifetime - things that didn't exist at mount time.

### Example: on-mount Cleanup (Paired Resources)

When you create a resource in `on-mount`, return its cleanup:

``` elisp
(vui-defcomponent live-feed ()
  :state ((messages nil))
  :on-mount
  (let ((subscription (subscribe-to-feed
                        (vui-async-callback (new-message)
                          (vui-set-state :messages
                            (lambda (old) (cons new-message old)))))))
    ;; Cleanup: unsubscribe the same resource we created
    (lambda () (unsubscribe subscription)))
  :render
  (vui-list messages #'render-message))
```

The subscription is created at mount and cleaned up at unmount - a perfect pair.

### Example: on-unmount (Accumulated State)

Use `on-unmount` when cleanup depends on state that changes over time:

``` elisp
(vui-defcomponent document-editor (doc-id)
  :state ((content "") (dirty nil))

  :on-mount
  (vui-set-state :content (load-document doc-id))

  :on-unmount
  ;; At unmount time, check if we have unsaved changes
  ;; We can't do this in on-mount cleanup because we don't know
  ;; what the final state will be
  (lambda ()
    (when dirty
      (save-document doc-id content)
      (message "Auto-saved changes to %s" doc-id)))

  :render
  (vui-vstack
   (vui-field
     :value content
     :on-change (lambda (v)
                  (vui-set-state :content v)
                  (vui-set-state :dirty t)))
   (when dirty
     (vui-text "(unsaved)" :face 'warning))))
```

Here, `on-mount` can't return the save logic because:

1.  At mount time, `dirty` is `nil` - nothing to save
2.  At mount time, `content` is the original - hasn't been edited yet
3.  The cleanup needs the **final** state, not the initial state

# use-effect: React to Changes

`vui-use-effect` runs side effects in response to dependency changes. It's the most flexible hook.

## Why Hooks Live in :render

You'll notice `vui-use-effect` is called inside `:render`:

``` elisp
:render
(progn
  (vui-use-effect (query)
    (search-for query))
  (vui-vstack ...))
```

This might seem odd - why call a side effect during render? The answer is that vui.el needs to track which effect is which across re-renders. It does this by call order: the first `vui-use-effect` call is always "effect \#1", the second is "effect \#2", and so on.

This is why you can't put hooks in conditionals - it would change the call order between renders.

## What use-effect Can Do

`vui-use-effect` subsumes the other lifecycle hooks:

- **Empty deps** `(vui-use-effect () ...)` - runs once on mount, like `on-mount`
- **Cleanup function** - runs on unmount, like `on-unmount`
- **With deps** `(vui-use-effect (x y) ...)` - runs when `x` or `y` change
- **Cleanup before re-run** - cleanup runs before each re-execution, not just unmount
- **Multiple effects** - use several `vui-use-effect` calls for separate concerns

This flexibility comes at a cost: you must think about dependencies. With `on-mount`, it just runs once. With `vui-use-effect`, you control *when* it runs by choosing what to depend on.

## Example: Search with Cancellation

``` elisp
;; Simulate async search with cancellation support
(defvar my-search-timer nil "Current pending search timer.")

(defun my-search (query callback)
  "Search for QUERY. After 2 seconds, call CALLBACK with results."
  (message ">>> Starting search for: '%s'" query)
  (setq my-search-timer
        (run-with-timer 2 nil callback
          (list (format "Result 1 for '%s'" query)
                (format "Result 2 for '%s'" query)))))

(defun my-cancel-search ()
  "Cancel any pending search."
  (when my-search-timer
    (message ">>> Cancelling pending search!")
    (cancel-timer my-search-timer)
    (setq my-search-timer nil)))

(vui-defcomponent search-results (query)
  :state ((results nil) (loading t))
  :render
  (progn
    (vui-use-effect (query)
      (message ">>> Effect running for query: '%s'" query)
      (vui-set-state :loading t)
      (vui-set-state :results nil)
      (my-search query
        (vui-async-callback (data)
          (message ">>> Search completed for: '%s'" query)
          (vui-set-state :results data)
          (vui-set-state :loading nil)))
      ;; Return cleanup - cancels search if query changes
      #'my-cancel-search)
    (vui-vstack
     (vui-text (format "Searching: '%s'" query) :face 'bold)
     (if loading
         (vui-text "Loading...")
       (if results
           (vui-vstack
            (vui-text (format "  • %s" (nth 0 results)))
            (vui-text (format "  • %s" (nth 1 results))))
         (vui-text "No results"))))))

(vui-defcomponent search-demo ()
  :state ((query "emacs"))
  :render
  (vui-vstack
   (vui-hstack
    (vui-button "[emacs]"
      :on-click (lambda () (vui-set-state :query "emacs")))
    (vui-button "[lisp]"
      :on-click (lambda () (vui-set-state :query "lisp")))
    (vui-button "[vui]"
      :on-click (lambda () (vui-set-state :query "vui"))))
   (vui-component 'search-results :query query)))
```

Try it:

``` elisp
(vui-mount (vui-component 'search-demo))
```

Watch `*Messages*` while clicking buttons:

1.  Initial: "\>\>\> Starting search for: 'emacs'"
2.  Wait 2 seconds: "\>\>\> Search completed for: 'emacs'" - results appear
3.  Click `[lisp]` quickly (before 2 seconds): "\>\>\> Cancelling pending search!" then "\>\>\> Starting search for: 'lisp'"

The cleanup function prevents stale results from overwriting newer queries!

## Dependency List

The first argument to `vui-use-effect` is a list of dependencies:

``` elisp
;; Run once on mount (empty deps)
(vui-use-effect ()
  (message "Mounted!"))

;; Run when 'count' changes
(vui-use-effect (count)
  (message "Count is now: %d" count))

;; Run when either 'user' or 'page' changes
(vui-use-effect (user page)
  (fetch-user-page user page))
```

The effect runs:

1.  After the first render (always)
2.  After any re-render where a dependency changed

## Cleanup Function

Like `on-mount`, `vui-use-effect` can return a cleanup function:

``` elisp
(vui-use-effect (user-id)
  ;; Setup: subscribe to user updates
  (let ((sub (subscribe-user-updates user-id callback)))
    ;; Cleanup: unsubscribe
    (lambda () (unsubscribe sub))))
```

The cleanup runs:

- Before the effect runs again (when deps change)
- When the component unmounts

This ensures you don't have stale subscriptions when dependencies change.

## Effect Identity

Each `vui-use-effect` in a component has a stable identity based on its position. This matters for correct cleanup:

``` elisp
(vui-defcomponent multi-effect ()
  :state ((a 1) (b 2))
  :render
  (progn
    (vui-use-effect (a)
      (message "Effect A: %d" a))
    (vui-use-effect (b)
      (message "Effect B: %d" b))
    (vui-text "...")))
```

These are tracked separately. Changing `a` only runs the first effect.

Try it:

``` elisp
(vui-defcomponent effect-demo ()
  :state ((a 1) (b 2))
  :render
  (progn
    (vui-use-effect (a)
      (message ">>> Effect A fired: %d" a))
    (vui-use-effect (b)
      (message ">>> Effect B fired: %d" b))
    (vui-vstack
     (vui-hstack
      (vui-button "Increment A"
        :on-click (lambda () (vui-set-state :a (1+ a))))
      (vui-button "Increment B"
        :on-click (lambda () (vui-set-state :b (1+ b)))))
     (vui-text (format "A=%d, B=%d" a b)))))

(vui-mount (vui-component 'effect-demo))
;; Click "Increment A" - only Effect A fires
;; Click "Increment B" - only Effect B fires
```

## Pitfalls

Don't put `vui-use-effect` in conditionals:

``` elisp
;; WRONG: hooks must be called unconditionally
:render
(progn
  (when should-track
    (vui-use-effect (value)
      (track-value value)))  ; Don't do this!
  ...)

;; RIGHT: put the condition inside the effect
:render
(progn
  (vui-use-effect (value should-track)
    (when should-track
      (track-value value)))
  ...)
```

Hooks rely on call order for identity. Conditional calls break this.

# use-async: Data Loading

Some operations are expensive - fetching data from an API, running a shell command, parsing a large file. In single-threaded Emacs, these block the entire editor until they complete. Users can't type, scroll, or do anything else.

If the operation can run asynchronously (the API supports callbacks, or you can spawn a subprocess), you avoid blocking. But now you have a UI problem: you need loading indicators, error handling, and a way to update the display when data arrives.

Emacs offers several async mechanisms:

| Use Case                      | Recommended Approach       |
|-------------------------------|----------------------------|
| External process (shell, CLI) | `make-process` + sentinel  |
| HTTP requests                 | `plz.el` or `url-retrieve` |
| CPU-heavy pure Lisp           | `async.el` (child Emacs)   |
| Chained async operations      | `promise.el`               |

`vui-use-async` doesn't replace these - it works *with* them. You provide a loader that uses async primitives; `vui-use-async` manages the UI state (loading, success, error) and triggers re-renders when data arrives.

## The Manual Approach

Without `vui-use-async`, you'd manage loading state yourself:

``` elisp
(vui-defcomponent user-profile-manual (user-id)
  :state ((user nil) (loading t) (error nil))
  :render
  (progn
    (vui-use-effect (user-id)
      (vui-set-state :loading t)
      (vui-set-state :error nil)
      (fetch-user user-id
        (vui-async-callback (data)
          (vui-set-state :user data)
          (vui-set-state :loading nil))
        (vui-async-callback (err)
          (vui-set-state :error err)
          (vui-set-state :loading nil))))
    (cond
     (loading (vui-text "Loading..."))
     (error (vui-text (format "Error: %s" error)))
     (t (render-user user)))))
```

This works, but every async operation needs the same boilerplate.

## The use-async Solution

`vui-use-async` extracts the pattern:

``` elisp
(vui-defcomponent user-profile (user-id)
  :render
  (let ((result (vui-use-async
                  (list 'user user-id)  ; Key: determines when to re-fetch
                  (lambda (resolve reject)
                    (fetch-user user-id resolve reject)))))
    (pcase (plist-get result :status)
      ('pending (vui-text "Loading..."))
      ('error (vui-text (format "Error: %s" (plist-get result :error))))
      ('ready (render-user (plist-get result :data))))))
```

No explicit state, no manual effect setup. `vui-use-async` provides:

- **Automatic state management** - tracks pending/ready/error for you
- **Key-based re-fetching** - when `user-id` changes, re-fetch automatically
- **Caching** - same key returns cached result without re-fetching
- **Consistent API** - `resolve/reject` pattern like JavaScript Promises

The hook doesn't make your code async - your loader must use async primitives (`make-process`, `url-retrieve`, etc.). What `vui-use-async` does is manage the *UI state* around async operations.

## The Key Mechanism

The first argument is a key that identifies the async operation:

``` elisp
;; Simple key: just a symbol
(vui-use-async 'users
  (lambda (resolve reject) ...))

;; Compound key: re-fetches when user-id changes
(vui-use-async (list 'user user-id)
  (lambda (resolve reject) ...))
```

When the key changes (compared with `equal`), the previous operation is cancelled and a new one starts.

## Return Value

`vui-use-async` returns a plist with:

| Key       | Value                             |
|-----------|-----------------------------------|
| `:status` | `'pending`, `'ready`, or `'error` |
| `:data`   | The resolved data (when `ready`)  |
| `:error`  | The error message (when `error`)  |

``` elisp
(let ((result (vui-use-async 'my-data loader)))
  (pcase (plist-get result :status)
    ('pending (vui-text "Loading..."))
    ('error (vui-text (format "Error: %s" (plist-get result :error))))
    ('ready (render-data (plist-get result :data)))))
```

## Important: use-async Doesn't Make Things Async

This is a common misconception. `vui-use-async` is a state machine for managing async operations - it doesn't perform async work itself. The loader function must use actual async mechanisms:

``` elisp
;; WRONG: This blocks!
(vui-use-async 'data
  (lambda (resolve _reject)
    ;; shell-command-to-string blocks Emacs
    (resolve (shell-command-to-string "slow-command"))))

;; RIGHT: Use async primitives
(vui-use-async 'data
  (lambda (resolve reject)
    (make-process
     :name "slow-command"
     :command '("slow-command")
     :sentinel (lambda (proc _event)
                 (when (eq (process-status proc) 'exit)
                   (if (= 0 (process-exit-status proc))
                       (resolve (process-buffer-output proc))
                     (reject "Command failed")))))))
```

## Example: Async Shell Command

Here's a complete, runnable example using `make-process`:

``` elisp
(vui-defcomponent async-command-demo ()
  :state ((command "echo 'Hello from async!'"))
  :render
  (let ((result (vui-use-async
                    (list 'cmd command)
                  (lambda (resolve reject)
                    (let ((output-buffer (generate-new-buffer " *async-output*")))
                      (make-process
                       :name "async-cmd"
                       :buffer output-buffer
                       :command (list "sh" "-c" command)
                       :sentinel
                       (lambda (proc _event)
                         (when (memq (process-status proc) '(exit signal))
                           (if (= 0 (process-exit-status proc))
                               (with-current-buffer output-buffer
                                 (funcall resolve (string-trim (buffer-string))))
                             (funcall reject "Command failed"))
                           (kill-buffer output-buffer)))))))))
    (vui-vstack
     (vui-hstack
      (vui-button "echo"
        :on-click (lambda () (vui-set-state :command "echo 'Hello!'")))
      (vui-button "date"
        :on-click (lambda () (vui-set-state :command "date")))
      (vui-button "sleep"
        :on-click (lambda () (vui-set-state :command "sleep 2 && echo 'Done!'"))))
     (vui-newline)
     (pcase (plist-get result :status)
       ('pending (vui-text "Running..." :face 'shadow))
       ('error (vui-text (format "Error: %s" (plist-get result :error)) :face 'error))
       ('ready (vui-text (format "Output: %s" (plist-get result :data))))))))
```

Try it:

``` elisp
(vui-mount (vui-component 'async-command-demo))
;; Click [echo] - instant result
;; Click [date] - shows current date
;; Click [sleep] - shows "Running..." for 2 seconds, then "Done!"
;; Click [sleep] then quickly click [echo] - sleep is cancelled, echo runs
```

## Cancellation

When the key changes or component unmounts, any pending operation should be cancelled. This happens automatically if your loader respects Emacs process semantics - vui.el tracks processes and can kill them when needed.

# Combining Hooks

Hooks compose naturally. Here's a component using multiple hooks:

``` elisp
(vui-defcomponent data-view (source-id)
  :state ((is-visible t)
          (view-count 0))

  :on-mount
  (progn
    (message "DataView mounted for source %s" source-id)
    nil)  ; No cleanup needed

  :on-unmount
  (lambda ()
    (message "DataView unmounted after %d views" view-count))

  :render
  (let ((result (vui-use-async
                  (list 'source source-id)
                  (lambda (resolve reject)
                    (fetch-source source-id resolve reject)))))
    (vui-use-effect (is-visible)
      (when is-visible
        (vui-set-state :view-count #'1+)))
    (if (not is-visible)
        (vui-text "[Hidden]")
      (pcase (plist-get result :status)
        ('pending (vui-text "Loading..."))
        ('error (vui-text "Failed to load"))
        ('ready
         (vui-vstack
          (vui-text (format "Source: %s (viewed %d times)" source-id view-count))
          (vui-text (plist-get result :data))))))))
```

# Async Context: The Full Story

Earlier we introduced `vui-with-async-context` and `vui-async-callback`. Here's the complete picture.

## The Problem

When Emacs runs async code (timer callbacks, process sentinels, hooks), it's outside the component's render cycle. vui.el doesn't know which component the code belongs to, so `vui-set-state` doesn't work.

## vui-with-async-context

Use when your callback doesn't receive data:

``` elisp
;; Timer - no arguments
(run-with-timer 1 1
  (vui-with-async-context
    (vui-set-state :count #'1+)))

;; Hook - ignore any arguments passed by the hook
(let ((handler (vui-with-async-context
                 (vui-set-state :width (frame-width)))))
  (add-hook 'window-size-change-functions handler))
```

## vui-async-callback

Use when your callback receives data from the async operation:

``` elisp
;; API callback receives response data
(fetch-data-async
  (vui-async-callback (data)
    (vui-set-state :items data)))

;; Process sentinel receives proc and event
(make-process
 :command '("echo" "hello")
 :sentinel
 (vui-async-callback (proc _event)
   (when (memq (process-status proc) '(exit signal))
     (vui-set-state :done t))))
```

## Why Two Macros?

`vui-with-async-context` wraps code and returns a zero-argument function. `vui-async-callback` does the same but the returned function accepts arguments.

| Situation              | Use                      |
|------------------------|--------------------------|
| Timer tick             | `vui-with-async-context` |
| Hook (ignore args)     | `vui-with-async-context` |
| API response           | `vui-async-callback`     |
| Process sentinel       | `vui-async-callback`     |
| Any callback with data | `vui-async-callback`     |

# Custom Hook Patterns

While vui.el doesn't have a formal custom hooks system, you can create reusable patterns by combining state and effects:

``` elisp
;; Pattern: Window size tracking
(vui-defcomponent responsive-component ()
  :state ((window-width (window-width)))
  :render
  (progn
    (vui-use-effect ()
      (let ((handler (vui-with-async-context
                       (vui-set-state :window-width (window-width)))))
        (add-hook 'window-size-change-functions handler)
        (lambda ()
          (remove-hook 'window-size-change-functions handler))))
    (if (< window-width 80)
        (vui-text "Narrow layout")
      (vui-text "Wide layout"))))
```

Try it:

``` elisp
(vui-mount (vui-component 'responsive-component))
;; Resize your Emacs frame
;; Watch the text change between "Narrow layout" and "Wide layout"
```

# Error Handling

Hook errors are caught and reported without crashing your UI:

``` elisp
:on-mount
(error "Something went wrong!")  ; Won't crash, will be logged
```

Customise error handling with `vui-lifecycle-error-handler`:

``` elisp
(setq vui-lifecycle-error-handler
      (lambda (component hook-name error)
        (message "Hook error in %s (%s): %s"
                 component hook-name error)))
```

# Hook Reference

| Hook | Runs When | Returns | Use For |
|----|----|----|----|
| `on-mount` | After first render | Optional cleanup fn | One-time setup |
| `on-unmount` | Before removal | Cleanup fn | Final cleanup |
| `vui-use-effect` | Mount + deps change | Optional cleanup fn | Reactive side effects |
| `vui-use-async` | Key changes | Plist (:status :data :error) | Async data management |

# Summary

- `on-mount`: One-time setup after first render. Return cleanup function.
- `on-unmount`: Final cleanup before removal. Use when cleanup depends on final state.
- `vui-use-effect`: Run side effects when dependencies change. Most flexible.
- `vui-use-async`: Manage async operations with loading/error states.

Key principles:

1.  **Always clean up** - timers, hooks, subscriptions
2.  **Keep hooks unconditional** - don't wrap in `if`, put conditions inside
3.  **Use dependencies** to control when effects run
4.  **Use functional updates** in async callbacks to avoid stale closures
5.  **Remember**: `vui-use-async` doesn't make code async - your loader must be async

Next: Practical patterns for async data loading in Emacs.
