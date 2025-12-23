When multiple operations should trigger a single side effect (like re-rendering a UI), you need batching. The naive approach - a simple flag - breaks when batches nest. Here's a robust pattern using depth counters and `unwind-protect`.

I needed this for [vui.el](https://github.com/d12frosted/vui.el), a React-inspired UI framework for Emacs. State changes trigger re-renders, but when you update multiple pieces of state, you want one render at the end, not one per change. And critically, functions that batch internally shouldn't break outer batches that call them.

The pattern looks simple. It has subtle failure modes. Getting it right once means never debugging it again.

# The Problem

Consider a UI system where state changes trigger re-renders. Multiple changes in sequence waste work:

``` elisp
(vui-set-state :count 1)   ; re-render
(vui-set-state :name "A")  ; re-render again
(vui-set-state :active t)  ; and again!
```

Three renders when one would suffice. The obvious fix is batching:

``` elisp
(vui-batch
  (vui-set-state :count 1)
  (vui-set-state :name "A")
  (vui-set-state :active t))  ; single re-render at the end
```

But now consider: what if one of those state changes calls another function that also batches?

``` elisp
(defun update-user (user)
  (vui-batch
    (vui-set-state :name (user-name user))
    (vui-set-state :email (user-email user))))

(vui-batch
  (vui-set-state :count 1)
  (update-user current-user)  ; nested batch!
  (vui-set-state :active t))
```

With a simple flag, the inner batch ends and triggers a render, then the outer batch continues. We wanted one render but got two.

# The Depth Counter Solution

Instead of a boolean flag, use a counter:

``` elisp
(defvar vui--batch-depth 0
  "Current nesting depth of vui-batch calls.")

(defvar vui--render-pending-p nil
  "Non-nil if a render was requested during batching.")
```

The pattern:

1.  Entering a batch increments depth
2.  State changes set the pending flag (don't render immediately)
3.  Exiting a batch decrements depth
4.  Only when depth returns to 0, check the flag and render

# Implementation

Here's the batch macro:

``` elisp
(defmacro vui-batch (&rest body)
  "Batch state updates in BODY into a single re-render."
  `(let ((vui--batch-depth (1+ vui--batch-depth))
         (vui--batch-root (vui--get-root-instance)))
     (unwind-protect
         (progn ,@body)
       (cl-decf vui--batch-depth)
       (when (and (= vui--batch-depth 0)
                  vui--render-pending-p
                  vui--batch-root)
         (setq vui--render-pending-p nil)
         (vui--rerender-instance vui--batch-root)))))
```

And the state change function checks whether we're in a batch:

``` elisp
(defun vui--schedule-render ()
  "Schedule a re-render of the root instance."
  (let ((root (vui--get-root-instance)))
    (when root
      (if (> vui--batch-depth 0)
          ;; Inside a batch - just mark as pending
          (setq vui--render-pending-p t)
        ;; Not in a batch - render immediately
        (vui--rerender-instance root)))))
```

Let's trace through the nested case:

``` text
(vui-batch                        ; depth: 0 → 1
  (vui-set-state :count 1)        ; pending = t, depth > 0, no render
  (vui-batch                      ; depth: 1 → 2
    (vui-set-state :name "A")     ; pending = t, depth > 0, no render
    (vui-set-state :email "x"))   ; pending = t, depth > 0, no render
                                  ; inner batch exits: depth: 2 → 1
                                  ; depth != 0, no render
  (vui-set-state :active t))      ; pending = t, depth > 0, no render
                                  ; outer batch exits: depth: 1 → 0
                                  ; depth = 0, pending = t, RENDER!
```

Exactly one render, at the end of the outermost batch.

## Why the Explicit Decrement?

You might wonder: if `let` creates a local binding that's automatically unwound, why do we need `cl-decf`?

The subtlety is that `let` restores the *outer* binding when it exits, but we need to check the depth *inside* the `unwind-protect`, before `let` unwinds. Consider nested batches:

``` elisp
(let ((vui--batch-depth 1))        ; outer batch
  (let ((vui--batch-depth 2))      ; inner batch
    ;; inner unwind-protect runs here
    ;; we need to check: is this the outermost batch?
    ;; if we just read vui--batch-depth, we see 2
    ;; we need to decrement to 1, see it's not 0, skip render
    )
  ;; outer let restores depth to 1
  ;; outer unwind-protect runs
  ;; we decrement to 0, see it IS 0, render
  )
```

The `cl-decf` computes "what will the depth be after this batch exits" so we can decide whether to render. The `let` unwinding then restores the outer binding, which happens to be that same value. They work together: `cl-decf` for the decision, `let` for the actual scope management.

# Why unwind-protect Matters

The `unwind-protect` is critical. Without it, an error in the body would leave the depth counter wrong:

``` elisp
;; BAD: no unwind-protect
(defmacro vui-batch-broken (&rest body)
  `(progn
     (cl-incf vui--batch-depth)
     ,@body
     (cl-decf vui--batch-depth)
     (when (= vui--batch-depth 0) ...)))

;; If body signals an error:
(vui-batch-broken
  (vui-set-state :count 1)
  (error "oops"))  ; depth is now stuck at 1 forever!
```

With `unwind-protect`, the cleanup runs even on error:

``` elisp
(unwind-protect
    (progn ,@body)         ; might error
  (cl-decf vui--batch-depth))  ; always runs
```

This is the same pattern as `finally` in other languages, or Go's `defer`. In Emacs Lisp, `unwind-protect` is how you guarantee cleanup.

# Capturing Root at Batch Start

Another subtle detail: we capture the root instance when entering the batch:

``` elisp
`(let ((vui--batch-depth (1+ vui--batch-depth))
       (vui--batch-root (vui--get-root-instance)))  ; capture here
    ...)
```

Why? The body might switch buffers. If we call `vui--get-root-instance` at batch exit, we might get the wrong buffer's root (or nil).

``` elisp
(with-current-buffer vui-buffer
  (vui-batch
    (vui-set-state :count 1)
    (with-current-buffer other-buffer
      (do-something))
    ;; Back in vui-buffer now, but what if
    ;; unwind-protect checked root while in other-buffer?
    ))
```

Capturing at the start ensures we render the right tree.

# The Pattern Generalises

This depth-counter pattern works for any "do X once after outermost scope exits":

``` elisp
;; Transaction batching
(defmacro with-transaction (&rest body)
  `(let ((db--transaction-depth (1+ db--transaction-depth)))
     (unwind-protect
         (progn ,@body)
       (cl-decf db--transaction-depth)
       (when (= db--transaction-depth 0)
         (db--commit-pending-writes)))))

;; Batched logging
(defmacro with-batched-logging (&rest body)
  `(let ((log--suppress-depth (1+ log--suppress-depth)))
     (unwind-protect
         (progn ,@body)
       (cl-decf log--suppress-depth)
       (when (= log--suppress-depth 0)
         (log--flush-messages)))))

;; Event coalescing
(defmacro batch-events (&rest body)
  `(let ((event--batch-depth (1+ event--batch-depth)))
     (unwind-protect
         (progn ,@body)
       (cl-decf event--batch-depth)
       (when (= event--batch-depth 0)
         (event--emit-pending)))))
```

The skeleton is identical. Only the "what to do at depth 0" changes.

# Common Mistakes

## Forgetting unwind-protect

Already covered, but worth repeating: always use `unwind-protect` when managing depth counters. Errors will corrupt your state otherwise.

## Checking depth before decrement

``` elisp
;; WRONG: check before decrement
(when (= vui--batch-depth 1)  ; about to become 0
  (render))
(cl-decf vui--batch-depth)

;; RIGHT: check after decrement
(cl-decf vui--batch-depth)
(when (= vui--batch-depth 0)
  (render))
```

The first version looks equivalent but fails if an error occurs between the check and decrement.

## Not capturing context

If your "do X" needs context from the batch entry point, capture it in the `let`:

``` elisp
(let ((depth (1+ current-depth))
      (context (current-context)))  ; capture!
  (unwind-protect
      ...
    (do-something-with context)))
```

Don't assume the context will be the same when `unwind-protect` runs.

# Final Thoughts

Depth counters with `unwind-protect` are a small pattern, but they solve a real problem cleanly. The key insights:

1.  **Depth counter, not boolean** - enables nesting
2.  **unwind-protect** - guarantees cleanup on any exit path
3.  **let binding** - proper scoping, works with unwind-protect
4.  **Capture context early** - grab what you need at batch entry

The pattern appears simple but has subtle failure modes. Get it right once, extract it into a macro, use it everywhere.

When you find yourself wanting to "do X after everything finishes" and that "everything" might involve nested scopes calling the same code, reach for this pattern.
