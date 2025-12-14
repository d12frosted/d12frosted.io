Imagine you're building a UI framework where components are just functions. Each time the UI updates, you call the function again to get the new view. Simple enough - but now you want components to have state. Where does that state live?

The function itself can't hold it (it runs fresh each time). You could store it externally keyed by component, but what if a component needs *multiple* pieces of state? You need to associate each piece of state with a specific call site within the function.

I ran into this problem while building [vui.el](https://github.com/d12frosted/vui.el), a React-inspired UI framework for Emacs. But even if you never build a UI library, the solution React pioneered is worth understanding - it's one of those ideas that seems obvious in hindsight but required genuine insight to discover.

The obvious answer is explicit keys:

``` elisp
;; Explicit key approach
(use-state :count 0)     ; identify by :count
(use-state :name "Bob")  ; identify by :name
```

But React's hooks solved this differently, and the pattern is worth understanding even outside React. The solution is subtle but powerful: use call order during execution as implicit identity.

# The Problem

Consider a component that uses multiple hooks:

``` elisp
(defcomponent my-counter ()
  :state ((count 0)
          (name ""))
  :render
  (progn
    (use-effect (count)
      (message "Count changed to %d" count))
    (use-effect (name)
      (message "Name changed to %s" name))
    (vui-text (format "%s: %d" name count))))
```

Each `use-effect` needs to track its own state: previous dependency values, cleanup functions, whether it has run before. The system must match "the first effect" on render N with "the first effect" on render N+1.

The naive approach is explicit identification:

``` elisp
;; What we want to avoid
(use-effect :effect-1 (count) ...)
(use-effect :effect-2 (name) ...)
```

This works but has problems:

1.  **Boilerplate** - every hook call needs a unique key
2.  **Collision risk** - what if two calls accidentally use the same key?
3.  **Refactoring friction** - moving code around requires updating keys

# The Solution: Implicit Ordering

Instead of explicit keys, use a counter that increments during render:

``` elisp
(defvar vui--effect-index 0
  "Counter for use-effect calls within current render.")

(defun vui--register-effect (deps effect-fn)
  "Register an effect with DEPS and EFFECT-FN."
  (let* ((instance vui--current-instance)
         (effect-id vui--effect-index)  ; The counter IS the identity
         (effects (vui-instance-effects instance))
         (prev-entry (assq effect-id effects)))
    ;; Increment for next hook call
    (cl-incf vui--effect-index)
    ;; Now use effect-id to look up previous state...
    (when (or (null prev-entry)
              (not (equal (cadr prev-entry) deps)))
      ;; Schedule effect to run after commit
      (push (list instance effect-id deps effect-fn (caddr prev-entry))
            vui--pending-effects))))
```

The key insight: if hooks are called in the same order every render, then the Nth hook call on render 1 corresponds to the Nth hook call on render 2.

# Why This Works

The counter resets at the start of each component's render:

``` elisp
(defun vui--render-instance (instance)
  "Render INSTANCE, returning the virtual tree."
  (let* ((vui--current-instance instance)
         (vui--effect-index 0)    ; Reset counter
         (vui--ref-index 0)       ; Reset for refs too
         (vui--callback-index 0)  ; And callbacks
         (vui--memo-index 0)      ; And memos
         ;; ...
         )
    (funcall render-fn props state)))
```

Each hook type has its own counter. During render, they increment in sequence. The position in this sequence becomes the hook's identity.

This creates a strict requirement: **hooks must be called in the same order every render**. You can't conditionally call hooks:

``` elisp
;; BAD: conditional hook
(when show-details
  (use-effect () ...))  ; Sometimes index 0, sometimes not called

;; GOOD: always call, conditionally act
(use-effect ()
  (when show-details
    ...))
```

This is the famous "Rules of Hooks" constraint - React's requirement that hooks be called unconditionally and in consistent order. It seems restrictive, but in practice it rarely matters, and the benefits are worth it.

# The Full Pattern

Here's how multiple hook types coexist, each with their own counter:

``` elisp
;; Each hook type maintains its own index
(defvar vui--effect-index 0)
(defvar vui--ref-index 0)
(defvar vui--callback-index 0)
(defvar vui--memo-index 0)

;; use-ref follows the same pattern
(defun vui--get-or-create-ref (initial-value)
  "Get existing ref or create new one with INITIAL-VALUE."
  (let* ((instance vui--current-instance)
         (ref-id vui--ref-index)  ; Counter as identity
         (refs (vui-instance-refs instance)))
    (cl-incf vui--ref-index)     ; Increment for next call
    (or (gethash ref-id refs)
        (puthash ref-id (cons initial-value nil) refs))))

;; use-callback uses the same approach
(defun vui--get-or-create-callback (deps callback-fn)
  "Get cached callback or create new one."
  (let* ((instance vui--current-instance)
         (callback-id vui--callback-index)
         (callbacks (vui-instance-callbacks instance)))
    (cl-incf vui--callback-index)
    (let ((entry (gethash callback-id callbacks)))
      (if (and entry (equal (car entry) deps))
          (cdr entry)  ; Return cached
        ;; Update cache
        (puthash callback-id (cons deps callback-fn) callbacks)
        callback-fn))))
```

# Storage Structures

The instance stores hook state in various structures, all keyed by these implicit IDs:

``` elisp
(cl-defstruct (vui-instance)
  ;; ...
  effects    ; Alist: ((effect-id deps cleanup-fn) ...)
  refs       ; Hash table: ref-id -> (value . nil)
  callbacks  ; Hash table: callback-id -> (deps . fn)
  memos      ; Hash table: memo-id -> (deps . value)
  ;; ...
  )
```

Each effect entry is a flat list `(effect-id deps cleanup-fn)`, so `cadr` retrieves deps and `caddr` retrieves the cleanup function. The effect system uses an alist because effects need ordering for cleanup. Refs, callbacks, and memos use hash tables for O(1) lookup.

# Why Not Explicit Keys?

Beyond the boilerplate issue, implicit identity has a deeper advantage: it makes hooks **compositional**.

Consider `use-previous`, a classic pattern for tracking a value's previous state:

``` elisp
(defun use-previous (value)
  "Return the previous VALUE from last render."
  (let ((ref (use-ref nil)))
    (use-effect (value)
      (setcar ref value)
      nil)
    (car ref)))
```

This hook combines `use-ref` and `use-effect` - two indices consumed, no coordination needed. During render N, you read from the ref (which holds the value from render N-1), then after commit the effect updates it for render N+1. The timing matters: effects run **after** render commits, which is why you can access the "previous" value during render.

This pattern enables comparing current vs previous values - useful for detecting changes, triggering animations on transitions, or skipping work when a value hasn't meaningfully changed.

With explicit keys, composing hooks would require key namespacing:

``` elisp
;; Hypothetical explicit-key version - messy
(defun use-previous (key value)
  (let ((ref (use-ref (intern (format "%s-ref" key)) nil)))
    (use-effect (intern (format "%s-effect" key)) (value)
      ...)))
```

Implicit identity means custom hooks "just work" - they consume some indices, and callers don't need to know or care.

# The Trade-off

This pattern trades explicit control for implicit correctness. You can't introspect which hook is which by name. You can't reorder hooks safely. Conditional hooks break things in subtle ways.

But in exchange, you get:

1.  **Zero boilerplate** - just call the hook
2.  **Automatic namespacing** - hooks in different components can't collide
3.  **Composability** - custom hooks work without coordination
4.  **Refactoring safety** - move code freely (as long as you keep the same order)

The constraint (same order every render) turns out to be easy to follow in practice and catches real bugs. If your effect sometimes runs and sometimes doesn't, that's usually a design problem anyway.

# Applicability Beyond UI

This pattern isn't limited to UI frameworks. Any system where:

- Functions are called repeatedly with the same logical structure
- You need to maintain state between calls
- Explicit identification creates friction

Consider a test framework that needs to track assertions:

``` elisp
(defvar test--assertion-index 0)

(defun assert-equal (expected actual)
  (let ((id test--assertion-index))
    (cl-incf test--assertion-index)
    ;; Can now track: did assertion #3 pass this run vs last run?
    (record-assertion id expected actual (equal expected actual))))
```

Or a memoization system for expensive computations in a pipeline:

``` elisp
(defvar pipeline--memo-index 0)
(defvar pipeline--memos (make-hash-table))

(defun memo (deps computation)
  "Memoize COMPUTATION based on DEPS."
  (let* ((id pipeline--memo-index)
         (entry (gethash id pipeline--memos)))
    (cl-incf pipeline--memo-index)
    (if (and entry (equal (car entry) deps))
        (cdr entry)
      (let ((result (funcall computation)))
        (puthash id (cons deps result) pipeline--memos)
        result))))
```

The pattern is general: when you have a sequence of operations that repeats, use position as identity.

# Final Thoughts

Implicit identity through call order is one of those patterns that seems wrong until you use it. "What do you mean there's no key? How do you know which is which?" The answer is: by counting.

It's a beautiful example of turning a constraint (hooks must be called in order) into a feature (zero configuration, automatic namespacing, composability). The pattern emerged from React's design, but it's broadly applicable to any system with repeated structured calls.

Sometimes the best identifier is no identifier at all - just "you're the third one".
