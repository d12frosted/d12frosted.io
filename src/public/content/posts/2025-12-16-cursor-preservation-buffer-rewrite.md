Here's a fun thing about building UIs in Emacs: everything is a text buffer. Your fancy interactive interface with buttons and text fields? It's built by inserting text. The cursor is just a position in that text - and it moves as you insert. When you need to update the UI, you erase the buffer and insert new text, character by character, the cursor dutifully advancing with each insertion.

You see where this is going.

You're editing a cell in a table. You change a value, hit `RET`. The UI redraws to reflect your change. The buffer is erased and recreated. The cursor, which was in the cell you just edited, is now at position 1 - the beginning of the buffer. Or somewhere in the middle of an unrelated field. Or, if you're lucky, in roughly the right place but offset by a few characters because a column above got wider.

This is the kind of bug that makes users describe your software as "janky" without being able to articulate why. Everything *works*, technically. It just feels like the floor keeps shifting under your feet.

I've been running into this for years. I have various tools in Emacs for managing wine tastings and my cellar - interactive buffers with editable fields, buttons, dynamic content. Every time I built one, I'd solve the cursor problem ad-hoc, slightly differently, never quite right. Eventually I got tired of it and built [vui.el](https://github.com/d12frosted/vui.el), a React-inspired UI framework, partly so I'd never have to think about cursor positioning again.

The solution looks obvious in retrospect. It wasn't. It took me several iterations, each one failing in a different way, before landing on something robust. This article walks through that progression - not because the early attempts are interesting in themselves, but because understanding *why* they fail illuminates the real problem.

# Attempt 1: Save and Restore Point

The first instinct is to save `point` before erasing:

``` elisp
(defun redraw-buffer ()
  (let ((saved-point (point)))
    (erase-buffer)
    (insert-new-content)
    (goto-char (min saved-point (point-max)))))
```

This works when content doesn't change much. But consider:

``` text
Before:                   After:
Line 1                    Line 1
Line 2 [cursor here]      New line inserted
Line 3                    Line 2 [cursor should be here]
                          Line 3
```

If the cursor was at position 15 (middle of "Line 2"), after redraw it's now in the middle of the new line. The user was focused on "Line 2" but the cursor landed somewhere else entirely.

The problem: absolute positions are meaningless when content changes.

# Why Not `save-excursion`?

The experienced Emacs user's next instinct: "just wrap it in `save-excursion`!"

``` elisp
(save-excursion
  (erase-buffer)
  (insert-new-content))
;; cursor magically restored?
```

Nope. `save-excursion` saves point as a marker - and markers don't survive `erase-buffer`. After the buffer is erased, the saved position points to `point-min`. The excursion faithfully restores… nothing useful.

You can verify this yourself:

``` elisp
(with-temp-buffer
  (insert "hello world")
  (goto-char 7)  ; after "hello "
  (save-excursion
    (erase-buffer)
    (insert "goodbye"))
  (point))  ; => 1, not 7
```

Same problem with raw markers. They track positions in the *existing* buffer. When you erase and rewrite, the marker's position becomes meaningless. Markers are great for tracking positions during incremental edits, not across full rewrites.

# Attempt 2: Widget Index

Since absolute positions don't work, what about relative positions? In a UI with interactive widgets (buttons, text fields), the widgets are stable landmarks. Even if their buffer positions shift, their identity persists.

If you've read [Implicit Identity Through Call Order](/posts/2025-12-14-implicit-identity-call-order), you might recognise an opportunity here. We learned that call order during execution can serve as implicit identity - "you're the 3rd hook called". Maybe we can do the same with widgets: "you're the 3rd widget in the buffer".

The idea: track which widget the cursor is in (by index), and the offset within that widget.

``` elisp
(defun vui--save-cursor-position ()
  "Save cursor position relative to current widget."
  (let ((widget (widget-at (point))))
    (if widget
        (let* ((bounds (vui--widget-bounds widget))
               (start (car bounds))
               (offset (- (point) start))
               (index (vui--widget-index widget)))
          (cons index offset))
      ;; No widget - fall back to line/column
      (cons nil (cons (line-number-at-pos) (current-column))))))

(defun vui--widget-index (widget)
  "Get index of WIDGET among all widgets in buffer."
  (let ((widgets (vui--collect-widgets))
        (idx 0))
    (catch 'found
      (dolist (w widgets)
        (when (eq w widget)
          (throw 'found idx))
        (cl-incf idx))
      nil)))
```

To restore, find the widget at the same index and apply the offset:

``` elisp
(defun vui--restore-cursor-position (info)
  "Restore cursor from INFO."
  (let ((index (car info))
        (offset (cdr info)))
    (when index
      (let* ((widgets (vui--collect-widgets))
             (widget (nth index widgets))
             (bounds (vui--widget-bounds widget))
             (start (car bounds))
             (end (cdr bounds)))
        (goto-char (max start (min (+ start offset) (1- end))))))))
```

This works! The cursor stays in the right widget even when content changes. Ship it.

…until someone adds a button.

``` text
Before clicking [Add]:          After clicking [Add]:
Button count: 0                 Button count: 1
[Add] ← cursor here             [Button 1] ← cursor ends up here!
                                [Add] ← should be here
```

The `[Add]` button was at index 0. After clicking, `[Button 1]` is inserted before it. Now `[Add]` is at index 1, but we saved index 0. The cursor lands on the wrong widget.

Index-based tracking fails whenever widgets are added or removed before the cursor position.

Remember why the counting trick works for hooks? The "Rules of Hooks" constraint: hooks must be called unconditionally, in the same order every render. That's a reasonable restriction for hooks - if your effect sometimes runs and sometimes doesn't, that's usually a design problem.

But for UIs? A good UI library *must* support dynamic content. Lists grow and shrink. Conditionals toggle visibility. Widgets appear and disappear based on state. Forbidding this would be absurd. So the invariant that makes index-based identity work for hooks simply doesn't hold for UI widgets.

We need something more robust.

# The Insight: UI as a Tree

A UI is conceptually a tree. You have containers holding containers holding widgets:

``` text
Root (vstack)
├── Text "Button count: 1"
├── Inner vstack
│   └── Button "Button 1"
└── Button "Add"
```

But the cursor is always in a *leaf*. You can't position the cursor "in a container" - only in the actual rendered text of a specific widget. So finding where the cursor is means finding which leaf contains it.

The index approach flattens this tree into a linear sequence: "you're in widget \#0". But that loses structural information. When a sibling is added to a different branch, your index changes even though your *position in the tree* hasn't.

What if we tracked the path instead?

# Attempt 3: Tree Path

Instead of "widget \#0", track "child 2 of child 0 of root" - a path through the tree.

``` text
Root (vstack)               Path from root:
├── Text "Button count: 1"  (0)
├── Inner vstack            (1)
│   └── Button "Button 1"   (1 0)
└── Button "Add"            (2)
```

Now when `[Button 1]` is added:

- `[Add]` is still at path `(2)` - child 2 of root
- `[Button 1]` is at path `(1 0)` - child 0 of the inner vstack at child 1

The `[Add]` button's path doesn't change when siblings are added to a different branch. Path-based tracking is robust to structural changes elsewhere in the tree.

## Tracking Paths During Render

The key is to build paths as we render. Each container tracks its position and propagates to children:

``` elisp
(defvar vui--render-path nil
  "Current path from root during rendering. List of indices.")

(defun vui--render-children (children)
  "Render CHILDREN, tracking path for each."
  (let ((idx 0))
    (dolist (child children)
      (let ((vui--render-path (cons idx vui--render-path)))
        (vui--render-vnode child))
      (cl-incf idx))))
```

When creating a widget, capture the current path:

``` elisp
;; Inside button rendering
(let* ((captured-path (reverse vui--render-path))
       (w (widget-create 'push-button ...)))
  (widget-put w :vui-path captured-path))
```

The path is built as a stack (cons at front) during traversal, then reversed when captured to get root-first order.

## Saving and Restoring

Save now includes the path:

``` elisp
(defun vui--save-cursor-position ()
  "Save cursor position relative to current widget."
  (let ((widget (widget-at (point)))
        (pos (point)))
    (if widget
        (let* ((bounds (vui--widget-bounds widget))
               (start (car bounds))
               (offset (if start (- pos start) 0))
               (path (widget-get widget :vui-path))
               (index (vui--widget-index widget)))
          (list :path path :index index :offset offset))
      (list :line (line-number-at-pos) :column (current-column)))))
```

Restore tries path first, falls back to index:

``` elisp
(defun vui--find-widget-by-path (path)
  "Find widget with matching :vui-path."
  (when path
    (catch 'found
      (dolist (w (vui--collect-widgets))
        (when (equal (widget-get w :vui-path) path)
          (throw 'found w)))
      nil)))

(defun vui--restore-cursor-position (info)
  "Restore cursor from INFO."
  (let* ((path (plist-get info :path))
         (index (plist-get info :index))
         (offset (plist-get info :offset))
         (line (plist-get info :line))
         (column (plist-get info :column))
         (widget (or (vui--find-widget-by-path path)
                     (nth index (vui--collect-widgets)))))
    (cond
     (widget
      (let* ((bounds (vui--widget-bounds widget))
             (start (car bounds))
             (end (cdr bounds)))
        (when (and start end)
          (goto-char (max start (min (+ start offset) (1- end)))))))
     ((and line column)
      (goto-char (point-min))
      (forward-line (1- line))
      (move-to-column column))
     (t
      (goto-char (point-min))))))
```

The fallback chain: path match → index match → line/column → point-min.

# What This Doesn't Solve

Path-based tracking is more robust than index-based, but it's not perfect. It handles:

- Widgets added/removed in other branches ✓
- Content changes within widgets ✓
- Widget type changing at the same path ✓

It doesn't handle:

- Widgets added/removed in the same container before your position
- Complete restructuring of the tree
- The path itself disappearing

``` text
Before:                    After:
vstack                     vstack
├── Button A  (0)          ├── Button NEW  (0)
├── Button B  (1) ←cursor  ├── Button A    (1) ← cursor lands here
└── Button C  (2)          ├── Button B    (2) ← should be here (?)
                           └── Button C    (3)
```

If a widget is inserted at index 0 in the same container, paths shift: Button B moves from `(1)` to `(2)`. We saved `(1)`, so cursor lands on Button A (now at path `(1)`).

This is fundamental. Without explicit widget identity (like React's `key` prop), the framework can't distinguish "Button B moved" from "Button A changed its label". It sees structure, not semantics.

# DWIM and the Limits of Automation

In Emacs, there's a philosophy called DWIM - "Do What I Mean". The editor should guess your intent and act accordingly. But DWIM has limits. Sometimes there's no way to know what you mean.

A UI framework sees widgets and structure. It doesn't know that `[Add]` is "the button the user was about to click" or that `[Button 1]` is "a dynamically generated button they don't care about". When structure changes, the framework guesses. Sometimes it guesses wrong.

This isn't a bug - it's a fundamental limitation. The framework provides best-effort cursor preservation within the constraints of what it can know.

## Designing for Stable Cursors

If you're building UIs with vui.el (or similar frameworks), you can help the framework help you:

1.  **Isolate dynamic content.** Wrap lists of dynamic widgets in their own container. Changes inside won't affect sibling paths.

2.  **Append, don't prepend.** Add new items at the end of lists when possible. Existing paths stay stable.

3.  **Consider the user's focus.** If an action creates something new, maybe the cursor *should* move to it. Don't fight the framework when its guess is reasonable.

The cursor preservation system handles the common case - content updates within a stable structure. For the edge cases, thoughtful UI design matters more than clever algorithms.

# Window Scroll Position

Cursor isn't the only thing to preserve. If the buffer is displayed in multiple windows, each window has its own scroll position:

``` elisp
(defun vui--save-window-starts ()
  "Save window-start for all windows showing current buffer."
  (let ((starts nil)
        (buf (current-buffer)))
    (dolist (win (get-buffer-window-list buf nil t))
      (push (cons win (window-start win)) starts))
    starts))

(defun vui--restore-window-starts (starts)
  "Restore window-start positions from STARTS."
  (dolist (entry starts)
    (let ((win (car entry))
          (start (cdr entry)))
      (when (window-live-p win)
        (set-window-start win (min start (point-max)))))))
```

This prevents jarring scroll jumps when content at the top changes.

# Putting It Together

The full redraw sequence:

``` elisp
(defun vui--rerender-buffer ()
  "Re-render the buffer, preserving cursor and scroll."
  (let ((cursor-info (vui--save-cursor-position))
        (window-starts (vui--save-window-starts))
        (vui--render-path nil))  ; Initialize path tracking
    (let ((inhibit-read-only t))
      (erase-buffer)
      (render-new-content))
    (vui--restore-cursor-position cursor-info)
    (vui--restore-window-starts window-starts)))
```

# The Linearisation Connection

Both this article and [Implicit Identity Through Call Order](/posts/implicit-identity-call-order) deal with the same fundamental problem: establishing identity across repeated operations without explicit keys. Both exploit the fact that a tree traversed in consistent order produces a stable sequence.

The difference is in how much of that sequence we track. Hooks can use a single index because the Rules of Hooks guarantee the sequence length is stable. For UI widgets, where the sequence can change, we need the full path - enough structural information to survive changes in other branches, even if we can't survive changes in our own.

# Final Thoughts

Cursor preservation seems like a small detail, but it's the difference between a UI that feels solid and one that feels like it's fighting you. Every time you type in a field and the cursor stays put, that's this code working invisibly.

The progression from naive (save point) to index-based to path-based mirrors a common pattern in software: each solution works until you find the case it doesn't handle, and the fix requires a deeper understanding of the problem.

The path-based approach isn't perfect. But it's honest about its limitations, graceful in its fallbacks, and robust enough for real-world use. Sometimes that's the best you can do.
