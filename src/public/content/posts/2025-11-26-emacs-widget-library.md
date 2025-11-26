The Emacs widget library (`widget.el` and `wid-edit.el`) has been part of Emacs since 1996, when Per Abrahamsen wrote it to power the Customize interface. Nearly three decades later, it remains the foundation for `M-x customize` and appears in various packages that need form-like interfaces. It's also largely unchanged, rarely discussed, and - when you actually try to build something non-trivial with it - surprisingly painful to work with.

This post is a critique born from experience. I've built complex UIs using the widget library, including a table widget with editable cells that reflows dynamically. The code lives in [widget-extra](https://github.com/d12frosted/widget-extra), a library I wrote to extend the built-in widget system. It works. It was hard. The process revealed both the library's hidden power and its fundamental limitations.

# What It Does Well

Before the critique, credit where it's due.

## Deep Integration with Emacs

Widgets are text. They live in buffers, use overlays and text properties, and work identically in GUI and terminal Emacs. This is philosophically aligned with Emacs's core principle: everything is a buffer. You can use standard navigation, search the buffer, even run keyboard macros across widget forms.

## Performance

A buffer with hundreds of widgets remains snappy. There are no heavy GUI objects, no separate rendering pipeline - just text with properties. The Customize interface, with its deeply nested groups and countless options, demonstrates this well.

## Type Hierarchy

The library excels at defining *what* widgets are. You can create new widget types that inherit from existing ones, override specific behaviours, and build a taxonomy of components. A `bounded-int-field` can inherit from `int-field` which inherits from `field` which inherits from `default`. This is genuinely powerful for building families of related widgets.

That said, this is a classical inheritance approach, and the game development community moved away from deep inheritance hierarchies years ago. The Entity-Component-System pattern, popularised by Unity and others, favours composition over inheritance: instead of an entity *being* a subclass of multiple base classes, it *has* components that define its behaviours. An entity with physics, visuals, and AI isn't a `PhysicsVisualAIEntity` subclass - it's just an entity with three components attached.

The widget library's type hierarchy works well when your widgets fit neatly into an "is-a" relationship. It becomes awkward when you need a widget that combines multiple orthogonal behaviours - editable, validated, formatted, linked to external state. You end up either creating deep hierarchies or manually composing behaviours through property combinations. Not a fatal flaw, but worth noting that the approach shows its age.

## A Reasonable Set of Primitives

The library provides what you'd expect: links, buttons, editable fields, checkboxes, radio buttons, dropdown menus, and editable lists. For a simple configuration screen or a linear questionnaire, widgets work fine.

# Where It Falls Apart

A confession before I start criticising: I might be wrong about some of this.

The documentation didn't work for me. The code was hard to navigate. I spent a lot of time confused. If there are better patterns I missed, I'd genuinely like to know - leave a comment or reach out. I'll happily update this post with corrections.

Also, I should mention: I'm primarily a server developer. I know little about building UIs. This might explain why I kept banging my head against walls that UI people would have walked around. On the other hand, it also means I approached the library without preconceptions, which occasionally has value. Make of that what you will.

## Hierarchy Without Layout

Here's the core confusion: the widget library is excellent at defining widget *types* (the "what") but offers almost nothing for widget *layout* (the "where").

When you define a new widget type, you're specifying its behaviour, validation, appearance, and relationship to other types. This is well-supported. But when you want to arrange widgets spatially - put these three in a row, align those labels, create a grid - you're on your own.

The library's composition primitives are minimal and poorly explained. You can nest widgets inside other widgets, but there's no layout engine. You insert text and widgets sequentially into a buffer, calculating positions manually. Want columns? Count characters. Want alignment? Pad with spaces. Want reflow when content changes? Rebuild everything.

This confusion between type hierarchy and spatial composition is never clearly addressed in the documentation, leaving developers to discover it painfully.

## No State Management

This is the fundamental architectural gap.

Modern UI development has converged on patterns for managing state: unidirectional data flow, reactive bindings, declarative state containers. The widget library offers none of this. When widget A's action needs to update widget B, you must:

1.  Store references to both widgets in buffer-local variables
2.  Write a `:notify` callback on widget A
3.  Manually call `widget-value-set` on widget B
4.  Call `widget-setup` to re-enable editing
5.  Hope you haven't broken anything

For a form with three interdependent fields, this is tedious. For a form with twenty, it's a maintenance nightmare. There's no concept of derived state, no way to declare "this widget's options depend on that widget's value," no subscription mechanism. Everything is imperative side effects, manually threaded through callback functions.

## No Widget Tree

Most UI toolkits provide a parent-child hierarchy. This gives you automatic layout propagation, event bubbling, scoped state, and declarative nesting.

The widget library is flat. Widgets are inserted into a buffer sequentially. Yes, composite widgets like `editable-list` have a `:parent` property for their items, but this isn't a general-purpose tree. You cannot nest arbitrary widgets inside a container and treat them as a unit.

## The Simplicity Paradox

Here's the irony: the widget library's performance comes *from* the same architectural simplicity that makes it hard to use - not *despite* it.

Widgets are just text with properties. No widget tree means no tree traversal overhead. No reactive state system means no dependency tracking cost. No layout engine means no layout calculations. The buffer *is* the UI, rendered by Emacs's extremely optimised text display machinery.

This is genuinely elegant for the Customize interface, where performance matters and the UI is fundamentally linear. The simplicity is a feature when your requirements match the design.

But that same simplicity becomes a burden when you want interdependent widgets, spatial layouts, or dynamic composition.

# Case Study: Building Layout Widgets

Enough critique. Let's build something and see what we learn.

(This is the part where a proper UI developer would probably reach for an existing solution. As a server developer with more arrogance than sense, I naturally decided to implement everything from scratch.)

All the code shown here is available in [widget-extra](https://github.com/d12frosted/widget-extra), a library I wrote to extend the built-in widget system with additional components: labels, fields, buttons, and layout widgets. You can use it directly or study it as a reference.

## Warm-up: A Fields Group with Aligned Tags

Before tackling tables, let's solve a simpler problem: displaying multiple fields with their tags aligned.

``` example
Name:   Boris
Age:    30
Email:  boris@example.com
```

The challenge: each field has a tag of different length, but we want the values to line up. The widget library provides no alignment primitives, so we calculate padding manually.

``` elisp
(define-widget 'fields-group 'default
  "Group multiple fields with automatic tag alignment."
  :convert-widget #'widget-types-convert-widget
  :copy #'widget-types-copy
  :format "%v"
  :extra-offset 1
  :value-create #'widget-fields-group-value-create)
```

The `:value-create` function measures all tags, finds the maximum length, then adds appropriate padding to each field:

``` elisp
(defun widget-fields-group-value-create (widget)
  "Create children with aligned tags."
  (let* ((args (widget-get widget :args))
         (max-tag-length (seq-max
                          (seq-map
                           (lambda (x) (length (or (widget-get x :tag) "")))
                           args))))
    (dolist (arg args)
      (widget-fields-group-add-item widget arg max-tag-length))))

(defun widget-fields-group-add-item (widget item max-tag-length)
  "Add ITEM to WIDGET with padding based on MAX-TAG-LENGTH."
  (let* ((tag (widget-get item :tag))
         (tag-length (if tag (length tag) 0))
         (offset (+ (widget-get widget :extra-offset)
                    (- max-tag-length tag-length)))
         (format (or (widget-get item :format) "%T%[%v%]"))
         (format (if (s-ends-with-p "\n" format)
                     format
                   (concat format "\n"))))
    (widget-put item :format format)
    (widget-put item :offset offset)
    (widget-create-child widget item)))
```

Usage:

``` elisp
(widget-create
 'fields-group
 (list 'field :tag "Name:" :value "Boris")
 (list 'int-field :tag "Age:" :value 30)
 (list 'field :tag "Email:" :value "boris@example.com"))
```

This is the pattern: measure first, then render with calculated offsets. No layout engine - just arithmetic and string padding.

Note also that we're modifying each child's `:offset` property before creation. The base `field` widget (also defined in `widget-extra`) supports a custom `%T` format escape that renders the tag with configurable spacing. This kind of cooperation between parent and child widgets requires planning the property protocol in advance.

## The Hard Part: A Table with Editable Cells

Now let's tackle something genuinely difficult.

A table with editable cells sounds simple: rows and columns, maybe some separators, widgets in each cell. But the requirements quickly compound:

1.  Columns must align - cells in the same column should have equal width
2.  When a cell's value changes length, the column must resize
3.  When the table redraws, the cursor must stay in the same logical position
4.  The whole table should be a single widget that can be created and manipulated atomically

None of this is provided. All of it is possible.

### Step 1: Define the Structure

A table takes rows as arguments. Each row is either a horizontal line or a data row containing widgets:

``` elisp
(define-widget 'table 'default
  "A table widget with rows, columns, and separators."
  :convert-widget #'widget-types-convert-widget
  :copy #'widget-types-copy
  :format "%v"
  :row-conj " | "
  :hline-conj "-+-"
  :hline-content ?-
  :padding ?\s
  :value-create #'widget-table-value-create
  :notify #'widget-table-notify)
```

Usage will look like:

``` elisp
(widget-create
 'table
 '(row (label :value "Name") (label :value "Age"))
 '(hline)
 '(row (field :value "Boris") (int-field :value 30)))
```

The `:value-create` function does the heavy lifting. The `:notify` function handles state changes.

### Step 2: Calculate Column Widths

Here's the first hack: to know how wide each column should be, we need to know how wide each cell's content is. But widgets don't have a "width" property - they're just text that gets inserted.

Solution: create each widget in a temporary buffer, measure the resulting text, then discard it:

``` elisp
(let* ((args (widget-get widget :args))
       (cols (apply #'max (mapcar (lambda (row)
                                    (length (widget-get row :args)))
                                  args)))
       (widths
        (->> (-iota cols)
             ;; Transpose: group by column instead of row
             (-map (lambda (i)
                     (-map (-partial #'nth i)
                           (--map (widget-get it :args) args))))
             ;; Measure each cell
             (--map-indexed
              (--map (when it
                       (with-temp-buffer
                         (widget-create it)
                         (- (point) 1)))
                     it)))))
  ;; widths is now a list of lists: ((col0-row0 col0-row1 ...) (col1-row0 ...))
  ...)
```

This is expensive - we create every widget twice. But it works, and for reasonable table sizes, it's fast enough.

### Step 3: Render with Padding

Now we iterate through rows, rendering each cell with appropriate padding:

``` elisp
(defun widget-table-value-create (widget)
  (let* ((args (widget-get widget :args))
         (widths (widget-table--calculate-widths widget))
         (max-widths (-map #'-max widths))
         (children))
    (-each-indexed args
      (lambda (row-index row)
        (pcase (car row)
          (`row
           (widget-insert (widget-get widget :row-start))
           (-each-indexed (widget-get row :args)
             (lambda (col-index col)
               (unless (= 0 col-index)
                 (widget-insert (widget-get widget :row-conj)))
               (let* ((w (nth row-index (nth col-index widths)))
                      (mw (nth col-index max-widths))
                      (pad (- mw w))
                      (child (widget-create-child widget col)))
                 ;; Track position for state management
                 (widget-put child :row-index row-index)
                 (widget-put child :col-index col-index)
                 (push child children)
                 ;; Add padding to align columns
                 (when (> pad 0)
                   (widget-insert (make-string pad ?\s))))))
           (widget-insert "\n"))

          (`hline
           ;; Draw separator line
           (--each (-iota (length max-widths))
             (unless (= it 0)
               (widget-insert (widget-get widget :hline-conj)))
             (widget-insert
              (make-string (nth it max-widths) ?-)))
           (widget-insert "\n")))))

    (widget-put widget :children (reverse children))))
```

The key insight: we store `:row-index` and `:col-index` on each child widget. This lets us find them again after a redraw.

### Step 4: Handle State Changes (The Hard Part)

When a cell's value changes, we need to:

1.  Update our internal representation (`:args`)
2.  Redraw the entire table (column widths may have changed)
3.  Put the cursor back where it was

The `:notify` callback receives the child widget that changed:

``` elisp
(defun widget-table-notify (widget child &optional _event)
  (let* ((row-index (widget-get child :row-index))
         (col-index (widget-get child :col-index))
         ;; Remember cursor position relative to widget start
         (child-from (marker-position (widget-get child :from)))
         (delta (when child-from (- (point) child-from)))
         (new-value (widget-value child)))

    ;; Update the spec in :args with new value
    (let* ((row (nth row-index (widget-get widget :args)))
           (original-spec (nth col-index (widget-get row :args)))
           (updated-spec (widget-table--update-spec-value
                          original-spec new-value)))
      (widget-put widget :args
                  (--update-at row-index
                               (progn
                                 (widget-put it :args
                                             (-replace-at col-index
                                                          updated-spec
                                                          (widget-get it :args)))
                                 it)
                               (widget-get widget :args))))

    ;; Redraw the entire table
    (widget-default-value-set widget (widget-get widget :value))

    ;; Restore cursor position
    (when-let ((child (--find (and (= row-index (widget-get it :row-index))
                                   (= col-index (widget-get it :col-index)))
                              (widget-get widget :children))))
      (when delta
        (goto-char (+ (widget-get child :from) delta))))))
```

This is the critical piece. We:

1.  Capture the cursor's offset from the widget's start *before* redrawing
2.  Modify `:args` to reflect the new value
3.  Trigger a full redraw via `widget-default-value-set`
4.  Find the same cell again by row/column indices
5.  Restore the cursor to the same offset

Without step 5, editing would be maddening - every keystroke would jump the cursor somewhere unexpected.

### Step 5: Update Specs Without Corruption

One subtle bug: widget specs in `:args` are often shared structures. If you modify them directly, you corrupt the original definitions. Deep copy is essential:

``` elisp
(defun widget-table--update-spec-value (spec new-value)
  "Return a copy of widget SPEC with :value set to NEW-VALUE."
  (let ((copy (copy-tree spec)))
    (if (plist-member (cdr copy) :value)
        (plist-put (cdr copy) :value new-value)
      (setcdr copy (cons :value (cons new-value (cdr copy)))))
    ;; Special case: menu-choice needs :tag updated too
    (when (eq (car copy) 'menu-choice)
      (plist-put (cdr copy) :tag new-value))
    copy))
```

## The Result

After all this, we have a table that:

- Aligns columns automatically
- Reflows when cell content changes
- Preserves cursor position through redraws
- Works with various widget types as cells

``` elisp
(widget-create
 'table
 '(row (label :value "Name") (label :value "Score"))
 '(hline)
 '(row (field :value "Alice") (int-field :value 95))
 '(row (field :value "Bob") (int-field :value 87)))
```

Renders as:

``` example
Name  | Score
------+------
Alice |    95
Bob   |    87
```

Edit "Alice" to "Alexandria" and watch the first column widen. The cursor stays in the cell you were editing.

## What This Teaches Us

Building this table required:

- **Measuring widgets by creating them in temporary buffers** - there's no introspection API for "how wide would this be?"
- **Manual coordinate tracking** - storing row/column indices because there's no widget tree to traverse
- **Full redraw on any change** - no incremental updates, no dirty-region tracking
- **Cursor position surgery** - capturing and restoring offsets because the library doesn't preserve context through redraws
- **Deep copying specs** - because shared structures will bite you

None of this is documented. All of it is discoverable only by building something and hitting walls.

And yet - it works. The underlying primitives (text properties, overlays, markers) are solid. The performance is good. You can build sophisticated UIs if you're willing to pay the complexity tax.

# The Modern Landscape

For keyboard-driven command menus, Transient (from Magit) has become the standard. It's well-documented, actively maintained, and designed around a coherent model of transient state.

For complex interactive UIs, there isn't a clear answer. The widget library occupies an awkward middle ground: too complex for simple needs, too limited for complex ones.

# What's Next

While building Emacs tools for [Barberry Garden](https://barberry.io) - my wine tasting management system - I've been pushing the widget library to its limits. The [brb](https://github.com/d12frosted/brb) package includes event planning interfaces, tasting score entry forms, and various administrative views. Tables with editable cells. Dynamic forms that reconfigure based on selections. Nested groups that expand and collapse.

It works, but the friction is constant. Every feature requires fighting the architecture. The cognitive overhead of manual state management, cursor preservation, and layout calculation adds up.

So I've started designing something new: a UI layer that uses `widget.el` under the hood but provides higher-level abstractions. Not a full reactive framework - Emacs doesn't need that complexity - but a thin system that handles:

- **Declarative composition**: describe what you want, not how to build it
- **Automatic state propagation**: when this changes, update that
- **Cursor-aware redraws**: preserve editing context through updates
- **Layout primitives**: rows, columns, groups that just work

The goal isn't to replace `widget.el` but to tame it. Keep the performance, hide the ceremony. Respect the fundamental constraints of Emacs UI - the criticality of cursor position, the two-dimensional nature of the buffer. We don't have a proper DOM and CSS, and that's actually fine.

No promises on timeline, but I may share the design document soon. One more React-inspired UI library for Emacs? Perhaps. But sometimes you need to build the tools that let you build what you actually want.

In the meantime, [widget-extra](https://github.com/d12frosted/widget-extra) is available and working. The widgets described in this post - labels, fields, buttons, `fields-group`, `table`, and more - are all there. Use it if it helps. Study it if you're curious. And if I do build the new UI system, `widget-extra` will likely be superseded - but until then, it's a reasonable way to build widget-based interfaces without starting from scratch.

# Conclusion

The Emacs widget library is more powerful than its documentation suggests and more painful than it should be. The type hierarchy is genuinely elegant. The layout story is essentially absent. State management is your problem.

Its performance comes from simplicity - the same simplicity that makes complex UIs difficult. There's no free lunch.

If you're building something simple, widgets work fine. If you're building something complex, budget time for archaeology. Read the source. Build small experiments. Accept that cursor position preservation will haunt your dreams.

I hope this post serves as a useful introduction to what you're getting into with `widget.el` - and perhaps [widget-extra](https://github.com/d12frosted/widget-extra) can save you some of the pain I went through. The library isn't comprehensive documentation of the widget system (that would require a book), but between this walkthrough and the source code, you should have enough to get started.

And when you hit walls - because you will - know that you're not alone. You're joining a long tradition of Emacs hackers who got their widgets working and then immediately wanted to forget everything about `wid-edit.el`.
