Building interactive UIs in Emacs has traditionally been [painful](/posts/2025-11-30-building-complex-uis-in-emacs). You manage buffers manually, scatter state across variables, and wrestle with the cursor every time you redraw. [vui.el](https://github.com/d12frosted/vui.el) brings React-style declarative components to Emacs, letting you describe *what* your UI should look like rather than *how* to update it.

This tutorial gets you from zero to a working component in 15 minutes.

# Installation

vui.el isn't on MELPA yet. Clone the repository and add it to your load path:

``` elisp
(add-to-list 'load-path "/path/to/vui.el")
(require 'vui)
```

Or with `use-package` and elpaca:

``` elisp
(use-package vui
  :ensure (:host github :repo "d12frosted/vui.el"))
```

**Important**: vui.el requires lexical binding. When trying examples, either:

- Evaluate code in an `.el` file with `;;; -*- lexical-binding: t -*-` at the top
- Or ensure your buffer has lexical-binding enabled: `M-: (setq-local lexical-binding t)`

# Hello World

Let's start with the simplest possible component:

``` elisp
(defcomponent hello-world ()
  :render
  (vui-text "Hello, World!"))

;; Mount it
(vui-mount (vui-component 'hello-world))
```

Evaluate this and a buffer appears with "Hello, World!". Nothing to write home about yet, but notice the pattern:

1.  `defcomponent` defines a reusable component
2.  `:render` specifies what to display
3.  `vui-mount` creates a buffer and renders the component

# Adding Props

Components accept *props* - inputs passed from the parent:

``` elisp
(defcomponent greeter (name)
  :render
  (vui-text (format "Hello, %s!" name)))

(vui-mount (vui-component 'greeter :name "Emacs"))
```

Props are declared in the argument list after the component name. Pass them with keyword arguments when creating the component.

# Composing Components

Build complex UIs by combining simple components:

``` elisp
(defcomponent greeting-card (name title)
  :render
  (vui-vstack
   (vui-text title :face 'bold)
   (vui-newline)
   (vui-component 'greeter :name name)))

(vui-mount (vui-component 'greeting-card
                          :name "Reader"
                          :title "Welcome!"))
```

`vui-vstack` stacks children vertically. `vui-component` creates a child component. This is composition - small pieces combining into larger ones.

# State: Making Things Interactive

Static text isn't very useful. Let's add state:

``` elisp
(defcomponent counter ()
  :state ((count 0))
  :render
  (vui-hstack
   (vui-text (format "Count: %d" count))
   (vui-space)
   (vui-button "+"
     :on-click (lambda ()
                 (vui-set-state :count (1+ count))))))

(vui-mount (vui-component 'counter))
```

If you get an error that `count` is not defined, make sure you actually enabled lexical binding as was explained in the installation section above.

Key concepts:

- `:state` declares local state with initial values
- State variables (`count`) are available in `:render`
- `vui-set-state` updates state and triggers re-render
- The UI automatically reflects the new state

Click the button and watch the count increase. No manual buffer updates needed.

# User Input: Buttons and Fields

vui.el wraps Emacs widgets for interactive elements:

``` elisp
(defcomponent name-form ()
  :state ((name ""))
  :render
  (vui-vstack
   (vui-hstack
    (vui-text "Name: ")
    (vui-field
     :value name
     :size 20
     :on-change (lambda (value)
                  (vui-set-state :name value))))
   (vui-newline)
   (if (string-empty-p name)
       (vui-text "Enter your name above")
     (vui-text (format "Hello, %s!" name)))))

(vui-mount (vui-component 'name-form))
```

The `vui-field` widget:

- Displays an editable text field
- Calls `:on-change` whenever the user types
- The callback updates state, triggering re-render
- The greeting updates automatically

# Conditional Rendering

Notice the `if` in the previous example - that's conditional rendering. The render function is just Elisp, so use any control flow:

``` elisp
(defcomponent toggle-demo ()
  :state ((visible t))
  :render
  (vui-vstack
   (vui-button (if visible "Hide" "Show")
     :on-click (lambda ()
                 (vui-set-state :visible (not visible))))
   (when visible
     (vui-fragment
      (vui-newline)
      (vui-text "Now you see me!")))))

(vui-mount (vui-component 'toggle-demo))
```

`vui-fragment` groups multiple elements without adding visual structure - think of it like `progn` but for UI elements, letting you return several things where one is expected. Return `nil` to render nothing.

# Lists

Render dynamic lists with `vui-list`:

``` elisp
(defcomponent todo-list ()
  :state ((items '("Buy milk" "Write code" "Take a break")))
  :render
  (vui-vstack
   (vui-text "Todo:" :face 'bold)
   (vui-list items
     (lambda (item)
       (vui-text (format "- %s" item))))))

(vui-mount (vui-component 'todo-list))
```

`vui-list` takes:

- A list of data
- A function that renders each item
- Optional `:key-fn` for stable identity across re-renders

# Layout Primitives

vui.el provides several layout components:

``` elisp
(defcomponent layout-demo ()
  :render
  (vui-vstack
   ;; Horizontal stack with spacing
   (vui-hstack
    :spacing 2
    (vui-button "One")
    (vui-button "Two")
    (vui-button "Three"))

   (vui-newline)

   ;; Box with fixed width and alignment
   (vui-box
    (vui-text "Centered!")
    :width 40
    :align :center)

   (vui-newline)

   ;; Table for structured data
   (vui-table
    :columns '((:width 15) (:width 10 :align :right))
    :rows '(("Item" "Price")
            ("Coffee" "$3.50")
            ("Sandwich" "$8.00"))
    :border :ascii)))

(vui-mount (vui-component 'layout-demo))
```

# Putting It Together

Here's a slightly more complete example - a simple task tracker:

``` elisp
(defcomponent task-item (task on-toggle on-delete)
  :render
  (vui-hstack
   (vui-checkbox (plist-get task :done)
     :on-change (lambda (_) (funcall on-toggle)))
   (vui-space)
   (vui-button "x"
     :on-click on-delete)
   (vui-space)
   (vui-text (plist-get task :text)
     :face (if (plist-get task :done) 'shadow nil))))

(defcomponent task-tracker ()
  :state ((tasks '((:id 1 :text "Learn vui.el" :done nil)
                   (:id 2 :text "Build something cool" :done nil)))
          (next-id 3)
          (new-task ""))
  :render
  (vui-vstack
   (vui-text "Task Tracker" :face 'org-level-1)
   (vui-newline)

   ;; Add new task
   (vui-hstack
    (vui-field
     :value new-task
     :size 30
     :on-change (lambda (v) (vui-set-state :new-task v)))
    (vui-space)
    (vui-button "Add"
      :on-click (lambda ()
                  (unless (string-empty-p new-task)
                    (vui-set-state :tasks
                      (append tasks
                              (list (list :id next-id
                                          :text new-task
                                          :done nil))))
                    (vui-set-state :next-id (1+ next-id))
                    (vui-set-state :new-task "")))))

   (vui-newline)

   ;; Task list
   (vui-list tasks
     (lambda (task)
       (vui-component 'task-item
         :task task
         :on-toggle (lambda ()
                      (vui-set-state :tasks
                        (mapcar (lambda (it)
                                  (if (= (plist-get it :id) (plist-get task :id))
                                      (plist-put (copy-sequence it) :done
                                                 (not (plist-get it :done)))
                                    it))
                                tasks)))
         :on-delete (lambda ()
                      (vui-set-state :tasks
                        (seq-remove (lambda (it)
                                      (= (plist-get it :id) (plist-get task :id)))
                                    tasks)))))
     (lambda (task) (plist-get task :id)))))

(vui-mount (vui-component 'task-tracker))
```

This demonstrates:

- Parent/child component communication via callback props
- List rendering with proper keys
- Multiple state updates
- Conditional styling based on state

# What's Next?

You now know the basics:

- Components with `defcomponent`
- Props for inputs, state for local data
- `vui-set-state` triggers re-renders
- Composition builds complex UIs from simple pieces
- Layout primitives for structure

You might be wondering how vui.el handles re-rendering efficiently under the hood. We'll cover that in an upcoming article, along with building a more realistic application step by step to show how to structure larger UIs with multiple components working together.

# Quick Reference

| Concept          | Syntax                                               |
|------------------|------------------------------------------------------|
| Define component | `(defcomponent name (props) :state ... :render ...)` |
| Create component | `(vui-component 'name :prop value)`                  |
| Mount to buffer  | `(vui-mount component)`                              |
| Update state     | `(vui-set-state :key value)`                         |
| Text             | `(vui-text "string" :face 'face)`                    |
| Button           | `(vui-button "label" :on-click fn)`                  |
| Field            | `(vui-field :value v :on-change fn)`                 |
| Checkbox         | `(vui-checkbox checked :on-change fn)`               |
| Vertical stack   | `(vui-vstack child1 child2 ...)`                     |
| Horizontal stack | `(vui-hstack child1 child2 ... :spacing n)`          |
| List             | `(vui-list items render-fn key-fn)`                  |
| Fragment         | `(vui-fragment child1 child2 ...)`                   |
| Newline          | `(vui-newline)`                                      |
| Space            | `(vui-space :width n)`                               |
