# Introduction

## Motivation

Emacs's built-in `widget.el` library provides a foundation for creating interactive UI elements in buffers. However, it has limitations when building complex, dynamic interfaces:

- **Imperative updates**: Widgets are created and modified imperatively, making it hard to reason about UI state.
- **No diffing**: Changes require manual deletion and recreation of widgets.
- **Coupled state**: Widget state is mixed with rendering logic.
- **Limited composition**: Composing widgets into complex layouts is cumbersome.

This library aims to bring React's declarative, component-based model to Emacs, enabling developers to build complex UIs by describing *what* should be rendered, not *how* to update it.

## Core Philosophy

1.  **Declarative**: Describe the UI as a function of state, not as a sequence of mutations.
2.  **Component-based**: Build complex UIs from small, reusable, composable pieces.
3.  **Unidirectional data flow**: Data flows down through props; events flow up through callbacks.
4.  **Predictable**: Same props + state always produces the same output.
5.  **Emacs-native**: Respect Emacs conventions (point, markers, keymaps, faces).

## Key Differences from React

| Aspect | React | This Library |
|----|----|----|
| Render target | DOM (tree of nodes) | Buffer (linear text with overlays) |
| User interaction | Mouse/keyboard events | Point position + keyboard |
| Focus model | Explicit focus management | Point is always "focused" |
| Layout | CSS (flexbox, grid) | Character-based (columns, rows) |
| Async | Concurrent rendering | Single-threaded, idle-time batching |

# Goals and Non-Goals

## Goals

- **Declarative component model** with props, state, and render functions
- **Efficient reconciliation** that minimises buffer mutations
- **Cursor preservation** across re-renders (critical for Emacs UX)
- **Composition primitives** for horizontal, vertical, and tabular layouts
- **Reactive state management** with dependency tracking
- **Context system** for deep data passing without prop drilling
- **Lifecycle hooks** for mount, unmount, and update events
- **Batched updates** to avoid redundant re-renders
- **Error boundaries** for graceful failure handling
- **Interoperability** with existing Emacs facilities (keymaps, faces, hooks)

## Non-Goals

- **Full CSS-like layout**: We work within buffer constraints (monospace, linear text)
- **Animation**: No built-in animation system (though hooks could enable it)
- **Server-side rendering**: Not applicable to Emacs
- **Concurrent rendering**: Emacs is single-threaded; we use idle timers instead
- **Backward compatibility with widget.el**: Clean break, though we may wrap widgets

# Foundational Architecture

## High-Level Overview

``` example
+-----------------------------------------------------------------+
|                        User Code                                |
|  (defcomponent todo-item (:props text done) ...)                |
+-----------------------------------------------------------------+
                              |
                              v
+-----------------------------------------------------------------+
|                    Component Registry                           |
|  Stores component definitions (type -> definition)              |
+-----------------------------------------------------------------+
                              |
                              v
+-----------------------------------------------------------------+
|                    Component Tree (Virtual)                     |
|  In-memory tree of component instances with props/state         |
|  This is the "Virtual DOM" equivalent                           |
+-----------------------------------------------------------------+
                              |
                              v
+-----------------------------------------------------------------+
|                    Reconciler                                   |
|  Compares old tree vs new tree                                  |
|  Produces minimal set of buffer operations                      |
+-----------------------------------------------------------------+
                              |
                              v
+-----------------------------------------------------------------+
|                    Buffer Renderer                              |
|  Executes buffer mutations (insert, delete, add overlays)       |
|  Handles cursor preservation                                    |
+-----------------------------------------------------------------+
                              |
                              v
+-----------------------------------------------------------------+
|                    Emacs Buffer                                 |
|  The actual visible text with overlays and text properties      |
+-----------------------------------------------------------------+
```

## The Render Cycle

``` example
State Change
     |
     v
+--------------+
| Mark dirty   |  Component and ancestors marked for re-render
+--------------+
     |
     v
+--------------+
| Schedule     |  Batch multiple changes, defer to idle time
+--------------+
     |
     v
+--------------+
| Render phase |  Build new virtual tree (pure, no side effects)
+--------------+
     |
     v
+--------------+
| Diff phase   |  Compare old tree <-> new tree
+--------------+
     |
     v
+--------------+
| Commit phase |  Apply mutations to buffer, run lifecycle hooks
+--------------+
```

## Key Design Decisions

**Decision 1: Immutable props, mutable state** - Props flow down from parent, are read-only to child - State is owned by component, changes trigger re-render

**Decision 2: Render functions are pure** - Given same props and state, produce same virtual tree - No direct buffer manipulation in render

**Decision 3: Virtual tree is a simple data structure** - Easy to diff, serialise, inspect - No circular references in the tree itself (parent refs stored separately)

**Decision 4: Buffer is the "DOM"** - Text content + overlays + text properties - All buffer mutations go through the renderer, never direct

**Decision 5: Build on widget.el for input primitives** - `ui-field` wraps widget field (not raw text + overlays) - `ui-button` wraps widget button - `ui-select` may use widget-choice internally - TAB navigation, field editing come free from widget library - Reconciler updates widget values rather than delete/recreate where possible - We provide: component model, state management, layout, reconciliation - Widget.el provides: battle-tested input handling, keyboard navigation

**Decision 6: Two-pass rendering architecture** - **Pass 1 (Measure)**: Calculate dimensions without buffer mutation - Width of each vnode (for tables, boxes) - \[Future: Height for multi-line support in horizontal layouts\] - **Pass 2 (Render)**: Insert into buffer with known dimensions - Even if MVP only uses pass 1 for width, the infrastructure exists for future multi-line support

# Core Data Structures

## Component Definition

A component definition is a template—it describes *how* to create instances.

``` elisp
(cl-defstruct (ui-component-def (:constructor ui-component-def--create))
  "Definition of a component type."
  name                    ; Symbol identifying this component type
  props-spec              ; List of (:name :type :default :required) plists
  initial-state-fn        ; (lambda (props) state)  -  returns initial state
  render-fn               ; (lambda (props state) vtree)  -  pure function

  ;; Lifecycle hooks (all optional)
  on-mount                ; (lambda (instance))
  on-unmount              ; (lambda (instance))
  on-update               ; (lambda (instance prev-props prev-state))

  ;; Optimisation
  should-update-fn        ; (lambda (old-props new-props old-state new-state) bool)
  memo-by                 ; nil, 'eq, 'equal, or custom comparison fn

  ;; Documentation
  docstring)
```

## Component Instance

An instance is a live component in the tree with actual props, state, and buffer position.

``` elisp
(cl-defstruct (ui-instance (:constructor ui-instance--create))
  "A live instance of a component."
  id                      ; Unique identifier (for debugging/tracking)
  key                     ; User-provided key for reconciliation (can be nil)
  def                     ; Reference to ui-component-def

  ;; Data
  props                   ; Plist of current props
  state                   ; Current state (mutable)

  ;; Tree structure
  parent                  ; Parent ui-instance (nil for root)
  children                ; List of child ui-instances (rendered children)

  ;; Buffer binding
  buffer                  ; Buffer this instance is rendered into
  region-start            ; Marker for start of this component's region
  region-end              ; Marker for end of this component's region
  overlays                ; List of overlays owned by this instance

  ;; Lifecycle state
  mounted-p               ; Has on-mount been called?
  dirty-p                 ; Needs re-render?

  ;; Cache
  last-vtree              ; Last rendered virtual tree (for diffing)
  last-props              ; Props from last render (for should-update check)
  last-state)             ; State from last render
```

## Virtual Tree Nodes

The virtual tree is what render functions produce. It's a pure data structure.

``` elisp
;; Base structure for all virtual nodes
(cl-defstruct (ui-vnode (:constructor nil))
  "Base type for virtual tree nodes."
  key)                    ; Optional key for reconciliation

;; A reference to a component (will be instantiated)
(cl-defstruct (ui-vnode-component (:include ui-vnode)
                                   (:constructor ui-vnode-component--create))
  "Virtual node representing a component."
  type                    ; Symbol  -  the component type name
  props                   ; Plist of props to pass
  children)               ; List of child vnodes (passed as :children prop)

;; Primitive: raw text
(cl-defstruct (ui-vnode-text (:include ui-vnode)
                              (:constructor ui-vnode-text--create))
  "Virtual node representing plain text."
  content                 ; String
  face                    ; Face or nil
  properties)             ; Additional text properties plist

;; Primitive: editable field
(cl-defstruct (ui-vnode-field (:include ui-vnode)
                               (:constructor ui-vnode-field--create))
  "Virtual node representing an editable text field."
  value                   ; Current string value
  size                    ; Width in characters (nil = variable)
  face
  on-change               ; (lambda (new-value))
  keymap)                 ; Optional keymap for the field

;; Primitive: clickable button
(cl-defstruct (ui-vnode-button (:include ui-vnode)
                                (:constructor ui-vnode-button--create))
  "Virtual node representing a button."
  label                   ; String or child vnode
  face
  on-click                ; (lambda ())
  disabled-p)

;; Primitive: newline
(cl-defstruct (ui-vnode-newline (:include ui-vnode)
                                 (:constructor ui-vnode-newline--create))
  "Virtual node representing a line break.")

;; Primitive: horizontal space
(cl-defstruct (ui-vnode-space (:include ui-vnode)
                               (:constructor ui-vnode-space--create))
  "Virtual node representing whitespace."
  width)                  ; Number of spaces

;; Container: sequence of children (renders children in order)
(cl-defstruct (ui-vnode-fragment (:include ui-vnode)
                                  (:constructor ui-vnode-fragment--create))
  "Virtual node that groups children without wrapper."
  children)               ; List of child vnodes
```

## Diff Operations

The reconciler produces a list of operations to transform the buffer.

``` elisp
(cl-defstruct (ui-op-insert (:constructor ui-op-insert--create))
  "Insert content at position."
  position                ; Buffer position (integer)
  vnode                   ; The vnode to render
  parent-instance)        ; Parent instance for context

(cl-defstruct (ui-op-delete (:constructor ui-op-delete--create))
  "Delete a region."
  start                   ; Start position
  end                     ; End position
  instance)               ; The instance being removed

(cl-defstruct (ui-op-replace (:constructor ui-op-replace--create))
  "Replace content in region."
  start
  end
  old-vnode
  new-vnode
  instance)

(cl-defstruct (ui-op-update-props (:constructor ui-op-update-props--create))
  "Update text properties/overlays without changing content."
  start
  end
  old-props
  new-props
  instance)

(cl-defstruct (ui-op-move (:constructor ui-op-move--create))
  "Move a region to a new position (for keyed reordering)."
  from-start
  from-end
  to-position
  instance)
```

## Context

For passing data deeply without prop drilling.

``` elisp
(cl-defstruct (ui-context (:constructor ui-context--create))
  "A context for sharing data across component tree."
  name                    ; Symbol identifying this context
  default-value)          ; Default if no provider found

(cl-defstruct (ui-context-provider (:constructor ui-context-provider--create))
  "Runtime provider binding."
  context                 ; The ui-context
  value)                  ; Current provided value
```

## Render Context

Passed through the tree during rendering, carrying inherited state.

``` elisp
(cl-defstruct (ui-render-context (:constructor ui-render-context--create))
  "Context passed during rendering."

  ;; Indentation
  indent-level            ; Current indentation (characters)
  indent-string           ; String to use (default: spaces)
  line-prefix             ; Computed prefix for current indent

  ;; Face inheritance
  face-base               ; Default text face
  face-input              ; Interactive element face
  face-muted              ; De-emphasised text (e.g., shadow)
  face-accent             ; Highlighted/important

  ;; Layout constraints
  available-width         ; Remaining horizontal space (for wrapping)
  in-table-p              ; Are we inside a table cell?

  ;; Parent reference (for cursor restoration)
  parent-instance)

(defvar ui--render-context nil
  "Dynamically bound during render.")

(defun ui-context-face (type)
  "Get face of TYPE (:base, :input, :muted, :accent) from context."
  (when ui--render-context
    (pcase type
      (:base (ui-render-context-face-base ui--render-context))
      (:input (ui-render-context-face-input ui--render-context))
      (:muted (ui-render-context-face-muted ui--render-context))
      (:accent (ui-render-context-face-accent ui--render-context)))))
```

## Scheduler

Manages batched updates.

``` elisp
(cl-defstruct (ui-scheduler (:constructor ui-scheduler--create))
  "Manages the render queue."
  dirty-roots             ; Set of root instances needing re-render
  pending-effects         ; List of (instance . effect-fn) to run after commit
  is-rendering-p          ; Are we currently in a render cycle?
  timer)                  ; Idle timer for deferred rendering
```

# Component Definition DSL

## The `defcomponent` Macro

``` elisp
(defcomponent todo-item
  "A single todo item with checkbox and text."

  ;; Props declaration
  :props ((text :type string :required t)
          (done :type boolean :default nil)
          (on-toggle :type function :required t)
          (on-delete :type function))

  ;; Initial state (optional)
  :state ((editing nil)
          (edit-text ""))

  ;; Lifecycle hooks (optional)
  :on-mount (lambda () (message "Todo mounted: %s" text))
  :on-unmount (lambda () (message "Todo unmounted"))
  :on-update (lambda (prev-props prev-state)
               (when (not (equal (plist-get prev-props :text) text))
                 (message "Text changed!")))

  ;; Optimisation (optional)
  :should-update (lambda (old-props new-props old-state new-state)
                   (or (not (equal old-props new-props))
                       (not (equal old-state new-state))))

  ;; Render function  -  the body
  :render
  (if editing
      ;; Editing mode
      (ui-fragment
        (ui-field :value edit-text
                  :on-change (lambda (v) (setf edit-text v))
                  :keymap todo-edit-keymap)
        (ui-text " ")
        (ui-button :label "Cancel"
                   :on-click (lambda ()
                               (setf editing nil)
                               (setf edit-text text))))
    ;; Display mode
    (ui-fragment
      (ui-button :label (if done "[x]" "[ ]")
                 :on-click on-toggle)
      (ui-text " ")
      (ui-text text :face (if done 'shadow nil))
      (when on-delete
        (ui-fragment
          (ui-text " ")
          (ui-button :label "×" :on-click on-delete))))))
```

## Macro Expansion

The `defcomponent` macro expands to:

``` elisp
(progn
  ;; Register the component definition
  (ui-register-component
   (ui-component-def--create
    :name 'todo-item
    :docstring "A single todo item with checkbox and text."
    :props-spec '((:name text :type string :required t)
                  (:name done :type boolean :default nil)
                  (:name on-toggle :type function :required t)
                  (:name on-delete :type function :required nil))
    :initial-state-fn (lambda (_props)
                        (list :editing nil :edit-text ""))
    :render-fn (lambda (props state)
                 ;; Destructure props and state into lexical bindings
                 (let ((text (plist-get props :text))
                       (done (plist-get props :done))
                       (on-toggle (plist-get props :on-toggle))
                       (on-delete (plist-get props :on-delete))
                       (editing (plist-get state :editing))
                       (edit-text (plist-get state :edit-text)))
                   ;; The render body
                   (if editing ...)))
    :on-mount (lambda () ...)
    :on-unmount (lambda () ...)
    :on-update (lambda (prev-props prev-state) ...)
    :should-update-fn (lambda (old-props new-props old-state new-state) ...)))

  ;; Define a constructor function for convenience
  (defun todo-item (&rest props)
    "Create a todo-item component. [Generated]"
    (ui-vnode-component--create
     :type 'todo-item
     :props props
     :key (plist-get props :key))))
```

## State Mutation with `let-state`

State variables need special handling to trigger re-renders on mutation:

``` elisp
(defmacro let-state (bindings &rest body)
  "Bind state variables with setters that trigger re-render.

Each binding is (NAME INITIAL-VALUE) or (NAME INITIAL-VALUE SETTER-NAME).

Example:
  (let-state ((count 0)
              (name \"\" set-name))
    (ui-button :on-click (lambda () (cl-incf count))))"
  ...)
```

Expansion:

``` elisp
(let-state ((count 0))
  (ui-button :on-click (lambda () (cl-incf count))))

;; Expands to:
(let* ((--ui-instance-- (ui--current-instance))
       (count (plist-get (ui-instance-state --ui-instance--) :count)))
  (cl-symbol-macrolet
      ((count (ui--state-accessor --ui-instance-- :count)))
    (ui-button :on-click (lambda () (cl-incf count)))))
```

Where `ui--state-accessor` is a generalised variable that: 1. Reads from instance state 2. On `setf`, updates state and marks instance dirty

## Primitive Constructors

``` elisp
(defun ui-text (content &rest props)
  "Create a text vnode.

CONTENT is the string to display.
PROPS can include :face, :key, :properties."
  (ui-vnode-text--create
   :key (plist-get props :key)
   :content content
   :face (plist-get props :face)
   :properties (plist-get props :properties)))

(defun ui-button (&rest props)
  "Create a button vnode.

PROPS must include :label, :on-click.
Optional: :face, :disabled, :key."
  (ui-vnode-button--create
   :key (plist-get props :key)
   :label (plist-get props :label)
   :face (plist-get props :face)
   :on-click (plist-get props :on-click)
   :disabled-p (plist-get props :disabled)))

(defun ui-field (&rest props)
  "Create an editable field vnode.

PROPS should include :value, :on-change.
Optional: :size, :face, :keymap, :key."
  (ui-vnode-field--create
   :key (plist-get props :key)
   :value (or (plist-get props :value) "")
   :size (plist-get props :size)
   :face (plist-get props :face)
   :on-change (plist-get props :on-change)
   :keymap (plist-get props :keymap)))

(defun ui-fragment (&rest children)
  "Group multiple vnodes without adding wrapper text.

Filters out nil children for conditional rendering."
  (ui-vnode-fragment--create
   :children (remq nil (flatten-list children))))

(defun ui-newline ()
  "Create a line break vnode."
  (ui-vnode-newline--create))

(defun ui-space (&optional width)
  "Create horizontal whitespace.

WIDTH defaults to 1."
  (ui-vnode-space--create :width (or width 1)))
```

## Input Capturing Primitives

Beyond inline-editable fields, we need components that capture input via prompts:

``` elisp
;; Selection from options (uses completing-read)
(cl-defstruct (ui-vnode-select (:include ui-vnode)
                                (:constructor ui-vnode-select--create))
  "Selection widget using completing-read."
  value                   ; Current selected value
  options                 ; List of options (strings or (display . value) pairs)
  on-change               ; (lambda (new-value))
  prompt                  ; Prompt string for completing-read
  require-match           ; Must select from options?
  face)

(defun ui-select (&rest props)
  "Create a selection vnode.

Renders as a button showing current value. When clicked,
presents options via `completing-read`.

PROPS:
  :value - Current selection
  :options - List of choices
  :on-change - Called with new selection
  :prompt - Minibuffer prompt (default: \"Select: \")
  :require-match - If t, must choose from options"
  (ui-vnode-select--create
   :key (plist-get props :key)
   :value (plist-get props :value)
   :options (plist-get props :options)
   :on-change (plist-get props :on-change)
   :prompt (or (plist-get props :prompt) "Select: ")
   :require-match (plist-get props :require-match)
   :face (plist-get props :face)))

(defun ui--render-select (vnode parent-instance)
  "Render a select as a clickable button."
  (let* ((value (ui-vnode-select-value vnode))
         (options (ui-vnode-select-options vnode))
         (on-change (ui-vnode-select-on-change vnode))
         (prompt (ui-vnode-select-prompt vnode))
         (require-match (ui-vnode-select-require-match vnode))
         (display-value (or value "[none]")))

    ;; Render as button
    (ui--render-button
     (ui-vnode-button--create
      :label (format "%s ▾" display-value)
      :face (or (ui-vnode-select-face vnode) 'button)
      :on-click (lambda ()
                  (let ((choice (completing-read
                                 prompt
                                 options
                                 nil
                                 require-match
                                 nil nil value)))
                    (when on-change
                      (funcall on-change choice)))))
     parent-instance)))

;; Number input with increment/decrement
(cl-defstruct (ui-vnode-number (:include ui-vnode)
                                (:constructor ui-vnode-number--create))
  "Numeric input with optional spinner."
  value                   ; Current number
  min                     ; Minimum value (optional)
  max                     ; Maximum value (optional)
  step                    ; Increment step (default 1)
  on-change
  spinner-p               ; Show +/- buttons?
  face)

(defun ui-number (&rest props)
  "Create a number input vnode."
  (ui-vnode-number--create
   :key (plist-get props :key)
   :value (or (plist-get props :value) 0)
   :min (plist-get props :min)
   :max (plist-get props :max)
   :step (or (plist-get props :step) 1)
   :on-change (plist-get props :on-change)
   :spinner-p (plist-get props :spinner)
   :face (plist-get props :face)))

;; Checkbox (boolean toggle)
(cl-defstruct (ui-vnode-checkbox (:include ui-vnode)
                                  (:constructor ui-vnode-checkbox--create))
  "Boolean checkbox."
  checked-p
  on-change
  label
  face)

(defun ui-checkbox (&rest props)
  "Create a checkbox vnode."
  (ui-vnode-checkbox--create
   :key (plist-get props :key)
   :checked-p (plist-get props :checked)
   :on-change (plist-get props :on-change)
   :label (plist-get props :label)
   :face (plist-get props :face)))
```

## Hidden Component

Components can be hidden without being destroyed, preserving their state:

``` elisp
(cl-defstruct (ui-vnode-hidden (:include ui-vnode)
                                (:constructor ui-vnode-hidden--create))
  "Container that can hide its children."
  visible-p               ; Whether children are rendered
  child)                  ; The child vnode

(defun ui-hidden (visible-p child)
  "Conditionally hide CHILD while preserving its instance.

Unlike (when visible-p child), the instance is NOT destroyed
when hidden. State is preserved, and effects continue running.

Use this when you want to toggle visibility frequently without
losing component state."
  (ui-vnode-hidden--create
   :visible-p visible-p
   :child child))

(defun ui--render-hidden (vnode parent-instance)
  "Render a hidden container."
  (let ((visible (ui-vnode-hidden-visible-p vnode))
        (child (ui-vnode-hidden-child vnode)))

    (if visible
        ;; Render child normally
        (ui--instantiate-and-render child parent-instance)

      ;; Create instance but don't render content
      ;; Markers point to same position (zero-width region)
      (let* ((start (point-marker))
             (instance (ui--create-hidden-instance child parent-instance)))
        (setf (ui-instance-region-start instance) start)
        (setf (ui-instance-region-end instance) start)
        (setf (ui-instance-visible-p instance) nil)
        instance))))

;; Hidden instances still have state and can run effects
;; but produce no buffer content
```

**Difference between hidden and conditional rendering:**

``` elisp
;; Conditional: instance destroyed when false
(when show-details
  (detail-panel :data data))

;; Hidden: instance preserved, just not rendered
(ui-hidden show-details
  (detail-panel :data data))
```

Use `ui-hidden` when: - Toggling visibility frequently (tabs, accordions) - Component has expensive initialization - You want to preserve user input in hidden forms

# Reconciliation Algorithm

## Overview

The reconciler's job is to take two virtual trees (old and new) and produce the minimal set of operations to transform the buffer from the old state to the new state.

React's reconciliation is based on two heuristics: 1. **Elements of different types produce different trees** — if a node type changes, tear down the old subtree entirely 2. **Keys provide stable identity** — children with the same key are the "same" element across renders

For Emacs, we add a third concern: 3. **Preserve cursor position** — track which component owns point and restore it after mutations

## Algorithm Structure

``` example
reconcile(old-instance, new-vnode) → list of operations

1. If new-vnode is nil:
   → Return [DELETE old-instance]

2. If old-instance is nil:
   → Return [INSERT new-vnode]

3. If types differ (old-instance.type ≠ new-vnode.type):
   → Return [DELETE old-instance, INSERT new-vnode]

4. If types match:
   a. Check should-update (if false, skip with no ops)
   b. Diff props → property update operations
   c. Render new children from new-vnode
   d. Reconcile old children with new children
   → Return combined operations
```

## Detailed Reconciliation Logic

``` elisp
(defun ui--reconcile (old-instance new-vnode parent-instance)
  "Reconcile OLD-INSTANCE with NEW-VNODE.

Returns a list of ui-op-* structures.
PARENT-INSTANCE is used for context when creating new instances."
  (cond
   ;; Case 1: Nothing new  -  delete old
   ((null new-vnode)
    (if old-instance
        (list (ui-op-delete--create
               :start (marker-position (ui-instance-region-start old-instance))
               :end (marker-position (ui-instance-region-end old-instance))
               :instance old-instance))
      nil))

   ;; Case 2: Nothing old  -  insert new
   ((null old-instance)
    (list (ui-op-insert--create
           :position (ui--insertion-point parent-instance)
           :vnode new-vnode
           :parent-instance parent-instance)))

   ;; Case 3: Type mismatch  -  replace entirely
   ((not (ui--same-type-p old-instance new-vnode))
    (list (ui-op-delete--create
           :start (marker-position (ui-instance-region-start old-instance))
           :end (marker-position (ui-instance-region-end old-instance))
           :instance old-instance)
          (ui-op-insert--create
           :position (marker-position (ui-instance-region-start old-instance))
           :vnode new-vnode
           :parent-instance parent-instance)))

   ;; Case 4: Same type  -  diff and recurse
   (t
    (ui--reconcile-same-type old-instance new-vnode))))

(defun ui--same-type-p (instance vnode)
  "Check if INSTANCE and VNODE represent the same component type."
  (cond
   ;; Component vnodes
   ((ui-vnode-component-p vnode)
    (and (ui-instance-def instance)
         (eq (ui-component-def-name (ui-instance-def instance))
             (ui-vnode-component-type vnode))))
   ;; Primitive vnodes  -  check struct type
   ((ui-vnode-text-p vnode)
    (ui--primitive-text-p instance))
   ((ui-vnode-button-p vnode)
    (ui--primitive-button-p instance))
   ((ui-vnode-field-p vnode)
    (ui--primitive-field-p instance))
   ;; etc.
   (t nil)))
```

## Reconciling Same-Type Components

``` elisp
(defun ui--reconcile-same-type (old-instance new-vnode)
  "Reconcile when types match. Returns list of operations."
  (let ((ops nil)
        (old-props (ui-instance-props old-instance))
        (new-props (ui-vnode-component-props new-vnode))
        (old-state (ui-instance-state old-instance)))

    ;; Check should-update optimisation
    (when (ui--should-update-p old-instance old-props new-props old-state)

      ;; Update instance props (in place)
      (setf (ui-instance-props old-instance) new-props)

      ;; Re-render to get new child vtree
      (let* ((def (ui-instance-def old-instance))
             (render-fn (ui-component-def-render-fn def))
             (new-vtree (funcall render-fn new-props old-state))
             (old-vtree (ui-instance-last-vtree old-instance)))

        ;; Reconcile children
        (setq ops (ui--reconcile-children
                   (ui-instance-children old-instance)
                   (ui--vtree-children new-vtree)
                   old-instance))

        ;; Cache new vtree
        (setf (ui-instance-last-vtree old-instance) new-vtree)
        (setf (ui-instance-last-props old-instance) new-props)
        (setf (ui-instance-last-state old-instance) old-state)))

    ops))

(defun ui--should-update-p (instance old-props new-props old-state)
  "Determine if INSTANCE should re-render."
  (let ((def (ui-instance-def instance)))
    (if-let ((should-update-fn (ui-component-def-should-update-fn def)))
        ;; Custom should-update function
        (funcall should-update-fn old-props new-props
                 old-state (ui-instance-state instance))
      ;; Default: update if props or state changed
      (or (not (equal old-props new-props))
          (not (equal old-state (ui-instance-state instance)))))))
```

## Reconciling Children Lists (The Key Algorithm)

This is where keys become critical. Without keys, inserting at the beginning of a list would cause all children to be "replaced".

``` elisp
(defun ui--reconcile-children (old-children new-vnodes parent-instance)
  "Reconcile list of OLD-CHILDREN with NEW-VNODES.

Uses keys for efficient matching. Returns list of operations."
  (let ((ops nil)
        (old-keyed (make-hash-table :test 'equal))
        (old-unkeyed nil)
        (new-keyed (make-hash-table :test 'equal))
        (new-unkeyed nil))

    ;; Index old children by key
    (dolist (child old-children)
      (let ((key (ui-instance-key child)))
        (if key
            (puthash key child old-keyed)
          (push child old-unkeyed))))
    (setq old-unkeyed (nreverse old-unkeyed))

    ;; Index new vnodes by key
    (dolist (vnode new-vnodes)
      (let ((key (ui-vnode-key vnode)))
        (if key
            (puthash key vnode new-keyed)
          (push vnode new-unkeyed))))
    (setq new-unkeyed (nreverse new-unkeyed))

    ;; Process keyed children
    (maphash (lambda (key new-vnode)
               (let ((old-child (gethash key old-keyed)))
                 (if old-child
                     ;; Key exists in both  -  reconcile
                     (progn
                       (push (ui--reconcile old-child new-vnode parent-instance) ops)
                       (remhash key old-keyed))
                   ;; New key  -  insert
                   (push (ui-op-insert--create
                          :vnode new-vnode
                          :parent-instance parent-instance)
                         ops))))
             new-keyed)

    ;; Remaining old keyed children were removed
    (maphash (lambda (_key old-child)
               (push (ui-op-delete--create
                      :instance old-child
                      :start (marker-position (ui-instance-region-start old-child))
                      :end (marker-position (ui-instance-region-end old-child)))
                     ops))
             old-keyed)

    ;; Process unkeyed children by position
    (let ((old-iter old-unkeyed)
          (new-iter new-unkeyed))
      (while (or old-iter new-iter)
        (let ((old-child (car old-iter))
              (new-vnode (car new-iter)))
          (push (ui--reconcile old-child new-vnode parent-instance) ops)
          (setq old-iter (cdr old-iter))
          (setq new-iter (cdr new-iter)))))

    ;; Flatten nested operation lists
    (apply #'append (nreverse ops))))
```

## Reconciling Primitives

Primitive vnodes (text, button, field) have simpler reconciliation:

``` elisp
(defun ui--reconcile-primitive-text (old-instance new-vnode)
  "Reconcile a text primitive."
  (let ((old-content (ui--primitive-text-content old-instance))
        (new-content (ui-vnode-text-content new-vnode))
        (old-face (ui--primitive-text-face old-instance))
        (new-face (ui-vnode-text-face new-vnode))
        (start (marker-position (ui-instance-region-start old-instance)))
        (end (marker-position (ui-instance-region-end old-instance))))

    (cond
     ;; Content changed  -  replace text
     ((not (string= old-content new-content))
      (list (ui-op-replace--create
             :start start
             :end end
             :old-vnode (ui--instance-to-vnode old-instance)
             :new-vnode new-vnode
             :instance old-instance)))

     ;; Only face changed  -  update properties
     ((not (equal old-face new-face))
      (list (ui-op-update-props--create
             :start start
             :end end
             :old-props (list :face old-face)
             :new-props (list :face new-face)
             :instance old-instance)))

     ;; No change
     (t nil))))

(defun ui--reconcile-primitive-field (old-instance new-vnode)
  "Reconcile an editable field primitive."
  (let ((old-value (ui--primitive-field-value old-instance))
        (new-value (ui-vnode-field-value new-vnode))
        (start (marker-position (ui-instance-region-start old-instance)))
        (end (marker-position (ui-instance-region-end old-instance))))

    ;; Fields are special: user might be editing
    ;; Only update if value changed AND field is not focused
    (if (and (not (string= old-value new-value))
             (not (ui--field-focused-p old-instance)))
        (list (ui-op-replace--create
               :start start
               :end end
               :old-vnode (ui--instance-to-vnode old-instance)
               :new-vnode new-vnode
               :instance old-instance))
      ;; Update callbacks even if value unchanged
      (progn
        (setf (ui--primitive-field-on-change old-instance)
              (ui-vnode-field-on-change new-vnode))
        nil))))
```

## Operation Ordering

Operations must be applied in the correct order to maintain valid buffer positions:

``` elisp
(defun ui--sort-operations (ops)
  "Sort operations for safe application.

Deletions are sorted by position descending (delete from end first).
Insertions are sorted by position ascending.
This prevents position shifts from invalidating later operations."
  (let ((deletes nil)
        (inserts nil)
        (updates nil)
        (moves nil))

    ;; Categorise
    (dolist (op ops)
      (cond
       ((ui-op-delete-p op) (push op deletes))
       ((ui-op-insert-p op) (push op inserts))
       ((ui-op-replace-p op) (push op updates))
       ((ui-op-update-props-p op) (push op updates))
       ((ui-op-move-p op) (push op moves))))

    ;; Sort
    (setq deletes (sort deletes
                        (lambda (a b)
                          (> (ui-op-delete-start a)
                             (ui-op-delete-start b)))))
    (setq inserts (sort inserts
                        (lambda (a b)
                          (< (ui-op-insert-position a)
                             (ui-op-insert-position b)))))

    ;; Order: deletes first (from end), then updates, then inserts
    (append deletes updates inserts moves)))
```

## Complexity Analysis

- **Best case (no changes)**: O(n) where n is tree depth (just prop comparison)
- **Typical case**: O(n) where n is number of changed nodes
- **Worst case (all keys changed)**: O(n) where n is total nodes

The key insight from React: by assuming different types produce different trees, we avoid the O(n³) tree-edit-distance algorithm.

# Cursor Preservation

## The Problem

In React/browser contexts, focus management is relatively simple — elements have a `focus()` method and the browser tracks which element is focused. In Emacs, **point** (the cursor position) is the primary interaction mechanism, and it's always somewhere in the buffer.

When we re-render components: - Text may be inserted before point → point should stay on the same logical content - Text may be deleted → point might need to move to a valid position
- The component containing point might be replaced → we need to restore to equivalent position - The component containing point might be deleted entirely → we need a fallback

## Goals

1.  **Semantic preservation**: Point should stay on the "same thing" after re-render
2.  **Field continuity**: If editing a field, don't disrupt the editing experience
3.  **Graceful degradation**: If exact position is impossible, choose sensible fallback
4.  **No flicker**: User should not perceive point jumping around

## Cursor Context Structure

Before any buffer mutations, we capture the cursor context:

``` elisp
(cl-defstruct (ui-cursor-context (:constructor ui-cursor-context--create))
  "Captured cursor state for restoration after re-render."

  ;; Which instance owns point?
  instance                ; The leaf ui-instance containing point
  instance-path           ; List of ancestor instances (root first)

  ;; Position within the instance
  offset-from-start       ; Characters from instance start to point
  offset-from-end         ; Characters from point to instance end

  ;; For fields: additional context
  field-p                 ; Is this an editable field?
  field-value             ; Field value at capture time
  field-cursor-pos        ; Position within field text

  ;; For lists: sibling context
  sibling-index           ; Index among siblings (for fallback)
  sibling-key             ; Key of the instance (if any)

  ;; Original position (for validation)
  original-point          ; Buffer position at capture time
  original-mark           ; Mark position (if active)
  mark-active-p)          ; Was mark active?
```

## Capturing Cursor Context

``` elisp
(defun ui--capture-cursor-context (root-instance)
  "Capture cursor context before re-render.

Returns a ui-cursor-context or nil if point is outside ROOT-INSTANCE."
  (let ((pt (point)))
    ;; Check if point is within root
    (when (and (>= pt (marker-position (ui-instance-region-start root-instance)))
               (<= pt (marker-position (ui-instance-region-end root-instance))))

      (let ((instance (ui--find-leaf-instance-at-point root-instance pt))
            (path nil))
        (when instance
          ;; Build path from root to instance
          (let ((current instance))
            (while current
              (push current path)
              (setq current (ui-instance-parent current))))

          (ui-cursor-context--create
           :instance instance
           :instance-path path
           :offset-from-start (- pt (marker-position
                                     (ui-instance-region-start instance)))
           :offset-from-end (- (marker-position
                                (ui-instance-region-end instance)) pt)
           :field-p (ui--primitive-field-p instance)
           :field-value (when (ui--primitive-field-p instance)
                          (ui--primitive-field-value instance))
           :field-cursor-pos (when (ui--primitive-field-p instance)
                               (- pt (marker-position
                                      (ui-instance-region-start instance))))
           :sibling-index (ui--sibling-index instance)
           :sibling-key (ui-instance-key instance)
           :original-point pt
           :original-mark (when (mark t) (marker-position (mark-marker)))
           :mark-active-p mark-active))))))

(defun ui--find-leaf-instance-at-point (instance pt)
  "Find the deepest (leaf) instance containing PT."
  (let ((start (marker-position (ui-instance-region-start instance)))
        (end (marker-position (ui-instance-region-end instance))))
    (when (and (>= pt start) (<= pt end))
      ;; Check children first (depth-first)
      (let ((found nil))
        (dolist (child (ui-instance-children instance))
          (when (and (not found)
                     (>= pt (marker-position (ui-instance-region-start child)))
                     (<= pt (marker-position (ui-instance-region-end child))))
            (setq found (ui--find-leaf-instance-at-point child pt))))
        (or found instance)))))

(defun ui--sibling-index (instance)
  "Return the index of INSTANCE among its parent's children."
  (when-let ((parent (ui-instance-parent instance)))
    (cl-position instance (ui-instance-children parent) :test #'eq)))
```

## Restoring Cursor Context

After mutations are applied, we restore point using the captured context.

**Key principle**: When the component owning cursor is destroyed, place cursor at the position where that component *was* — i.e., at the start of where the destroyed component lived within its parent. This feels most natural to users.

``` elisp
(defun ui--restore-cursor-context (context root-instance)
  "Restore cursor position from CONTEXT after re-render.

Uses a series of fallback strategies if the original instance is gone.

Strategy priority:
1. Find same instance by key path → restore offset within it
2. Find instance by structural path → restore offset
3. Instance destroyed → go to where it WAS (parent's child position)
4. Parent also destroyed → original position if valid
5. Everything gone → start of root"
  (when context
    (let ((restored nil))

      ;; Strategy 1: Find instance by key path
      (setq restored (ui--restore-by-key-path context root-instance))

      ;; Strategy 2: Find instance by structural path
      (unless restored
        (setq restored (ui--restore-by-structural-path context root-instance)))

      ;; Strategy 3: Component destroyed  -  go to where it was in parent
      ;; This is the most user-friendly behavior: cursor stays at the
      ;; "gap" where the component used to be
      (unless restored
        (setq restored (ui--restore-to-destroyed-position context root-instance)))

      ;; Strategy 4: Use original position if still valid
      (unless restored
        (setq restored (ui--restore-by-original-position context root-instance)))

      ;; Strategy 5: Go to start of root
      (unless restored
        (goto-char (marker-position (ui-instance-region-start root-instance))))

      ;; Restore mark if it was active
      (when (ui-cursor-context-mark-active-p context)
        (ui--restore-mark context root-instance)))))

(defun ui--restore-to-destroyed-position (context root-instance)
  "Place cursor where the destroyed component was.

Finds the parent, then positions at the start of where the Nth child
would be (based on original sibling index)."
  (let* ((path (ui-cursor-context-instance-path context))
         (parent-path (butlast path))
         (sibling-idx (ui-cursor-context-sibling-index context)))

    (when-let ((parent (ui--find-by-path parent-path root-instance)))
      (let ((children (ui-instance-children parent)))
        (cond
         ;; Parent has children  -  go to the one at our old index (or last)
         (children
          (let* ((target-idx (min sibling-idx (1- (length children))))
                 (target (nth target-idx children)))
            (goto-char (marker-position (ui-instance-region-start target)))
            t))

         ;; Parent has no children  -  go to parent's content start
         (t
          (goto-char (marker-position (ui-instance-region-start parent)))
          t))))))

(defun ui--restore-by-key-path (context root-instance)
  "Try to find instance by following keys in the path."
  (let ((path (ui-cursor-context-instance-path context))
        (current root-instance))

    ;; Walk down the path, matching by key where possible
    (catch 'failed
      (dolist (path-instance (cdr path)) ; Skip root
        (let ((key (ui-instance-key path-instance))
              (found nil))
          (if key
              ;; Look for child with same key
              (dolist (child (ui-instance-children current))
                (when (equal (ui-instance-key child) key)
                  (setq found child)))
            ;; No key  -  try structural match
            (let ((idx (cl-position path-instance
                                    (ui-cursor-context-instance-path context)
                                    :test #'eq)))
              (setq found (nth (ui--sibling-index path-instance)
                               (ui-instance-children current)))))
          (if found
              (setq current found)
            (throw 'failed nil))))

      ;; Found the instance  -  restore position within it
      (ui--goto-instance-offset current context)
      t)))

(defun ui--restore-by-structural-path (context root-instance)
  "Try to find instance by following sibling indices."
  (let ((path (ui-cursor-context-instance-path context))
        (current root-instance))

    (catch 'failed
      (dolist (path-instance (cdr path))
        (let* ((target-idx (ui--sibling-index path-instance))
               (children (ui-instance-children current))
               (found (and target-idx
                           (< target-idx (length children))
                           (nth target-idx children))))
          (if found
              (setq current found)
            (throw 'failed nil))))

      (ui--goto-instance-offset current context)
      t)))

(defun ui--restore-by-sibling (context root-instance)
  "Try to find a sibling of the original instance."
  (let* ((path (ui-cursor-context-instance-path context))
         (parent-path (butlast path))
         (parent (ui--find-by-path parent-path root-instance)))

    (when parent
      (let* ((idx (ui-cursor-context-sibling-index context))
             (children (ui-instance-children parent))
             (target (cond
                      ;; Same index if it exists
                      ((and idx (< idx (length children)))
                       (nth idx children))
                      ;; Last child if original index too high
                      (children (car (last children))))))
        (when target
          (goto-char (marker-position (ui-instance-region-start target)))
          t)))))

(defun ui--restore-by-original-position (context root-instance)
  "Check if original position is still within root bounds."
  (let ((pt (ui-cursor-context-original-point context))
        (start (marker-position (ui-instance-region-start root-instance)))
        (end (marker-position (ui-instance-region-end root-instance))))
    (when (and (>= pt start) (<= pt end))
      (goto-char pt)
      t)))

(defun ui--goto-instance-offset (instance context)
  "Move point to appropriate position within INSTANCE."
  (let ((start (marker-position (ui-instance-region-start instance)))
        (end (marker-position (ui-instance-region-end instance)))
        (offset (ui-cursor-context-offset-from-start context)))

    ;; For fields, try to preserve cursor position within text
    (if (and (ui-cursor-context-field-p context)
             (ui--primitive-field-p instance))
        (let ((field-pos (ui-cursor-context-field-cursor-pos context))
              (field-len (- end start)))
          (goto-char (+ start (min field-pos field-len))))

      ;; For other instances, use offset (clamped to bounds)
      (goto-char (+ start (min offset (- end start)))))))
```

## Special Handling for Editable Fields

Fields require extra care because the user may be mid-edit:

``` elisp
(defun ui--field-focused-p (instance)
  "Check if INSTANCE is a focused field (point is inside it)."
  (and (ui--primitive-field-p instance)
       (let ((pt (point))
             (start (marker-position (ui-instance-region-start instance)))
             (end (marker-position (ui-instance-region-end instance))))
         (and (>= pt start) (<= pt end)))))

(defun ui--preserve-field-edit (old-instance new-vnode commit-fn)
  "Special handling for re-rendering a field that's being edited.

If the field is focused and user has made changes, we have options:
1. Defer the update until field loses focus
2. Merge incoming value with user's edits
3. Preserve user's edits and ignore incoming value

This function returns the vnode to actually render."
  (if (ui--field-focused-p old-instance)
      ;; Field is focused  -  check for conflicts
      (let ((user-value (ui--get-field-buffer-content old-instance))
            (old-value (ui--primitive-field-value old-instance))
            (new-value (ui-vnode-field-value new-vnode)))

        (cond
         ;; User hasn't changed anything  -  safe to update
         ((string= user-value old-value)
          new-vnode)

         ;; Incoming value same as what user has  -  no conflict
         ((string= user-value new-value)
          new-vnode)

         ;; Conflict: user has edits AND incoming value differs
         ;; Strategy: preserve user's edits, schedule update for later
         (t
          (ui--schedule-deferred-field-update old-instance new-vnode)
          ;; Return vnode with user's current value
          (ui-vnode-field--create
           :key (ui-vnode-key new-vnode)
           :value user-value
           :size (ui-vnode-field-size new-vnode)
           :face (ui-vnode-field-face new-vnode)
           :on-change (ui-vnode-field-on-change new-vnode)
           :keymap (ui-vnode-field-keymap new-vnode)))))

    ;; Field not focused  -  update normally
    new-vnode))
```

## Mark and Region Preservation

``` elisp
(defun ui--restore-mark (context root-instance)
  "Restore mark position if possible."
  (when-let ((original-mark (ui-cursor-context-original-mark context)))
    (let ((start (marker-position (ui-instance-region-start root-instance)))
          (end (marker-position (ui-instance-region-end root-instance))))
      ;; Only restore if mark is still within bounds
      (when (and (>= original-mark start) (<= original-mark end))
        (push-mark original-mark t t)))))
```

## Integration with Render Cycle

The cursor preservation wraps the entire commit phase:

``` elisp
(defun ui--commit-phase (root-instance operations)
  "Apply OPERATIONS to buffer, preserving cursor position."
  (let ((context (ui--capture-cursor-context root-instance)))

    ;; Apply all operations
    (let ((inhibit-read-only t)
          (inhibit-modification-hooks t))
      (dolist (op (ui--sort-operations operations))
        (ui--apply-operation op)))

    ;; Restore cursor
    (ui--restore-cursor-context context root-instance)

    ;; Run post-commit hooks
    (ui--run-commit-hooks root-instance)))
```

# Layout System

## Buffer Constraints

Unlike CSS/browser layout, Emacs buffers have fundamental constraints:

- **Monospace**: All characters occupy the same width (mostly)
- **Linear**: Text flows left-to-right, top-to-bottom
- **No absolute positioning**: Can't place text at arbitrary x,y coordinates
- **No overlap**: Text can't overlap (overlays can, but not content)
- **Line-based**: Vertical positioning is in lines, not pixels

However, we can achieve flexible layouts using: - Whitespace for horizontal spacing - Newlines for vertical separation - Display properties for advanced alignment - Tables (using `org-table` or custom formatting)

## Layout Primitives

### Render Context

Layout primitives need access to a render context that tracks the current indentation level:

``` elisp
(cl-defstruct (ui-render-context (:constructor ui-render-context--create))
  "Context passed during rendering."
  indent-level            ; Current left margin (characters)
  indent-string           ; String to use for indentation (default spaces)
  column                  ; Current column position
  line-start-p)           ; Are we at the start of a line?

(defvar ui--render-context nil
  "Dynamically bound during rendering.")

(defun ui--current-indent ()
  "Get current indentation level."
  (if ui--render-context
      (ui-render-context-indent-level ui--render-context)
    0))

(defun ui--emit-indent ()
  "Emit indentation at start of line if needed."
  (when (and ui--render-context
             (ui-render-context-line-start-p ui--render-context)
             (> (ui-render-context-indent-level ui--render-context) 0))
    (insert (ui--make-indent-string))
    (setf (ui-render-context-line-start-p ui--render-context) nil)))

(defun ui--make-indent-string ()
  "Create indentation string for current level."
  (let ((level (ui-render-context-indent-level ui--render-context))
        (char (or (ui-render-context-indent-string ui--render-context) " ")))
    (if (= (length char) 1)
        (make-string level (string-to-char char))
      ;; Multi-char indent string (e.g., "│ ")
      (let ((result ""))
        (dotimes (_ level)
          (setq result (concat result char)))
        result))))
```

### Indent Component

The `indent` component establishes a left margin for all descendants:

``` elisp
(cl-defstruct (ui-vnode-indent (:include ui-vnode)
                                (:constructor ui-vnode-indent--create))
  "Indentation container."
  amount                  ; Number of characters to indent
  char                    ; Character/string for indent (default space)
  children)               ; Child vnodes

(defun ui-indent (amount &rest args)
  "Indent children by AMOUNT characters.

Usage:
  (ui-indent 2 child1 child2)
  (ui-indent 2 :char \"│ \" child1 child2)  ; tree-style indent"
  (let ((char " ")
        (children nil))

    (while (keywordp (car args))
      (pcase (pop args)
        (:char (setq char (pop args)))
        (:key nil)))

    (setq children (remq nil (flatten-list args)))

    (ui-vnode-indent--create
     :amount amount
     :char char
     :children children)))

(defun ui--render-indent (vnode parent-instance)
  "Render an indent container."
  (let* ((amount (ui-vnode-indent-amount vnode))
         (char (or (ui-vnode-indent-char vnode) " "))
         (old-context ui--render-context)
         (new-context (ui-render-context--create
                       :indent-level (+ (ui--current-indent) amount)
                       :indent-string char
                       :column (if old-context
                                   (ui-render-context-column old-context)
                                 0)
                       :line-start-p (if old-context
                                         (ui-render-context-line-start-p old-context)
                                       t))))

    ;; Render children with increased indent
    (let ((ui--render-context new-context))
      (dolist (child (ui-vnode-indent-children vnode))
        (ui--render-vnode child parent-instance)))))
```

**Indent propagation for newlines:**

``` elisp
(defun ui--render-newline (vnode parent-instance)
  "Render a newline, respecting indent context."
  (insert "\n")

  ;; Mark that we're at line start (indent will be emitted on next content)
  (when ui--render-context
    (setf (ui-render-context-line-start-p ui--render-context) t)
    (setf (ui-render-context-column ui--render-context) 0))

  (ui--create-primitive-instance 'newline vnode parent-instance
                                  (1- (point)) (point)))

(defun ui--render-text (vnode parent-instance)
  "Render text, emitting indent at line starts."
  (let ((start (point))
        (content (ui-vnode-text-content vnode))
        (face (ui-vnode-text-face vnode)))

    ;; Handle multi-line content
    (let ((lines (split-string content "\n")))
      (dotimes (i (length lines))
        (when (> i 0)
          (insert "\n")
          (when ui--render-context
            (setf (ui-render-context-line-start-p ui--render-context) t)))

        ;; Emit indent if at line start
        (ui--emit-indent)

        ;; Insert line content
        (insert (nth i lines))))

    ;; Apply face
    (when face
      (put-text-property start (point) 'face face))

    (ui--create-primitive-instance 'text vnode parent-instance start (point))))
```

### Fragment (No Layout)

Simply concatenates children:

``` elisp
(defun ui-fragment (&rest children)
  "Group children with no additional layout."
  (ui-vnode-fragment--create
   :children (remq nil (flatten-list children))))
```

### Horizontal Stack (h-stack)

Places children horizontally with optional spacing:

``` elisp
(cl-defstruct (ui-vnode-hstack (:include ui-vnode)
                                (:constructor ui-vnode-hstack--create))
  "Horizontal layout container."
  children                ; List of child vnodes
  spacing                 ; Spaces between children (default 1)
  align)                  ; :left, :center, :right (for fixed-width containers)

(defun ui-hstack (&rest args)
  "Create a horizontal stack layout.

Usage: (ui-hstack child1 child2 child3)
       (ui-hstack :spacing 2 child1 child2)
       (ui-hstack :align :center :width 40 child1 child2)"
  (let ((spacing 1)
        (align :left)
        (width nil)
        (children nil))

    ;; Parse keyword arguments
    (while (keywordp (car args))
      (pcase (pop args)
        (:spacing (setq spacing (pop args)))
        (:align (setq align (pop args)))
        (:width (setq width (pop args)))
        (:key nil))) ; handled by vnode

    ;; Remaining args are children
    (setq children (remq nil (flatten-list args)))

    (ui-vnode-hstack--create
     :children children
     :spacing spacing
     :align align)))
```

Rendering h-stack:

``` elisp
(defun ui--render-hstack (vnode)
  "Render a horizontal stack to buffer."
  (let ((spacing (or (ui-vnode-hstack-spacing vnode) 1))
        (children (ui-vnode-hstack-children vnode))
        (space-str nil))

    (setq space-str (make-string spacing ?\s))

    (let ((first t))
      (dolist (child children)
        (unless first
          (insert space-str))
        (ui--render-vnode child)
        (setq first nil)))))
```

### Vertical Stack (v-stack)

Places children vertically:

``` elisp
(cl-defstruct (ui-vnode-vstack (:include ui-vnode)
                                (:constructor ui-vnode-vstack--create))
  "Vertical layout container."
  children                ; List of child vnodes
  spacing                 ; Blank lines between children (default 0)
  indent)                 ; Left indent for all children (default 0)

(defun ui-vstack (&rest args)
  "Create a vertical stack layout.

Usage: (ui-vstack child1 child2 child3)
       (ui-vstack :spacing 1 child1 child2)  ; blank line between
       (ui-vstack :indent 2 child1 child2)   ; 2-space indent"
  (let ((spacing 0)
        (indent 0)
        (children nil))

    (while (keywordp (car args))
      (pcase (pop args)
        (:spacing (setq spacing (pop args)))
        (:indent (setq indent (pop args)))
        (:key nil)))

    (setq children (remq nil (flatten-list args)))

    (ui-vnode-vstack--create
     :children children
     :spacing spacing
     :indent indent)))
```

Rendering v-stack:

``` elisp
(defun ui--render-vstack (vnode)
  "Render a vertical stack to buffer."
  (let ((spacing (or (ui-vnode-vstack-spacing vnode) 0))
        (indent (or (ui-vnode-vstack-indent vnode) 0))
        (children (ui-vnode-vstack-children vnode))
        (indent-str nil)
        (spacing-str nil))

    (setq indent-str (make-string indent ?\s))
    (setq spacing-str (concat "\n" (make-string spacing ?\n)))

    (let ((first t))
      (dolist (child children)
        (unless first
          (insert spacing-str))
        (insert indent-str)
        (ui--render-vnode child)
        (insert "\n")
        (setq first nil)))))
```

### Box (Fixed-Width Container)

A container with fixed width, supporting alignment:

``` elisp
(cl-defstruct (ui-vnode-box (:include ui-vnode)
                             (:constructor ui-vnode-box--create))
  "Fixed-width container with alignment."
  child                   ; Single child vnode
  width                   ; Width in characters
  align                   ; :left, :center, :right
  padding                 ; (left . right) padding
  border                  ; nil or border character
  fill)                   ; Character to fill empty space

(defun ui-box (&rest args)
  "Create a fixed-width box.

Usage: (ui-box :width 20 :align :center child)"
  (let ((width 20)
        (align :left)
        (padding '(0 . 0))
        (border nil)
        (fill ?\s)
        (child nil))

    (while (keywordp (car args))
      (pcase (pop args)
        (:width (setq width (pop args)))
        (:align (setq align (pop args)))
        (:padding (setq padding (pop args)))
        (:border (setq border (pop args)))
        (:fill (setq fill (pop args)))
        (:key nil)))

    (setq child (car args))

    (ui-vnode-box--create
     :child child
     :width width
     :align align
     :padding padding
     :border border
     :fill fill)))
```

## Table Layout

Tables are common in Emacs UIs (think `tabulated-list-mode`).

### Column Specification

Each column can specify:

| Property | Values | Description |
|----|----|----|
| `:width` | Integer | Fixed width in characters |
| `:min-width` | Integer | Minimum width (auto-expand for content) |
| `:max-width` | Integer | Maximum width (truncate if exceeded) |
| `:overflow` | `:truncate`, `:ellipsis` | What to do when content exceeds max-width |
| `:align` | `:left`, `:center`, `:right` | Text alignment |
| `:header` | String | Header text |

**Layout behaviour**: - If only `:width` specified: fixed width, truncate overflow - If `:min-width` specified: auto-size to content, but at least this wide - If `:max-width` specified: auto-size but truncate beyond this - If both `:min-width` and `:max-width`: auto-size within range

**Re-measurement**: By default, tables re-measure all cell widths on every render. This ensures the layout adapts when content changes (e.g., a score updates from "4.1" to "4.12", or a name gets longer).

**Static optimisation**: For tables with fixed content, `:static t` skips re-measurement and reuses cached column widths. Use when you know content won't change size.

``` elisp
(cl-defstruct (ui-vnode-table (:include ui-vnode)
                               (:constructor ui-vnode-table--create))
  "Table layout with rows and columns."
  columns                 ; List of column specs
  rows                    ; List of rows, each row is list of cell vnodes
  header-p                ; Show header row?
  border                  ; nil, :ascii, :unicode
  stripe-face             ; Face for alternating rows
  static-p)               ; If t, skip re-measurement (optimisation)

(defun ui-table (&rest args)
  "Create a table layout.

Column spec properties:
  :width N        - Fixed width (ignores content size)
  :min-width N    - Minimum width, expand for content
  :max-width N    - Maximum width, truncate beyond
  :overflow MODE  - :truncate (default) or :ellipsis
  :align ALIGN    - :left (default), :center, :right
  :header STRING  - Header text

Table properties:
  :static t       - Skip re-measurement (use when content size won't change)
  :border MODE    - nil, :ascii, :unicode
  :stripe FACE    - Face for alternating rows

Usage:
  (ui-table
    :columns ((:min-width 5 :max-width 20 :header \"Name\")
              (:width 7 :header \"Price\" :align :right)
              (:width 5 :header \"Qty\" :align :right))
    :rows ((\"Тру Амерікан Бургер\" \"395 UAH\" \"3\")
           (\"Бейбі Бургер\" \"260 UAH\" \"2\")))"
  ...)

(cl-defstruct (ui-vnode-row (:include ui-vnode)
                             (:constructor ui-vnode-row--create))
  "A table row."
  cells                   ; List of cell vnodes
  header-p)               ; Is this a header row?

(cl-defstruct (ui-vnode-cell (:include ui-vnode)
                              (:constructor ui-vnode-cell--create))
  "A table cell."
  content                 ; Child vnode
  colspan                 ; Column span (default 1)
  align)                  ; Override column alignment
```

Table rendering (uses two-pass for width calculation):

``` elisp
(defun ui--render-table (vnode)
  "Render a table to buffer."
  (let* ((columns (ui-vnode-table-columns vnode))
         (rows (ui-vnode-table-rows vnode))
         (border (ui-vnode-table-border vnode))
         ;; Pass 1: Calculate column widths
         (col-widths (ui--calculate-column-widths columns rows)))

    ;; Pass 2: Render with known widths
    ;; Render header if present
    (when (ui-vnode-table-header-p vnode)
      (ui--render-table-row
       (mapcar (lambda (c) (plist-get c :header)) columns)
       col-widths
       columns
       border
       'header))

    ;; Render separator
    (when (and (ui-vnode-table-header-p vnode) border)
      (ui--render-table-separator col-widths border))

    ;; Render data rows
    (let ((row-idx 0))
      (dolist (row rows)
        (ui--render-table-row row col-widths columns border
                              (if (cl-evenp row-idx) 'even 'odd))
        (cl-incf row-idx)))))

(defun ui--calculate-column-widths (columns rows static-p cached-widths)
  "Calculate actual column widths based on specs and content.

If STATIC-P and CACHED-WIDTHS exist, return cached.
Otherwise measure all cells and compute widths respecting constraints."
  (if (and static-p cached-widths)
      cached-widths
    ;; Measure content width for each column
    (let* ((num-cols (length columns))
           (content-widths (make-vector num-cols 0)))

      ;; Measure header widths
      (cl-loop for col in columns
               for i from 0
               do (when-let ((header (plist-get col :header)))
                    (aset content-widths i
                          (max (aref content-widths i)
                               (string-width header)))))

      ;; Measure cell content widths
      (dolist (row rows)
        (cl-loop for cell in row
                 for i from 0
                 do (let ((width (ui--measure-cell-width cell)))
                      (aset content-widths i
                            (max (aref content-widths i) width)))))

      ;; Apply constraints from column specs
      (cl-loop for col in columns
               for i from 0
               collect (ui--apply-width-constraints
                        col (aref content-widths i))))))

(defun ui--measure-cell-width (cell)
  "Measure the display width of CELL content."
  (cond
   ((stringp cell) (string-width cell))
   ((ui-vnode-p cell)
    ;; Render to temp buffer and measure
    (with-temp-buffer
      (ui--render-vnode-for-measure cell)
      (goto-char (point-min))
      ;; Width is max line length (in case of unexpected newlines)
      (let ((max-width 0))
        (while (not (eobp))
          (setq max-width (max max-width (- (line-end-position)
                                            (line-beginning-position))))
          (forward-line 1))
        max-width)))
   (t 0)))

(defun ui--apply-width-constraints (col-spec content-width)
  "Apply column constraints to CONTENT-WIDTH.

Returns the final column width."
  (let ((fixed (plist-get col-spec :width))
        (min-w (plist-get col-spec :min-width))
        (max-w (plist-get col-spec :max-width)))
    (cond
     ;; Fixed width overrides everything
     (fixed fixed)
     ;; Apply min/max constraints
     (t (let ((width content-width))
          (when min-w (setq width (max width min-w)))
          (when max-w (setq width (min width max-w)))
          width)))))

(defun ui--render-table-row (cells widths columns border row-type)
  "Render a single table row."
  (let ((border-char (pcase border
                       (:ascii "|")
                       (:unicode "│")
                       (_ ""))))

    (insert border-char)
    (cl-loop for cell in cells
             for width in widths
             for col in columns
             do (progn
                  (ui--render-table-cell cell width (plist-get col :align))
                  (insert border-char)))
    (insert "\n")))

(defun ui--render-table-cell (content width align)
  "Render a cell with padding to WIDTH."
  (let* ((text (if (stringp content)
                   content
                 (with-temp-buffer
                   (ui--render-vnode content)
                   (buffer-string))))
         (text-width (string-width text))
         (padding (- width text-width))
         (left-pad (pcase align
                     (:right padding)
                     (:center (/ padding 2))
                     (_ 0)))
         (right-pad (- padding left-pad)))

    (insert (make-string (max 0 left-pad) ?\s))
    (insert (truncate-string-to-width text width))
    (insert (make-string (max 0 right-pad) ?\s))))
```

## List Layout

For rendering lists with bullets or numbers:

``` elisp
(cl-defstruct (ui-vnode-list (:include ui-vnode)
                              (:constructor ui-vnode-list--create))
  "List layout with markers."
  items                   ; List of item vnodes
  marker-fn               ; (lambda (index) marker-string) or :bullet, :number
  indent)                 ; Indent for continuation lines

(defun ui-list (&rest args)
  "Create a list layout.

Usage:
  (ui-list :marker :bullet item1 item2 item3)
  (ui-list :marker :number item1 item2)
  (ui-list :marker (lambda (i) (format \"%c) \" (+ ?a i))) item1 item2)"
  (let ((marker :bullet)
        (indent 2)
        (items nil))

    (while (keywordp (car args))
      (pcase (pop args)
        (:marker (setq marker (pop args)))
        (:indent (setq indent (pop args)))
        (:key nil)))

    (setq items (remq nil (flatten-list args)))

    (ui-vnode-list--create
     :items items
     :marker-fn (pcase marker
                  (:bullet (lambda (_i) "• "))
                  (:dash (lambda (_i) "- "))
                  (:number (lambda (i) (format "%d. " (1+ i))))
                  (:alpha (lambda (i) (format "%c) " (+ ?a i))))
                  ((pred functionp) marker)
                  (_ (lambda (_i) "• ")))
     :indent indent)))
```

## Conditional Layout

For responsive layouts based on buffer/window width:

``` elisp
(defun ui-when-wide (min-width then-vnode &optional else-vnode)
  "Render THEN-VNODE if window is at least MIN-WIDTH columns.

Otherwise render ELSE-VNODE (or nothing if nil)."
  (if (>= (window-width) min-width)
      then-vnode
    else-vnode))

(defun ui-responsive (&rest breakpoints)
  "Choose layout based on window width.

BREAKPOINTS is a plist of (WIDTH VNODE ...).
Returns the vnode for the largest width that fits.

Usage:
  (ui-responsive
    80 (ui-hstack col1 col2 col3)   ; Wide: 3 columns
    60 (ui-hstack col1 (ui-vstack col2 col3))  ; Medium: 2 columns
    0  (ui-vstack col1 col2 col3))  ; Narrow: 1 column"
  (let ((width (window-width))
        (result nil))
    (cl-loop for (min-w vnode) on breakpoints by #'cddr
             when (>= width min-w)
             do (setq result vnode)
             and return nil)
    (or result (ui-fragment))))
```

## Spacing and Alignment Utilities

``` elisp
(defun ui-pad (padding child)
  "Add PADDING around CHILD.

PADDING can be:
  - Integer: same padding all around
  - (H . V): horizontal and vertical
  - (L T R B): left, top, right, bottom"
  ...)

(defun ui-align (alignment child)
  "Align CHILD within available space.

ALIGNMENT is :left, :center, or :right."
  ...)

(defun ui-separator (&optional char width)
  "Create a horizontal separator line."
  (ui-vnode-text--create
   :content (make-string (or width (window-width)) (or char ?─))
   :face 'shadow))

(defun ui-blank-lines (n)
  "Create N blank lines."
  (ui-vnode-text--create
   :content (make-string n ?\n)))
```

## Layout Edge Cases and Constraints

### Multi-line Content in Horizontal Layouts

When a child in `h-stack` contains newlines, the layout breaks. We have options:

1.  **Forbid**: Throw error if child contains newlines (simplest)
2.  **Flatten**: Replace newlines with spaces
3.  **Complex layout**: Track columns and align subsequent lines (complex)

**Decision**: For MVP, we forbid newlines in h-stack children. Use v-stack for multi-line content.

``` elisp
(defun ui--validate-hstack-child (vnode)
  "Ensure VNODE doesn't contain newlines."
  (when (ui-vnode-text-p vnode)
    (when (string-match-p "\n" (ui-vnode-text-content vnode))
      (error "h-stack children cannot contain newlines"))))
```

### Text Wrapping

The library does **not** handle text wrapping. Content that exceeds window width will either: - Extend beyond visible area (if `truncate-lines` is t) - Wrap at window edge (if `truncate-lines` is nil)

For controlled wrapping, use `fill-region` on content before passing to components, or implement a `ui-wrapped-text` component that pre-wraps.

### Read-Only Regions

Text that shouldn't be edited needs protection:

``` elisp
(defun ui--render-text (vnode parent-instance)
  "Render text with read-only protection."
  (let ((start (point))
        (content (ui-vnode-text-content vnode)))

    (ui--emit-indent)
    (insert content)

    ;; Make non-field text read-only
    (put-text-property start (point) 'read-only t)
    (put-text-property start (point) 'front-sticky '(read-only))
    (put-text-property start (point) 'rear-nonsticky '(read-only))

    ...))
```

Fields explicitly remove read-only in their region.

## Focus and Navigation

## Keyboard Navigation

Users expect TAB/S-TAB to move between interactive elements (fields, buttons):

``` elisp
(defvar ui-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "TAB") #'ui-next-field)
    (define-key map (kbd "<tab>") #'ui-next-field)
    (define-key map (kbd "S-TAB") #'ui-prev-field)
    (define-key map (kbd "<backtab>") #'ui-prev-field)
    (define-key map (kbd "RET") #'ui-activate)
    map)
  "Keymap for UI navigation.")

(defun ui-next-field ()
  "Move to next interactive element."
  (interactive)
  (let ((start (point))
        (found nil))
    ;; Search forward for next button/field overlay
    (while (and (not found) (< (point) (point-max)))
      (goto-char (next-overlay-change (point)))
      (when (ui--interactive-overlay-at (point))
        (setq found t)))

    ;; Wrap around
    (unless found
      (goto-char (point-min))
      (while (and (not found) (< (point) start))
        (goto-char (next-overlay-change (point)))
        (when (ui--interactive-overlay-at (point))
          (setq found t))))

    (unless found
      (goto-char start)
      (message "No interactive elements"))))

(defun ui--interactive-overlay-at (pos)
  "Check for interactive overlay at POS."
  (cl-find-if (lambda (ov)
                (or (overlay-get ov 'ui-button)
                    (overlay-get ov 'ui-field)))
              (overlays-at pos)))
```

## Focus Tracking

``` elisp
(defvar-local ui--focused-instance nil
  "Currently focused instance, if any.")

(defun ui-focus (instance)
  "Set focus to INSTANCE."
  (when ui--focused-instance
    (ui--blur ui--focused-instance))
  (setq ui--focused-instance instance)
  (when instance
    (goto-char (marker-position (ui-instance-region-start instance)))
    (run-hooks 'ui-focus-hook)))
```

## Undo Integration

Buffer changes should integrate with Emacs undo:

``` elisp
(defun ui--commit-operations (operations root-instance)
  "Apply OPERATIONS with undo integration."
  (let ((inhibit-read-only t))

    ;; Group all changes as single undo step
    (undo-boundary)

    (with-current-buffer (ui-instance-buffer root-instance)
      (let ((sorted-ops (ui--sort-operations operations)))
        (dolist (op sorted-ops)
          (ui--apply-operation op))))

    (undo-boundary)))
```

**Limitation**: Undo will restore buffer text but not component state. After undo, the UI may be inconsistent. Options:

1.  **Disable undo** in UI buffers (`buffer-undo-list` = t)
2.  **State snapshots**: Store state before changes, restore on undo (complex)
3.  **Accept inconsistency**: Document that undo may break UI (pragmatic)

For MVP, we disable undo in UI buffers and provide explicit undo-like actions (e.g., "Undo last change" button).

## Buffer Narrowing

Our markers should work correctly with narrowing, but rendering assumes full buffer access:

``` elisp
(defun ui-render (vnode &optional buffer position)
  "Render VNODE. Temporarily widens buffer if narrowed."
  (save-restriction
    (widen)
    ;; ... rendering logic
    ))
```

## Multiple Roots Interaction

When multiple independent UI trees exist in one buffer:

1.  **Non-overlapping regions**: Each root has exclusive markers; they shouldn't overlap
2.  **Independent updates**: Re-rendering one root doesn't affect others
3.  **Potential issue**: Inserting text in one tree shifts positions of later trees

``` elisp
(defun ui--validate-non-overlapping (new-root buffer)
  "Ensure NEW-ROOT doesn't overlap existing roots."
  (let ((new-start (marker-position (ui-instance-region-start new-root)))
        (new-end (marker-position (ui-instance-region-end new-root))))
    (dolist (existing (ui--buffer-roots buffer))
      (let ((ex-start (marker-position (ui-instance-region-start existing)))
            (ex-end (marker-position (ui-instance-region-end existing))))
        (when (and (< new-start ex-end) (> new-end ex-start))
          (error "UI regions cannot overlap"))))))
```

``` elisp
(defcomponent dashboard
  "A multi-column dashboard layout."
  :props ((title :type string)
          (stats :type list)
          (items :type list))

  :render
  (ui-vstack
    ;; Header
    (ui-hstack
      (ui-text title :face 'bold)
      (ui-space 4)
      (ui-text (format-time-string "%Y-%m-%d") :face 'shadow))

    (ui-separator)

    ;; Stats row
    (ui-hstack :spacing 4
      (mapcar (lambda (stat)
                (ui-box :width 15 :align :center
                  (ui-vstack
                    (ui-text (plist-get stat :value) :face 'bold)
                    (ui-text (plist-get stat :label) :face 'shadow))))
              stats))

    (ui-blank-lines 1)

    ;; Main content (responsive)
    (ui-responsive
      100 (ui-hstack :spacing 2
            (ui-box :width 48 (sidebar))
            (ui-box :width 48 (main-content items)))
      0 (ui-vstack
          (sidebar)
          (ui-separator)
          (main-content items)))))
```

# State Management

## State Model Overview

State in this system follows React's model:

1.  **Local state**: Owned by a component, triggers re-render when changed
2.  **Props**: Passed from parent, read-only to child
3.  **Context**: Shared across tree without prop drilling
4.  **Effects**: Side effects triggered by state/prop changes

``` example
+-----------------+
|   Root State    |
|  (context/store)|
+--------+--------+
         | context
+--------v--------+
|   Parent        |
| +-------------+ |
| | local state | |
| +-------------+ |
+--------+--------+
         | props (v) + callbacks (^)
+--------v--------+
|   Child         |
| +-------------+ |
| | local state | |
| +-------------+ |
+-----------------+
```

## Local State Implementation

Each component instance has a state slot. Changes to state mark the instance dirty and schedule a re-render.

``` elisp
(defvar ui--current-instance nil
  "Dynamically bound to the current instance during render.")

(defun ui-set-state! (instance key value)
  "Set state KEY to VALUE in INSTANCE, triggering re-render."
  (let* ((old-state (ui-instance-state instance))
         (old-value (plist-get old-state key)))

    ;; Only update if value changed
    (unless (equal old-value value)
      ;; Update state
      (setf (ui-instance-state instance)
            (plist-put (copy-sequence old-state) key value))

      ;; Mark dirty and schedule re-render
      (ui--mark-dirty instance)
      (ui--schedule-render (ui--root-instance instance)))))

(defun ui-get-state (instance key)
  "Get state KEY from INSTANCE."
  (plist-get (ui-instance-state instance) key))

;; Generalised variable for setf support
(gv-define-setter ui-get-state (value instance key)
  `(ui-set-state! ,instance ,key ,value))
```

## The `let-state` Macro

Provides convenient syntax for declaring and using state:

``` elisp
(defmacro let-state (bindings &rest body)
  "Bind state variables with automatic re-render on mutation.

BINDINGS is a list of (NAME INITIAL) or (NAME INITIAL SETTER-NAME).

Within BODY, NAME is readable and settable via `setf`.
If SETTER-NAME is provided, it's bound to a setter function.

Example:
  (let-state ((count 0)
              (name \"\" set-name))
    (ui-button :on-click (lambda () (cl-incf count)))
    (ui-button :on-click (lambda () (funcall set-name \"Bob\"))))"

  (declare (indent 1))

  (let ((instance-sym (gensym "instance"))
        (state-bindings nil)
        (setter-bindings nil)
        (gv-bindings nil))

    ;; Parse bindings
    (dolist (binding bindings)
      (let* ((name (nth 0 binding))
             (initial (nth 1 binding))
             (setter-name (nth 2 binding))
             (key (intern (format ":%s" name))))

        ;; State accessor binding
        (push `(,name (ui-get-state ,instance-sym ,key)) state-bindings)

        ;; Setter binding (if named)
        (when setter-name
          (push `(,setter-name
                  (lambda (v) (ui-set-state! ,instance-sym ,key v)))
                setter-bindings))

        ;; GV expander for setf
        (push `(,name (lambda (v) (ui-set-state! ,instance-sym ,key v)))
              gv-bindings)))

    ;; Generate expansion
    `(let ((,instance-sym ui--current-instance))
       ;; Initialise state if needed (first render)
       (unless (ui-instance-state ,instance-sym)
         (setf (ui-instance-state ,instance-sym)
               (list ,@(cl-loop for b in bindings
                                collect (intern (format ":%s" (car b)))
                                collect (cadr b)))))

       ;; Bind state accessors
       (let (,@(nreverse state-bindings)
             ,@(nreverse setter-bindings))

         ;; Set up symbol-macrolet for setf support
         (cl-symbol-macrolet
             ,(cl-loop for (name setter-fn) in gv-bindings
                       collect `(,name
                                 (ui--state-place ,instance-sym
                                                  ,(intern (format ":%s" name)))))
           ,@body)))))

;; Define a gv-expander for our state place
(gv-define-expander ui--state-place
  (lambda (do &rest args)
    (gv-letplace (getter setter) `(ui-get-state ,@args)
      (funcall do getter
               (lambda (v)
                 `(ui-set-state! ,(car args) ,(cadr args) ,v))))))
```

## Derived State (Computed Values)

Sometimes state is derived from props or other state. We provide a memoisation helper:

``` elisp
(defmacro let-memo (bindings &rest body)
  "Compute and cache derived values.

BINDINGS is a list of (NAME DEPS COMPUTE-FN).
VALUE is recomputed only when DEPS change.

Example:
  (let-memo ((filtered-items (items filter-text)
               (seq-filter (lambda (i) (string-match filter-text i)) items)))
    (ui-list :items filtered-items))"

  (declare (indent 1))

  (let ((instance-sym (gensym "instance"))
        (memo-bindings nil))

    (dolist (binding bindings)
      (let ((name (nth 0 binding))
            (deps (nth 1 binding))
            (compute (nth 2 binding))
            (cache-key (gensym "memo-cache")))

        (push `(,name
                (ui--memo-get-or-compute
                 ,instance-sym
                 ',cache-key
                 (list ,@deps)
                 (lambda () ,compute)))
              memo-bindings)))

    `(let ((,instance-sym ui--current-instance))
       (let (,@(nreverse memo-bindings))
         ,@body))))

(defun ui--memo-get-or-compute (instance cache-key deps compute-fn)
  "Get cached value or compute and cache it."
  (let* ((memo-cache (ui-instance-memo-cache instance))
         (cached (gethash cache-key memo-cache))
         (cached-deps (car cached))
         (cached-value (cdr cached)))

    (if (and cached (equal cached-deps deps))
        cached-value
      (let ((new-value (funcall compute-fn)))
        (puthash cache-key (cons deps new-value) memo-cache)
        new-value))))
```

## Effects (Side Effects on State Change)

Effects run after render when dependencies change:

``` elisp
(defmacro use-effect (deps &rest body)
  "Run BODY as a side effect when DEPS change.

DEPS is a list of variables. Effect runs:
- After first render
- After re-render if any dep changed

If BODY returns a function, it's called as cleanup before
next effect run or on unmount.

Example:
  (use-effect (user-id)
    (fetch-user-data user-id)
    (lambda () (cancel-fetch)))  ; cleanup"

  (declare (indent 1))

  (let ((instance-sym (gensym "instance"))
        (effect-id (gensym "effect")))

    `(let ((,instance-sym ui--current-instance))
       (ui--register-effect
        ,instance-sym
        ',effect-id
        (list ,@deps)
        (lambda () ,@body)))))

(defun ui--register-effect (instance effect-id deps effect-fn)
  "Register an effect to run after commit."
  (let* ((effects (ui-instance-effects instance))
         (prev-effect (assq effect-id effects))
         (prev-deps (nth 1 prev-effect)))

    ;; Check if deps changed
    (when (or (null prev-effect)
              (not (equal prev-deps deps)))

      ;; Schedule effect for post-commit
      (push (list effect-id deps effect-fn (nth 3 prev-effect))
            (ui-scheduler-pending-effects (ui--get-scheduler))))))

(defun ui--run-pending-effects ()
  "Run all pending effects after commit."
  (let ((effects (ui-scheduler-pending-effects (ui--get-scheduler))))
    (setf (ui-scheduler-pending-effects (ui--get-scheduler)) nil)

    (dolist (effect effects)
      (let ((effect-id (nth 0 effect))
            (deps (nth 1 effect))
            (effect-fn (nth 2 effect))
            (cleanup-fn (nth 3 effect)))

        ;; Run cleanup from previous effect
        (when cleanup-fn
          (funcall cleanup-fn))

        ;; Run new effect, capture cleanup
        (let ((new-cleanup (funcall effect-fn)))
          ;; Store cleanup for next time
          (when (functionp new-cleanup)
            (ui--store-cleanup effect-id new-cleanup)))))))
```

## Refs (Mutable References Without Re-render)

Sometimes you need mutable state that doesn't trigger re-render:

``` elisp
(defmacro use-ref (initial-value)
  "Create a mutable ref that doesn't trigger re-render.

Returns a cons cell (value . nil). Access via (car ref), set via (setcar ref v).

Useful for:
- Storing previous values
- Holding timer/process references
- Caching expensive computations

Example:
  (let ((timer-ref (use-ref nil)))
    (use-effect ()
      (setcar timer-ref (run-with-timer 1 1 #'update))
      (lambda () (cancel-timer (car timer-ref)))))"

  (let ((instance-sym (gensym "instance"))
        (ref-id (gensym "ref")))

    `(let ((,instance-sym ui--current-instance))
       (ui--get-or-create-ref ,instance-sym ',ref-id ,initial-value))))

(defun ui--get-or-create-ref (instance ref-id initial-value)
  "Get existing ref or create new one."
  (let ((refs (ui-instance-refs instance)))
    (or (gethash ref-id refs)
        (let ((ref (cons initial-value nil)))
          (puthash ref-id ref refs)
          ref))))
```

## Callbacks (Stable Function References)

To avoid re-renders when passing callbacks, we stabilise them:

``` elisp
(defmacro use-callback (deps &rest body)
  "Create a memoised callback that only changes when DEPS change.

Useful for callbacks passed to child components to prevent
unnecessary re-renders.

Example:
  (let ((handle-click (use-callback (item-id)
                        (lambda () (delete-item item-id)))))
    (ui-button :on-click handle-click))"

  (declare (indent 1))

  `(ui--memo-callback ui--current-instance
                      (list ,@deps)
                      (lambda () ,@body)))

(defun ui--memo-callback (instance deps make-callback)
  "Return cached callback or create new one if deps changed."
  (let* ((cache (ui-instance-callback-cache instance))
         (cache-key (sxhash make-callback))
         (cached (gethash cache-key cache))
         (cached-deps (car cached))
         (cached-fn (cdr cached)))

    (if (and cached (equal cached-deps deps))
        cached-fn
      (let ((new-fn (funcall make-callback)))
        (puthash cache-key (cons deps new-fn) cache)
        new-fn))))
```

## State Update Batching

Multiple state changes should be batched into a single re-render:

``` elisp
(defvar ui--batch-depth 0
  "Nesting depth of batch operations.")

(defvar ui--batched-updates nil
  "List of (instance . state-updates) pending in current batch.")

(defmacro ui-batch (&rest body)
  "Batch state updates in BODY into a single re-render."
  `(let ((ui--batch-depth (1+ ui--batch-depth)))
     (unwind-protect
         (progn ,@body)
       (when (= ui--batch-depth 1)
         (ui--flush-batched-updates)))))

(defun ui--flush-batched-updates ()
  "Apply all batched updates and trigger single re-render."
  (let ((updates ui--batched-updates))
    (setq ui--batched-updates nil)

    ;; Group updates by instance
    (let ((by-instance (make-hash-table :test 'eq)))
      (dolist (update updates)
        (let ((instance (car update))
              (state-changes (cdr update)))
          (puthash instance
                   (append (gethash instance by-instance) state-changes)
                   by-instance)))

      ;; Apply updates and mark dirty
      (maphash (lambda (instance changes)
                 (let ((state (ui-instance-state instance)))
                   (dolist (change changes)
                     (setq state (plist-put state (car change) (cdr change))))
                   (setf (ui-instance-state instance) state)
                   (ui--mark-dirty instance)))
               by-instance))

    ;; Schedule single re-render
    (ui--schedule-render-if-needed)))
```

## Reducer Pattern (Complex State Logic)

For complex state transitions, a reducer pattern can help:

``` elisp
(defmacro use-reducer (reducer initial-state)
  "Manage state with a reducer function.

REDUCER is (lambda (state action) new-state).
Returns (state . dispatch) where dispatch is (lambda (action) ...).

Example:
  (let* ((result (use-reducer #'my-reducer initial-state))
         (state (car result))
         (dispatch (cdr result)))
    (ui-button :on-click (lambda () (funcall dispatch '(:type increment)))))"

  (let ((instance-sym (gensym "instance")))

    `(let ((,instance-sym ui--current-instance))
       (let-state ((state ,initial-state))
         (cons state
               (lambda (action)
                 (setf state (funcall ,reducer state action))))))))

;; Example reducer
(defun todo-reducer (state action)
  "Reducer for todo list state."
  (pcase (plist-get action :type)
    ('add-todo
     (let ((text (plist-get action :text)))
       (plist-put state :todos
                  (cons (list :id (cl-gensym) :text text :done nil)
                        (plist-get state :todos)))))

    ('toggle-todo
     (let ((id (plist-get action :id)))
       (plist-put state :todos
                  (mapcar (lambda (todo)
                            (if (equal (plist-get todo :id) id)
                                (plist-put todo :done
                                           (not (plist-get todo :done)))
                              todo))
                          (plist-get state :todos)))))

    ('delete-todo
     (let ((id (plist-get action :id)))
       (plist-put state :todos
                  (cl-remove-if (lambda (todo)
                                  (equal (plist-get todo :id) id))
                                (plist-get state :todos)))))
    (_ state)))

---

* Context System

** Purpose

Context solves the "prop drilling" problem  -  passing data through many layers of components that don't need it, just to reach a deeply nested component.

Without context:
```

App (theme) → Layout (theme) → Sidebar (theme) → Button (uses theme)

``` example

With context:
```

App (provides theme) → … → Button (consumes theme directly)

``` example

** Creating a Context

#+begin_src elisp
(defmacro defcontext (name &optional default-value docstring)
  "Define a context NAME with optional DEFAULT-VALUE.

Creates:
- `NAME-context`: The context object
- `NAME-provider`: Function to create a provider vnode
- `use-NAME`: Function to consume the context value

Example:
  (defcontext theme 'light \"The current UI theme.\")"

  (declare (doc-string 3))

  (let ((context-var (intern (format "%s-context" name)))
        (provider-fn (intern (format "%s-provider" name)))
        (consumer-fn (intern (format "use-%s" name))))

    `(progn
       ;; The context object
       (defvar ,context-var
         (ui-context--create
          :name ',name
          :default-value ,default-value)
         ,(or docstring (format "Context for %s." name)))

       ;; Provider function
       (defun ,provider-fn (value &rest children)
         ,(format "Provide %s context with VALUE to CHILDREN." name)
         (ui-vnode-provider--create
          :context ,context-var
          :value value
          :children children))

       ;; Consumer hook
       (defun ,consumer-fn ()
         ,(format "Get current %s context value." name)
         (ui--consume-context ,context-var)))))
```

## Context Data Structures

``` elisp
(cl-defstruct (ui-context (:constructor ui-context--create))
  "A context definition."
  name                    ; Symbol identifying this context
  default-value)          ; Value when no provider found

(cl-defstruct (ui-vnode-provider (:include ui-vnode)
                                  (:constructor ui-vnode-provider--create))
  "A context provider vnode."
  context                 ; The ui-context being provided
  value                   ; The value to provide
  children)               ; Child vnodes

(cl-defstruct (ui-context-binding (:constructor ui-context-binding--create))
  "Runtime binding of a context to a value."
  context                 ; The ui-context
  value                   ; Current provided value
  provider-instance)      ; The instance that's providing this
```

## Context Resolution

During render, we maintain a stack of context bindings:

``` elisp
(defvar ui--context-stack nil
  "Stack of context bindings during render.
Each entry is a ui-context-binding.")

(defun ui--consume-context (context)
  "Get the current value of CONTEXT.

Searches up the context stack for a matching provider.
Returns default-value if no provider found."
  (or (cl-loop for binding in ui--context-stack
               when (eq (ui-context-binding-context binding) context)
               return (ui-context-binding-value binding))
      (ui-context-default-value context)))

(defun ui--with-context-provider (context value body-fn)
  "Execute BODY-FN with CONTEXT bound to VALUE."
  (let ((ui--context-stack
         (cons (ui-context-binding--create
                :context context
                :value value
                :provider-instance ui--current-instance)
               ui--context-stack)))
    (funcall body-fn)))
```

## Rendering Providers

``` elisp
(defun ui--render-provider (vnode)
  "Render a context provider vnode."
  (let ((context (ui-vnode-provider-context vnode))
        (value (ui-vnode-provider-value vnode))
        (children (ui-vnode-provider-children vnode)))

    (ui--with-context-provider context value
      (lambda ()
        (dolist (child children)
          (ui--render-vnode child))))))
```

## Context Change Detection

When a context value changes, all consumers need to re-render:

``` elisp
(defun ui--context-consumers (context root-instance)
  "Find all instances that consume CONTEXT under ROOT-INSTANCE."
  (let ((consumers nil))
    (ui--walk-instance-tree
     root-instance
     (lambda (instance)
       (when (ui--instance-consumes-context-p instance context)
         (push instance consumers))))
    consumers))

(defun ui--instance-consumes-context-p (instance context)
  "Check if INSTANCE's render function uses CONTEXT."
  ;; We track this during render
  (memq context (ui-instance-consumed-contexts instance)))

(defun ui--mark-context-consumers-dirty (context provider-instance)
  "Mark all consumers of CONTEXT as dirty."
  (dolist (consumer (ui--context-consumers context provider-instance))
    (ui--mark-dirty consumer)))
```

## Optimising Context Updates

To avoid re-rendering the entire subtree when context changes:

``` elisp
(defun ui--reconcile-provider (old-instance new-vnode)
  "Reconcile a context provider."
  (let* ((context (ui-vnode-provider-context new-vnode))
         (old-value (ui--provider-value old-instance))
         (new-value (ui-vnode-provider-value new-vnode)))

    ;; Update provider value
    (setf (ui--provider-value old-instance) new-value)

    ;; If value changed, mark consumers dirty
    (unless (equal old-value new-value)
      (ui--mark-context-consumers-dirty context old-instance))

    ;; Continue reconciling children normally
    (ui--reconcile-children
     (ui-instance-children old-instance)
     (ui-vnode-provider-children new-vnode)
     old-instance)))
```

## Multiple Contexts

Components can consume multiple contexts:

``` elisp
(defcontext theme 'light)
(defcontext locale "en")
(defcontext user nil)

(defcomponent user-greeting
  :render
  (let ((theme (use-theme))
        (locale (use-locale))
        (user (use-user)))
    (ui-text
     (format (get-greeting locale) (plist-get user :name))
     :face (if (eq theme 'dark) 'bold 'default))))
```

## Context Composition Pattern

For many contexts, a composition helper:

``` elisp
(defun ui-providers (providers &rest children)
  "Nest multiple context providers.

PROVIDERS is a list of (context . value) pairs.

Example:
  (ui-providers
    `((,theme-context . dark)
      (,locale-context . \"en\")
      (,user-context . ,current-user))
    (app-content))"

  (if (null providers)
      (ui-fragment children)
    (let ((provider (car providers))
          (rest (cdr providers)))
      (ui-vnode-provider--create
       :context (car provider)
       :value (cdr provider)
       :children (list (apply #'ui-providers rest children))))))
```

## Example: Theme Context

``` elisp
;; Define theme context
(defcontext theme 'light "Current color theme.")

;; Theme-aware components
(defcomponent themed-button
  :props ((label :required t)
          (on-click :required t))

  :render
  (let ((theme (use-theme)))
    (ui-button
     :label label
     :on-click on-click
     :face (pcase theme
             ('light 'custom-button-light)
             ('dark 'custom-button-dark)))))

(defcomponent themed-panel
  :props ((title :required t)
          (children))

  :render
  (let ((theme (use-theme)))
    (ui-vstack
      (ui-text title :face (if (eq theme 'dark)
                               'header-dark
                             'header-light))
      (ui-separator)
      (ui-fragment children))))

;; App with theme switching
(defcomponent app
  :state ((theme 'light))

  :render
  (theme-provider theme
    (ui-vstack
      ;; Theme toggle
      (ui-button
       :label (format "Theme: %s" theme)
       :on-click (lambda ()
                   (setf theme (if (eq theme 'light) 'dark 'light))))

      ;; All children can access theme
      (themed-panel :title "Welcome"
        (themed-button :label "Click me" :on-click #'do-something)))))
```

## Context vs Props Guidelines

Use **props** when: - Data is needed by immediate children - Explicit data flow is clearer - Component reusability is important

Use **context** when: - Data is needed by many components at different levels - Data changes infrequently (theme, locale, auth) - Prop drilling would be excessive (\>3 levels)

# Lifecycle Hooks

## Lifecycle Overview

Components go through distinct phases:

``` example
+-----------------------------------------------------------------+
|                     MOUNTING                                    |
|  1. Create instance                                             |
|  2. Initialize state (initial-state-fn)                         |
|  3. Render (render-fn)                                          |
|  4. Commit to buffer                                            |
|  5. Run effects                                                 |
|  6. Call on-mount                                               |
+-----------------------------------------------------------------+
                              |
                              v
+-----------------------------------------------------------------+
|                     UPDATING (repeated)                         |
|  1. Receive new props or state change                           |
|  2. Check should-update                                         |
|  3. Render (render-fn)                                          |
|  4. Reconcile and commit                                        |
|  5. Run effect cleanups for changed deps                        |
|  6. Run effects for changed deps                                |
|  7. Call on-update                                              |
+-----------------------------------------------------------------+
                              |
                              v
+-----------------------------------------------------------------+
|                     UNMOUNTING                                  |
|  1. Run all effect cleanups                                     |
|  2. Call on-unmount                                             |
|  3. Remove from buffer                                          |
|  4. Clean up instance                                           |
+-----------------------------------------------------------------+
```

## Hook Definitions in defcomponent

``` elisp
(defcomponent my-component
  :props ((id :required t))
  :state ((data nil))

  ;; Called once after first render and commit
  :on-mount
  (lambda ()
    (message "Component %s mounted" id)
    (fetch-initial-data id))

  ;; Called after every update (not first render)
  ;; Receives previous props and state
  :on-update
  (lambda (prev-props prev-state)
    (when (not (equal (plist-get prev-props :id) id))
      (message "ID changed from %s to %s"
               (plist-get prev-props :id) id)
      (fetch-initial-data id)))

  ;; Called before removal from tree
  :on-unmount
  (lambda ()
    (message "Component %s unmounting" id)
    (cancel-pending-requests))

  :render
  ...)
```

## Hook Execution

``` elisp
(defun ui--run-mount-hooks (instance)
  "Run mount hooks for INSTANCE and its descendants."
  (unless (ui-instance-mounted-p instance)
    (setf (ui-instance-mounted-p instance) t)

    ;; Run children first (bottom-up)
    (dolist (child (ui-instance-children instance))
      (ui--run-mount-hooks child))

    ;; Run this instance's hook
    (when-let ((hook (ui-component-def-on-mount
                      (ui-instance-def instance))))
      (let ((ui--current-instance instance))
        (condition-case err
            (funcall hook)
          (error
           (ui--handle-lifecycle-error instance 'on-mount err)))))))

(defun ui--run-update-hooks (instance prev-props prev-state)
  "Run update hook for INSTANCE."
  (when-let ((hook (ui-component-def-on-update
                    (ui-instance-def instance))))
    (let ((ui--current-instance instance))
      (condition-case err
          (funcall hook prev-props prev-state)
        (error
         (ui--handle-lifecycle-error instance 'on-update err))))))

(defun ui--run-unmount-hooks (instance)
  "Run unmount hooks for INSTANCE and its descendants."
  (when (ui-instance-mounted-p instance)
    ;; Run this instance's hook first (top-down)
    (when-let ((hook (ui-component-def-on-unmount
                      (ui-instance-def instance))))
      (let ((ui--current-instance instance))
        (condition-case err
            (funcall hook)
          (error
           (ui--handle-lifecycle-error instance 'on-unmount err)))))

    ;; Run effect cleanups
    (ui--run-all-effect-cleanups instance)

    ;; Run children
    (dolist (child (ui-instance-children instance))
      (ui--run-unmount-hooks child))

    (setf (ui-instance-mounted-p instance) nil)))
```

## Effect Lifecycle

Effects have their own lifecycle interleaved with component lifecycle:

``` elisp
(defun ui--run-effects-after-commit (instance)
  "Run pending effects for INSTANCE after commit."
  (dolist (effect (ui-instance-pending-effects instance))
    (let* ((effect-id (nth 0 effect))
           (deps (nth 1 effect))
           (effect-fn (nth 2 effect))
           (prev-cleanup (ui--get-effect-cleanup instance effect-id)))

      ;; Run previous cleanup
      (when prev-cleanup
        (condition-case err
            (funcall prev-cleanup)
          (error
           (ui--handle-effect-error instance effect-id 'cleanup err))))

      ;; Run effect
      (condition-case err
          (let ((cleanup (funcall effect-fn)))
            (ui--store-effect-cleanup instance effect-id cleanup))
        (error
         (ui--handle-effect-error instance effect-id 'effect err)))))

  ;; Clear pending effects
  (setf (ui-instance-pending-effects instance) nil))

(defun ui--run-all-effect-cleanups (instance)
  "Run all effect cleanups for INSTANCE (on unmount)."
  (maphash (lambda (effect-id cleanup)
             (when cleanup
               (condition-case err
                   (funcall cleanup)
                 (error
                  (ui--handle-effect-error instance effect-id 'cleanup err)))))
           (ui-instance-effect-cleanups instance)))
```

## Timing Guarantees

The library provides these timing guarantees:

1.  **on-mount** is called after the component is visible in the buffer
2.  **on-update** is called after changes are visible
3.  **on-unmount** is called before removal from buffer
4.  **Effect cleanups** run before their corresponding effects
5.  **Parent hooks** run after all children (mount) or before (unmount)

# Scheduler and Batching

## The Problem

Without batching, each state change would trigger a full re-render:

``` elisp
;; Bad: 3 separate re-renders
(setf count (1+ count))      ; Re-render 1
(setf name "Bob")            ; Re-render 2
(setf items (cons x items))  ; Re-render 3
```

With batching:

``` elisp
;; Good: 1 re-render
(ui-batch
  (setf count (1+ count))
  (setf name "Bob")
  (setf items (cons x items)))
;; Single re-render here
```

## Scheduler Structure

``` elisp
(cl-defstruct (ui-scheduler (:constructor ui-scheduler--create))
  "Manages the render queue and update batching."

  ;; Render queue
  dirty-instances          ; Hash set of instances needing re-render
  dirty-roots              ; List of root instances to process

  ;; Batching
  batch-depth              ; Current nesting depth of ui-batch
  pending-state-updates    ; List of (instance key value) to apply

  ;; Rendering state
  is-rendering-p           ; Are we in a render cycle?
  is-committing-p          ; Are we in commit phase?

  ;; Deferred rendering
  idle-timer               ; Timer for deferred rendering
  idle-delay               ; Delay before idle render (default 0.01)

  ;; Effects
  pending-effects          ; Effects to run after commit
  pending-layout-effects   ; Layout effects (run before paint)

  ;; Metrics
  render-count             ; Total renders (for debugging)
  last-render-time)        ; Duration of last render cycle

(defvar ui--scheduler nil
  "The global scheduler instance.")

(defun ui--get-scheduler ()
  "Get or create the global scheduler."
  (or ui--scheduler
      (setq ui--scheduler (ui-scheduler--create
                           :dirty-instances (make-hash-table :test 'eq)
                           :batch-depth 0
                           :idle-delay 0.01))))
```

## Marking Dirty

``` elisp
(defun ui--mark-dirty (instance)
  "Mark INSTANCE as needing re-render."
  (let ((scheduler (ui--get-scheduler)))
    ;; Add to dirty set
    (puthash instance t (ui-scheduler-dirty-instances scheduler))

    ;; Find and track root
    (let ((root (ui--root-instance instance)))
      (unless (memq root (ui-scheduler-dirty-roots scheduler))
        (push root (ui-scheduler-dirty-roots scheduler))))

    ;; Schedule render (unless already rendering or batching)
    (unless (or (ui-scheduler-is-rendering-p scheduler)
                (> (ui-scheduler-batch-depth scheduler) 0))
      (ui--schedule-render))))

(defun ui--root-instance (instance)
  "Find the root instance for INSTANCE."
  (let ((current instance))
    (while (ui-instance-parent current)
      (setq current (ui-instance-parent current)))
    current))
```

## Scheduling Renders

``` elisp
(defun ui--schedule-render ()
  "Schedule a render cycle, batching rapid changes."
  (let ((scheduler (ui--get-scheduler)))
    (unless (ui-scheduler-idle-timer scheduler)
      (setf (ui-scheduler-idle-timer scheduler)
            (run-with-idle-timer
             (ui-scheduler-idle-delay scheduler)
             nil
             #'ui--process-scheduled-render)))))

(defun ui--process-scheduled-render ()
  "Process all scheduled renders."
  (let ((scheduler (ui--get-scheduler)))
    ;; Clear timer
    (setf (ui-scheduler-idle-timer scheduler) nil)

    ;; Process if not already rendering
    (unless (ui-scheduler-is-rendering-p scheduler)
      (ui--render-cycle))))

(defun ui--render-cycle ()
  "Execute a complete render cycle."
  (let* ((scheduler (ui--get-scheduler))
         (start-time (current-time)))

    (setf (ui-scheduler-is-rendering-p scheduler) t)

    (unwind-protect
        (progn
          ;; Phase 1: Apply pending state updates
          (ui--apply-pending-state-updates scheduler)

          ;; Phase 2: Render dirty trees
          (let ((roots (ui-scheduler-dirty-roots scheduler)))
            (setf (ui-scheduler-dirty-roots scheduler) nil)
            (clrhash (ui-scheduler-dirty-instances scheduler))

            (dolist (root roots)
              (ui--render-root root)))

          ;; Phase 3: Run effects
          (ui--run-pending-effects))

      ;; Cleanup
      (setf (ui-scheduler-is-rendering-p scheduler) nil)
      (setf (ui-scheduler-last-render-time scheduler)
            (float-time (time-subtract (current-time) start-time)))
      (cl-incf (ui-scheduler-render-count scheduler)))))

(defun ui--render-root (root-instance)
  "Render a single root instance tree."
  (let ((context (ui--capture-cursor-context root-instance)))

    ;; Render phase: build new virtual tree
    (let* ((def (ui-instance-def root-instance))
           (props (ui-instance-props root-instance))
           (state (ui-instance-state root-instance))
           (new-vtree (let ((ui--current-instance root-instance))
                        (funcall (ui-component-def-render-fn def)
                                 props state))))

      ;; Diff phase: compare with old tree
      (let ((ops (ui--reconcile-children
                  (ui-instance-children root-instance)
                  (ui--vtree-children new-vtree)
                  root-instance)))

        ;; Commit phase: apply to buffer
        (ui--commit-operations ops root-instance)

        ;; Update cache
        (setf (ui-instance-last-vtree root-instance) new-vtree)))

    ;; Restore cursor
    (ui--restore-cursor-context context root-instance)))
```

## Batching API

``` elisp
(defmacro ui-batch (&rest body)
  "Batch all state updates in BODY into a single render.

Use this when making multiple state changes that should
result in a single re-render.

Example:
  (ui-batch
    (setf count (1+ count))
    (setf items (cons new-item items)))"

  `(let* ((scheduler (ui--get-scheduler))
          (was-batching (> (ui-scheduler-batch-depth scheduler) 0)))
     (cl-incf (ui-scheduler-batch-depth scheduler))
     (unwind-protect
         (progn ,@body)
       (cl-decf (ui-scheduler-batch-depth scheduler))
       (when (and (not was-batching)
                  (= (ui-scheduler-batch-depth scheduler) 0))
         (ui--flush-batch scheduler)))))

(defun ui--flush-batch (scheduler)
  "Apply all batched updates and trigger render."
  ;; Apply pending state updates
  (ui--apply-pending-state-updates scheduler)

  ;; Schedule render if there are dirty instances
  (when (ui-scheduler-dirty-roots scheduler)
    (ui--schedule-render)))
```

## Immediate vs Deferred Rendering

Sometimes you need immediate rendering (e.g., for testing):

``` elisp
(defun ui-flush-sync ()
  "Synchronously flush all pending renders.

Use sparingly  -  mainly for testing or when you need
immediate visual feedback."
  (let ((scheduler (ui--get-scheduler)))
    ;; Cancel any pending idle timer
    (when (ui-scheduler-idle-timer scheduler)
      (cancel-timer (ui-scheduler-idle-timer scheduler))
      (setf (ui-scheduler-idle-timer scheduler) nil))

    ;; Process immediately
    (while (ui-scheduler-dirty-roots scheduler)
      (ui--render-cycle))))

(defmacro ui-sync (&rest body)
  "Execute BODY and flush renders synchronously."
  `(progn
     ,@body
     (ui-flush-sync)))
```

## Priority Scheduling (Advanced)

For complex UIs, different updates can have different priorities:

``` elisp
(defvar ui--priority-queues
  (list :immediate nil    ; User interactions
        :normal nil       ; State updates
        :deferred nil)    ; Background updates
  "Priority queues for scheduled work.")

(defun ui--schedule-with-priority (priority work)
  "Schedule WORK with given PRIORITY."
  (let ((queue (plist-get ui--priority-queues priority)))
    (plist-put ui--priority-queues priority (cons work queue)))
  (ui--schedule-render))

(defun ui--process-priority-queues ()
  "Process work in priority order."
  ;; Immediate: process all
  (dolist (work (nreverse (plist-get ui--priority-queues :immediate)))
    (funcall work))
  (plist-put ui--priority-queues :immediate nil)

  ;; Normal: process all
  (dolist (work (nreverse (plist-get ui--priority-queues :normal)))
    (funcall work))
  (plist-put ui--priority-queues :normal nil)

  ;; Deferred: process one, reschedule rest
  (when-let ((deferred (plist-get ui--priority-queues :deferred)))
    (funcall (car (last deferred)))
    (plist-put ui--priority-queues :deferred (butlast deferred))
    (when (plist-get ui--priority-queues :deferred)
      (ui--schedule-render))))
```

## Render Metrics

``` elisp
(defun ui-render-metrics ()
  "Return metrics about rendering performance."
  (let ((scheduler (ui--get-scheduler)))
    (list :total-renders (ui-scheduler-render-count scheduler)
          :last-render-ms (* 1000 (ui-scheduler-last-render-time scheduler))
          :pending-roots (length (ui-scheduler-dirty-roots scheduler))
          :pending-effects (length (ui-scheduler-pending-effects scheduler)))))

(defun ui-debug-render-cycle ()
  "Enable debug logging for render cycles."
  (advice-add 'ui--render-cycle :around
              (lambda (orig-fn)
                (message "UI: Starting render cycle...")
                (let ((result (funcall orig-fn)))
                  (message "UI: Render complete in %.2fms"
                           (* 1000 (ui-scheduler-last-render-time
                                    (ui--get-scheduler))))
                  result))))

---

* Buffer Renderer

** Overview

The buffer renderer is responsible for:
1. Translating vnodes into buffer content
2. Applying diff operations to the buffer
3. Managing overlays and text properties
4. Maintaining the instance ↔ buffer mapping

** Initial Rendering

When mounting a new tree:

#+begin_src elisp
(defun ui-render (vnode &optional buffer position)
  "Render VNODE into BUFFER at POSITION.

BUFFER defaults to current buffer.
POSITION defaults to point.

Returns the root instance."
  (let ((buffer (or buffer (current-buffer)))
        (position (or position (point))))

    (with-current-buffer buffer
      (save-excursion
        (goto-char position)

        (let* ((start-marker (point-marker))
               (instance (ui--instantiate-and-render vnode nil)))

          ;; Set up markers
          (set-marker-insertion-type start-marker t)
          (setf (ui-instance-region-start instance) start-marker)
          (setf (ui-instance-region-end instance) (point-marker))
          (setf (ui-instance-buffer instance) buffer)

          ;; Register as root
          (ui--register-root instance buffer)

          ;; Run mount hooks
          (ui--run-mount-hooks instance)

          instance)))))

(defun ui--instantiate-and-render (vnode parent-instance)
  "Create instance from VNODE and render to buffer."
  (cond
   ;; Component vnode
   ((ui-vnode-component-p vnode)
    (ui--instantiate-component vnode parent-instance))

   ;; Fragment
   ((ui-vnode-fragment-p vnode)
    (ui--render-fragment vnode parent-instance))

   ;; Primitives
   ((ui-vnode-text-p vnode)
    (ui--render-text vnode parent-instance))

   ((ui-vnode-button-p vnode)
    (ui--render-button vnode parent-instance))

   ((ui-vnode-field-p vnode)
    (ui--render-field vnode parent-instance))

   ((ui-vnode-newline-p vnode)
    (ui--render-newline vnode parent-instance))

   ((ui-vnode-space-p vnode)
    (ui--render-space vnode parent-instance))

   ;; Layout containers
   ((ui-vnode-hstack-p vnode)
    (ui--render-hstack vnode parent-instance))

   ((ui-vnode-vstack-p vnode)
    (ui--render-vstack vnode parent-instance))

   ((ui-vnode-provider-p vnode)
    (ui--render-provider vnode parent-instance))

   (t (error "Unknown vnode type: %S" vnode))))
```

## Rendering Primitives

``` elisp
(defun ui--render-text (vnode parent-instance)
  "Render a text vnode."
  (let ((start (point))
        (content (ui-vnode-text-content vnode))
        (face (ui-vnode-text-face vnode))
        (props (ui-vnode-text-properties vnode)))

    ;; Insert text
    (insert content)

    ;; Apply face
    (when face
      (put-text-property start (point) 'face face))

    ;; Apply additional properties
    (when props
      (add-text-properties start (point) props))

    ;; Create instance
    (ui--create-primitive-instance
     'text vnode parent-instance start (point))))

(defun ui--render-button (vnode parent-instance)
  "Render a button vnode."
  (let ((start (point))
        (label (ui-vnode-button-label vnode))
        (face (or (ui-vnode-button-face vnode) 'button))
        (on-click (ui-vnode-button-on-click vnode))
        (disabled (ui-vnode-button-disabled-p vnode)))

    ;; Insert button text
    (insert (if (stringp label) label "[button]"))

    ;; Create overlay for button behavior
    (let ((overlay (make-overlay start (point))))
      (overlay-put overlay 'face (if disabled 'shadow face))
      (overlay-put overlay 'mouse-face (unless disabled 'highlight))
      (overlay-put overlay 'keymap (ui--button-keymap on-click disabled))
      (overlay-put overlay 'ui-button t)
      (overlay-put overlay 'ui-on-click on-click)
      (overlay-put overlay 'help-echo (unless disabled "Click to activate"))

      ;; Create instance with overlay
      (let ((instance (ui--create-primitive-instance
                       'button vnode parent-instance start (point))))
        (push overlay (ui-instance-overlays instance))
        instance))))

(defun ui--button-keymap (on-click disabled)
  "Create keymap for button."
  (let ((map (make-sparse-keymap)))
    (unless disabled
      (define-key map [mouse-1]
        (lambda () (interactive) (funcall on-click)))
      (define-key map (kbd "RET")
        (lambda () (interactive) (funcall on-click)))
      (define-key map (kbd "SPC")
        (lambda () (interactive) (funcall on-click))))
    map))

(defun ui--render-field (vnode parent-instance)
  "Render an editable field vnode."
  (let* ((start (point))
         (value (ui-vnode-field-value vnode))
         (size (ui-vnode-field-size vnode))
         (face (or (ui-vnode-field-face vnode) 'widget-field))
         (on-change (ui-vnode-field-on-change vnode))
         (keymap (ui-vnode-field-keymap vnode)))

    ;; Insert value with padding if fixed size
    (if size
        (insert (truncate-string-to-width
                 (concat value (make-string size ?\s)) size))
      (insert value))

    ;; Create overlay
    (let ((overlay (make-overlay start (point) nil t nil)))
      (overlay-put overlay 'face face)
      (overlay-put overlay 'field t)
      (overlay-put overlay 'ui-field t)
      (overlay-put overlay 'modification-hooks
                   (list (ui--make-field-modification-hook on-change)))
      (overlay-put overlay 'insert-in-front-hooks
                   (list #'ui--field-insert-hook))
      (overlay-put overlay 'insert-behind-hooks
                   (list #'ui--field-insert-hook))

      ;; Apply custom keymap
      (when keymap
        (overlay-put overlay 'keymap keymap))

      (let ((instance (ui--create-primitive-instance
                       'field vnode parent-instance start (point))))
        (push overlay (ui-instance-overlays instance))
        instance))))

(defun ui--make-field-modification-hook (on-change)
  "Create modification hook that calls ON-CHANGE."
  (lambda (overlay after-p beg end &optional length)
    (when (and after-p on-change)
      (let ((new-value (buffer-substring-no-properties
                        (overlay-start overlay)
                        (overlay-end overlay))))
        (funcall on-change new-value)))))
```

## Applying Diff Operations

``` elisp
(defun ui--commit-operations (operations root-instance)
  "Apply OPERATIONS to buffer."
  (let ((inhibit-read-only t)
        (inhibit-modification-hooks t)
        (buffer (ui-instance-buffer root-instance)))

    (with-current-buffer buffer
      ;; Sort operations for safe application
      (let ((sorted-ops (ui--sort-operations operations)))
        (dolist (op sorted-ops)
          (ui--apply-operation op))))))

(defun ui--apply-operation (op)
  "Apply a single diff operation."
  (cond
   ((ui-op-delete-p op)
    (ui--apply-delete op))

   ((ui-op-insert-p op)
    (ui--apply-insert op))

   ((ui-op-replace-p op)
    (ui--apply-replace op))

   ((ui-op-update-props-p op)
    (ui--apply-update-props op))

   ((ui-op-move-p op)
    (ui--apply-move op))))

(defun ui--apply-delete (op)
  "Apply a delete operation."
  (let ((instance (ui-op-delete-instance op)))
    ;; Run unmount hooks first
    (ui--run-unmount-hooks instance)

    ;; Remove overlays
    (dolist (overlay (ui-instance-overlays instance))
      (delete-overlay overlay))

    ;; Delete text
    (delete-region (ui-op-delete-start op) (ui-op-delete-end op))

    ;; Clean up markers
    (set-marker (ui-instance-region-start instance) nil)
    (set-marker (ui-instance-region-end instance) nil)))

(defun ui--apply-insert (op)
  "Apply an insert operation."
  (goto-char (ui-op-insert-position op))
  (let ((instance (ui--instantiate-and-render
                   (ui-op-insert-vnode op)
                   (ui-op-insert-parent-instance op))))

    ;; Add to parent's children
    (when-let ((parent (ui-op-insert-parent-instance op)))
      (push instance (ui-instance-children parent)))

    ;; Run mount hooks
    (ui--run-mount-hooks instance)))

(defun ui--apply-replace (op)
  "Apply a replace operation."
  (let ((start (ui-op-replace-start op))
        (end (ui-op-replace-end op))
        (instance (ui-op-replace-instance op))
        (new-vnode (ui-op-replace-new-vnode op)))

    ;; Preserve cursor context
    (let ((at-start (= (point) start))
          (at-end (= (point) end))
          (offset (- (point) start)))

      ;; Delete old content
      (dolist (overlay (ui-instance-overlays instance))
        (delete-overlay overlay))
      (setf (ui-instance-overlays instance) nil)
      (delete-region start end)

      ;; Insert new content
      (goto-char start)
      (ui--render-vnode-into-instance new-vnode instance)

      ;; Restore cursor approximately
      (cond
       (at-start (goto-char (marker-position
                             (ui-instance-region-start instance))))
       (at-end (goto-char (marker-position
                           (ui-instance-region-end instance))))
       (t (goto-char (min (+ start offset)
                          (marker-position
                           (ui-instance-region-end instance)))))))))

(defun ui--apply-update-props (op)
  "Apply a property update operation."
  (let ((start (ui-op-update-props-start op))
        (end (ui-op-update-props-end op))
        (new-props (ui-op-update-props-new-props op)))

    ;; Update text properties
    (when-let ((face (plist-get new-props :face)))
      (put-text-property start end 'face face))

    ;; Update overlay properties if instance has overlays
    (when-let* ((instance (ui-op-update-props-instance op))
                (overlays (ui-instance-overlays instance)))
      (dolist (overlay overlays)
        (when-let ((face (plist-get new-props :face)))
          (overlay-put overlay 'face face))))))
```

## Instance ↔ Buffer Mapping

``` elisp
(defun ui--instance-at-point (&optional pos)
  "Find the leaf instance at POS (default: point)."
  (let ((pos (or pos (point))))
    (catch 'found
      (dolist (root (ui--buffer-roots (current-buffer)))
        (when-let ((instance (ui--find-instance-at-pos root pos)))
          (throw 'found instance))))))

(defun ui--find-instance-at-pos (instance pos)
  "Find deepest instance containing POS."
  (let ((start (marker-position (ui-instance-region-start instance)))
        (end (marker-position (ui-instance-region-end instance))))
    (when (and (>= pos start) (<= pos end))
      ;; Check children first
      (or (cl-loop for child in (ui-instance-children instance)
                   thereis (ui--find-instance-at-pos child pos))
          instance))))

(defvar-local ui--buffer-roots nil
  "List of root instances rendered in this buffer.")

(defun ui--register-root (instance buffer)
  "Register INSTANCE as a root in BUFFER."
  (with-current-buffer buffer
    (push instance ui--buffer-roots)))

(defun ui--unregister-root (instance)
  "Unregister INSTANCE as a root."
  (when-let ((buffer (ui-instance-buffer instance)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (setq ui--buffer-roots (delq instance ui--buffer-roots))))))

(defun ui--buffer-roots (buffer)
  "Get all root instances in BUFFER."
  (buffer-local-value 'ui--buffer-roots buffer))
```

## Overlay Management

``` elisp
(defun ui--clean-instance-overlays (instance)
  "Remove all overlays belonging to INSTANCE."
  (dolist (overlay (ui-instance-overlays instance))
    (when (overlay-buffer overlay)
      (delete-overlay overlay)))
  (setf (ui-instance-overlays instance) nil))

(defun ui--update-overlay-region (instance start end)
  "Update overlay regions for INSTANCE."
  (dolist (overlay (ui-instance-overlays instance))
    (move-overlay overlay start end)))

(defun ui--overlay-at-point ()
  "Get the UI overlay at point, if any."
  (cl-find-if (lambda (ov)
                (or (overlay-get ov 'ui-button)
                    (overlay-get ov 'ui-field)))
              (overlays-at (point))))
```

## Buffer Teardown

``` elisp
(defun ui-unmount (instance)
  "Unmount INSTANCE and remove from buffer."
  (when instance
    ;; Run unmount hooks
    (ui--run-unmount-hooks instance)

    ;; Remove from buffer
    (let ((inhibit-read-only t)
          (buffer (ui-instance-buffer instance)))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (delete-region
           (marker-position (ui-instance-region-start instance))
           (marker-position (ui-instance-region-end instance))))))

    ;; Clean up
    (ui--unregister-root instance)
    (ui--clean-instance-overlays instance)
    (set-marker (ui-instance-region-start instance) nil)
    (set-marker (ui-instance-region-end instance) nil)))

(defun ui-unmount-buffer (&optional buffer)
  "Unmount all UI instances in BUFFER."
  (let ((buffer (or buffer (current-buffer))))
    (dolist (root (ui--buffer-roots buffer))
      (ui-unmount root))))

---

* Error Handling

** Error Categories

Errors can occur at different phases:

1. **Render errors**: Exception in render function
2. **Lifecycle errors**: Exception in on-mount/on-update/on-unmount
3. **Effect errors**: Exception in effect or cleanup function
4. **Event handler errors**: Exception in on-click/on-change callbacks

** Error Boundary Pattern

Like React, we support error boundaries  -  components that catch errors in their subtree:

#+begin_src elisp
(cl-defstruct (ui-vnode-error-boundary (:include ui-vnode)
                                        (:constructor ui-vnode-error-boundary--create))
  "Error boundary container."
  child                   ; Normal child vnode
  fallback-fn             ; (lambda (error) vnode)  -  what to render on error
  on-error)               ; (lambda (error info) void)  -  error reporting

(defun ui-error-boundary (&rest args)
  "Create an error boundary.

Usage:
  (ui-error-boundary
    :fallback (lambda (err) (ui-text (format \"Error: %s\" err)))
    :on-error (lambda (err info) (log-error err info))
    (potentially-failing-component))"

  (let ((fallback-fn nil)
        (on-error nil)
        (child nil))

    (while (keywordp (car args))
      (pcase (pop args)
        (:fallback (setq fallback-fn (pop args)))
        (:on-error (setq on-error (pop args)))
        (:key nil)))

    (setq child (car args))

    (ui-vnode-error-boundary--create
     :child child
     :fallback-fn (or fallback-fn
                      (lambda (err)
                        (ui-text (format "Error: %S" err)
                                 :face 'error)))
     :on-error on-error)))
```

## Error Boundary Implementation

``` elisp
(defun ui--render-error-boundary (vnode parent-instance)
  "Render an error boundary."
  (let* ((instance (ui--create-error-boundary-instance vnode parent-instance))
         (child-vnode (ui-vnode-error-boundary-child vnode))
         (fallback-fn (ui-vnode-error-boundary-fallback-fn vnode))
         (on-error (ui-vnode-error-boundary-on-error vnode)))

    (condition-case err
        ;; Try to render child normally
        (let ((child-instance (ui--instantiate-and-render
                               child-vnode instance)))
          (setf (ui-instance-children instance) (list child-instance))
          (setf (ui-instance-error instance) nil))

      ;; On error, render fallback
      (error
       (setf (ui-instance-error instance) err)

       ;; Report error
       (when on-error
         (funcall on-error err
                  (list :component (ui-vnode-error-boundary-child vnode)
                        :phase 'render)))

       ;; Render fallback
       (let ((fallback-vnode (funcall fallback-fn err)))
         (let ((fallback-instance (ui--instantiate-and-render
                                   fallback-vnode instance)))
           (setf (ui-instance-children instance) (list fallback-instance))))))

    instance))

(defun ui--recover-from-error (boundary-instance)
  "Attempt to recover from error in BOUNDARY-INSTANCE."
  (when (ui-instance-error boundary-instance)
    ;; Clear error and trigger re-render
    (setf (ui-instance-error boundary-instance) nil)
    (ui--mark-dirty boundary-instance)))
```

## Catching Lifecycle Errors

``` elisp
(defun ui--handle-lifecycle-error (instance phase error)
  "Handle error in lifecycle hook."
  (let ((boundary (ui--find-error-boundary instance)))
    (if boundary
        ;; Propagate to boundary
        (progn
          (setf (ui-instance-error boundary) error)
          (when-let ((on-error (ui-vnode-error-boundary-on-error
                                (ui-instance-last-vtree boundary))))
            (funcall on-error error (list :instance instance :phase phase)))
          (ui--mark-dirty boundary))

      ;; No boundary  -  log and continue
      (message "UI lifecycle error in %s (%s): %S"
               (ui-component-def-name (ui-instance-def instance))
               phase
               error))))

(defun ui--find-error-boundary (instance)
  "Find nearest error boundary ancestor of INSTANCE."
  (let ((current (ui-instance-parent instance)))
    (while current
      (when (ui--error-boundary-p current)
        (cl-return current))
      (setq current (ui-instance-parent current)))))

(defun ui--error-boundary-p (instance)
  "Check if INSTANCE is an error boundary."
  (and (ui-instance-def instance)
       (eq (ui-component-def-name (ui-instance-def instance))
           'ui--error-boundary)))
```

## Catching Effect Errors

``` elisp
(defun ui--handle-effect-error (instance effect-id phase error)
  "Handle error in effect."
  (let ((boundary (ui--find-error-boundary instance)))
    (if boundary
        (progn
          (setf (ui-instance-error boundary) error)
          (when-let ((on-error (ui-vnode-error-boundary-on-error
                                (ui-instance-last-vtree boundary))))
            (funcall on-error error
                     (list :instance instance
                           :effect-id effect-id
                           :phase phase)))
          (ui--mark-dirty boundary))

      (message "UI effect error in %s (effect %s, %s): %S"
               (ui-component-def-name (ui-instance-def instance))
               effect-id phase error))))
```

## Event Handler Error Handling

Event handlers run outside the render cycle, so we wrap them:

``` elisp
(defun ui--wrap-event-handler (handler instance)
  "Wrap HANDLER to catch and report errors."
  (lambda (&rest args)
    (condition-case err
        (apply handler args)
      (error
       (ui--handle-event-error instance err)))))

(defun ui--handle-event-error (instance error)
  "Handle error from event handler."
  ;; For event errors, we just log  -  don't break the UI
  (message "UI event handler error in %s: %S"
           (if instance
               (ui-component-def-name (ui-instance-def instance))
             "unknown")
           error)

  ;; Optionally call global error handler
  (when ui-global-error-handler
    (funcall ui-global-error-handler error instance 'event)))

(defvar ui-global-error-handler nil
  "Function called for all UI errors.
Signature: (lambda (error instance phase) ...)")
```

## Development Mode Helpers

``` elisp
(defvar ui-debug-mode nil
  "When non-nil, enable verbose error reporting.")

(defun ui--debug-error (error context)
  "Log detailed error information when debugging."
  (when ui-debug-mode
    (message "UI Error Details:")
    (message "  Error: %S" error)
    (message "  Context: %S" context)
    (message "  Backtrace:")
    (let ((frames (backtrace-frames)))
      (cl-loop for frame in (seq-take frames 10)
               do (message "    %S" frame)))))

(defmacro ui-with-debug (&rest body)
  "Execute BODY with debug mode enabled."
  `(let ((ui-debug-mode t))
     ,@body))
```

## Recovery Strategies

``` elisp
(defun ui-reset-component (instance)
  "Reset INSTANCE to initial state, clearing errors."
  (when instance
    ;; Clear error state
    (setf (ui-instance-error instance) nil)

    ;; Reset to initial state
    (let* ((def (ui-instance-def instance))
           (initial-state-fn (ui-component-def-initial-state-fn def))
           (props (ui-instance-props instance)))
      (setf (ui-instance-state instance)
            (if initial-state-fn
                (funcall initial-state-fn props)
              nil)))

    ;; Trigger re-render
    (ui--mark-dirty instance)))

(defun ui-reset-tree (root-instance)
  "Reset entire tree under ROOT-INSTANCE."
  (ui--walk-instance-tree
   root-instance
   #'ui-reset-component))

---

* API Reference

** Component Definition

#+begin_src elisp
(defcomponent NAME DOCSTRING
  :props PROPS-SPEC
  :state STATE-SPEC
  :on-mount MOUNT-FN
  :on-update UPDATE-FN
  :on-unmount UNMOUNT-FN
  :should-update PREDICATE-FN
  :render RENDER-BODY)
```

**PROPS-SPEC**: List of `(NAME :type TYPE :default DEFAULT :required BOOL)`

**STATE-SPEC**: List of `(NAME INITIAL-VALUE)`

## Primitive VNodes

| Function | Description | Key Props |
|----|----|----|
| `(ui-text CONTENT &rest PROPS)` | Plain text | `:face`, `:properties` |
| `(ui-button &rest PROPS)` | Clickable button | `:label`, `:on-click`, `:disabled` |
| `(ui-field &rest PROPS)` | Editable text field | `:value`, `:on-change`, `:size`, `:keymap` |
| `(ui-newline)` | Line break |  |
| `(ui-space &optional WIDTH)` | Horizontal space |  |
| `(ui-fragment &rest CHILDREN)` | Group without wrapper |  |

## Layout VNodes

| Function | Description | Key Props |
|----|----|----|
| `(ui-hstack &rest ARGS)` | Horizontal layout | `:spacing`, `:align` |
| `(ui-vstack &rest ARGS)` | Vertical layout | `:spacing`, `:indent` |
| `(ui-box &rest ARGS)` | Fixed-width container | `:width`, `:align`, `:padding` |
| `(ui-table &rest ARGS)` | Table layout | `:columns`, `:rows`, `:border` |
| `(ui-list &rest ARGS)` | Bulleted/numbered list | `:marker`, `:indent` |

## State Hooks

``` elisp
;; Declare state with setf support
(let-state ((NAME INITIAL) ...)
  BODY)

;; Memoised derived values
(let-memo ((NAME DEPS COMPUTE-EXPR) ...)
  BODY)

;; Side effects
(use-effect DEPS
  EFFECT-BODY
  [CLEANUP-FN])

;; Mutable ref (no re-render)
(use-ref INITIAL-VALUE)

;; Memoised callback
(use-callback DEPS
  CALLBACK-BODY)

;; Reducer pattern
(use-reducer REDUCER-FN INITIAL-STATE)
```

## Context

``` elisp
;; Define context
(defcontext NAME DEFAULT-VALUE DOCSTRING)

;; Provide context
(NAME-provider VALUE &rest CHILDREN)

;; Consume context
(use-NAME)  ; returns current value
```

## Rendering

``` elisp
;; Mount tree into buffer
(ui-render VNODE &optional BUFFER POSITION) → INSTANCE

;; Unmount
(ui-unmount INSTANCE)
(ui-unmount-buffer &optional BUFFER)

;; Force synchronous flush
(ui-flush-sync)

;; Batch updates
(ui-batch &rest BODY)
```

# Error Handling

``` elisp
;; Error boundary
(ui-error-boundary
  :fallback (lambda (err) FALLBACK-VNODE)
  :on-error (lambda (err info) ...)
  CHILD)

;; Recovery
(ui-reset-component INSTANCE)
(ui-reset-tree ROOT-INSTANCE)
```

## Utilities

``` elisp
;; Find instance at point
(ui--instance-at-point &optional POS)

;; Render metrics
(ui-render-metrics) → plist

;; Debug mode
(setq ui-debug-mode t)
```

# Implementation Roadmap

## Phase 1: Core Foundation (MVP)

**Goal**: Render static component trees with basic interactivity

**1.1 Data Structures** - \[ \] Define all cl-defstructs (vnode types, instance, context, scheduler) - \[ \] Component registry (hash-table mapping name → definition) - \[ \] Basic `defcomponent` macro (props only, no state yet)

**1.2 Initial Rendering** - \[ \] Render primitives: text, button, newline, space - \[ \] Fragment support - \[ \] Marker-based region tracking (start/end per instance) - \[ \] Overlay creation for buttons - \[ \] Read-only text protection

**1.3 Basic Interactivity** - \[ \] Button click handling via overlays - \[ \] Field editing with on-change callback - \[ \] Custom keymap support on fields - \[ \] TAB/S-TAB navigation between interactive elements

**1.4 Simple State** - \[ \] `let-state` macro - \[ \] State mutation triggering re-render - \[ \] Full tree re-render (no diffing yet — rebuild everything)

**Deliverable**: Can render a static form with buttons and fields that respond to user input.

## Phase 2: Reconciliation

**Goal**: Efficient updates via diffing

**2.1 Virtual Tree Diffing** - \[ \] Implement `ui--reconcile` main function - \[ \] Type comparison logic - \[ \] Key-based child matching - \[ \] Operation generation (insert, delete, replace, update-props)

**2.2 Commit Phase** - \[ \] Apply delete operations (from end to start) - \[ \] Apply insert operations - \[ \] Apply replace operations - \[ \] Apply property update operations - \[ \] Operation sorting for safe application

**2.3 Cursor Preservation** - \[ \] Cursor context capture before mutations - \[ \] Key-path based restoration - \[ \] Structural path fallback - \[ \] Destroyed component handling (go to where it was) - \[ \] Field edit protection (don't update focused field)

**Deliverable**: Can add/remove items from a list without full re-render; cursor stays in place.

## Phase 3: Advanced State

**Goal**: Full state management capabilities

**3.1 Effects System** - \[ \] `use-effect` implementation - \[ \] Dependency array tracking - \[ \] Cleanup function support - \[ \] Effect execution after commit

**3.2 Additional Hooks** - \[ \] `use-ref` (mutable reference without re-render) - \[ \] `use-callback` (memoised callbacks) - \[ \] `let-memo` (memoised computed values) - \[ \] `use-reducer` (reducer pattern for complex state)

**3.3 Context System** - \[ \] `defcontext` macro - \[ \] Provider vnode and rendering - \[ \] Context consumption via `use-NAME` - \[ \] Context change propagation to consumers

**Deliverable**: Can build components with side effects, shared state, and deep data passing.

## Phase 4: Layout & Polish

**Goal**: Production-ready features

**4.1 Layout Primitives** - \[ \] `ui-hstack` with spacing - \[ \] `ui-vstack` with spacing and indent - \[ \] `ui-indent` with render context propagation - \[ \] `ui-box` with fixed width and alignment - \[ \] Basic `ui-table` support

**4.2 Lifecycle Hooks** - \[ \] `on-mount` (after first render) - \[ \] `on-update` (after re-render, with prev props/state) - \[ \] `on-unmount` (before removal) - \[ \] Proper hook ordering (children before parents for mount)

**4.3 Error Handling** - \[ \] `ui-error-boundary` component - \[ \] Lifecycle error catching and propagation - \[ \] Event handler error wrapping - \[ \] `ui-reset-component` for recovery

**4.4 Scheduler Improvements** - \[ \] `ui-batch` for grouping state updates - \[ \] Idle-time rendering via timer - \[ \] `ui-flush-sync` for immediate updates

**Deliverable**: Full-featured UI library with layouts, error handling, and optimised updates.

## Phase 5: Optimisation & Tooling

**Goal**: Performance and developer experience

**5.1 Performance** - \[ \] `should-update` optimisation (skip re-render if no changes) - \[ \] Memo comparison modes (eq, equal, custom) - \[ \] Render timing instrumentation

**5.2 Developer Tools** - \[ \] Component inspector (show tree structure) - \[ \] State viewer (show current state of instances) - \[ \] Render cycle debugging/logging

**5.3 Documentation & Examples** - \[ \] API documentation with docstrings - \[ \] Tutorial: building a todo app - \[ \] Example: file browser - \[ \] Example: form with validation

**Deliverable**: Production-ready library with tooling and documentation.

## Milestones

| Milestone | Phase | Deliverable |
|----|----|----|
| **M1: Hello World** | 1 | Static components with basic state and interaction |
| **M2: Todo App** | 2 | List with add/remove, efficient updates, cursor preservation |
| **M3: Feature Complete** | 4 | All core features working |
| **M4: Production Ready** | 5 | Optimised, documented, tested |

## Testing Strategy

**Unit Tests** - Each data structure constructor - Macro expansions - Individual reconciliation operations

**Integration Tests** - Multi-component render scenarios - State change → re-render → correct output - Cursor preservation across various mutations

**Visual/Buffer Tests** - Render component → verify buffer contents - Compare before/after for operations

**Property-Based Testing** (optional) - Random trees → reconcile → correct result - Random state changes → consistent state

## Dependencies

**Required (built-in)**: - `cl-lib` — structures, loop, utilities - `subr-x` — when-let, if-let, thread macros

**Optional**: - `ert` — for testing

**No external packages required.**

# Appendix A: Comparison with widget.el

| Aspect      | widget.el            | This Library             |
|-------------|----------------------|--------------------------|
| Model       | Imperative           | Declarative              |
| Updates     | Manual create/delete | Automatic reconciliation |
| State       | Mixed with widget    | Explicit, separated      |
| Composition | Format strings       | Tree structure           |
| Lifecycle   | Implicit             | Explicit hooks           |
| Cursor      | Per-widget handling  | Automatic preservation   |

## Migration Path

Existing widget.el code can coexist. To wrap a widget:

``` elisp
(defcomponent widget-wrapper
  :props ((widget-type :required t)
          (widget-props :default nil))

  :render
  (ui-vnode-widget--create
   :widget-type widget-type
   :widget-props widget-props))
```

# Appendix B: Example Applications

## B.1 Todo Application

``` elisp
(defcontext todos nil)

(defcomponent todo-item
  :props ((todo :required t)
          (on-toggle :required t)
          (on-delete :required t))

  :render
  (ui-hstack
    (ui-button
     :label (if (plist-get todo :done) "[x]" "[ ]")
     :on-click (lambda () (funcall on-toggle (plist-get todo :id))))
    (ui-text (plist-get todo :text)
             :face (if (plist-get todo :done) 'shadow nil))
    (ui-button :label "×"
               :on-click (lambda () (funcall on-delete (plist-get todo :id))))))

(defcomponent todo-list
  :state ((todos nil)
          (new-text ""))

  :render
  (ui-vstack
    ;; Input
    (ui-hstack
      (ui-field :value new-text
                :on-change (lambda (v) (setf new-text v)))
      (ui-button :label "Add"
                 :on-click (lambda ()
                             (push (list :id (cl-gensym)
                                        :text new-text
                                        :done nil)
                                   todos)
                             (setf new-text ""))))

    ;; List
    (ui-vstack
      (mapcar (lambda (todo)
                (todo-item
                 :key (plist-get todo :id)
                 :todo todo
                 :on-toggle (lambda (id)
                              (setf todos
                                    (mapcar (lambda (t)
                                              (if (eq (plist-get t :id) id)
                                                  (plist-put t :done
                                                             (not (plist-get t :done)))
                                                t))
                                            todos)))
                 :on-delete (lambda (id)
                              (setf todos
                                    (cl-remove-if
                                     (lambda (t) (eq (plist-get t :id) id))
                                     todos)))))
              todos))

    ;; Summary
    (ui-text (format "%d items, %d completed"
                     (length todos)
                     (cl-count-if (lambda (t) (plist-get t :done)) todos))
             :face 'shadow)))
```

## B.2 File Browser

``` elisp
(defcomponent file-item
  :props ((path :required t)
          (on-select :required t))

  :render
  (let ((name (file-name-nondirectory path))
        (is-dir (file-directory-p path)))
    (ui-hstack
      (ui-text (if is-dir "📁" "📄"))
      (ui-button :label name
                 :on-click (lambda () (funcall on-select path))))))

(defcomponent file-browser
  :props ((root :required t))
  :state ((current-dir nil)
          (selected nil))

  :on-mount
  (lambda () (setf current-dir root))

  :render
  (let ((entries (directory-files current-dir t "^[^.]")))
    (ui-vstack
      ;; Breadcrumb
      (ui-hstack
        (ui-button :label "↑"
                   :on-click (lambda ()
                               (setf current-dir
                                     (file-name-directory
                                      (directory-file-name current-dir)))))
        (ui-text current-dir :face 'bold))

      (ui-separator)

      ;; File list
      (ui-vstack
        (mapcar (lambda (path)
                  (file-item
                   :key path
                   :path path
                   :on-select (lambda (p)
                                (if (file-directory-p p)
                                    (setf current-dir p)
                                  (setf selected p)))))
                entries))

      ;; Selection
      (when selected
        (ui-fragment
          (ui-separator)
          (ui-text (format "Selected: %s" selected) :face 'success))))))
```

# Appendix C: Naming Conventions

| Prefix         | Meaning                   |
|----------------|---------------------------|
| `ui-`          | Public API                |
| `ui--`         | Internal/private          |
| `ui-vnode-`    | Virtual node types        |
| `ui-instance-` | Instance accessors        |
| `ui-op-`       | Diff operations           |
| `use-`         | Hooks (consume in render) |
| `let-`         | Binding forms             |
| `def`          | Definition forms          |

# Appendix D: Open Questions & Future Work

These are areas identified during design that need further exploration or are deferred to future versions.

## D.1 Multi-line Content in Horizontal Layouts

**Current decision**: Forbid newlines in `h-stack` children.

**Future option**: Support proper column alignment where multi-line content extends the "row height":

``` example
| comp1 | comp2.1 | comp3 |
|       | comp2.2 |       |
|       | comp2.3 |       |
```

This requires two-pass rendering (measure then render) and is significantly more complex.

## D.2 Table Multi-line Cells

Similarly, tables currently assume single-line cells. Multi-line cells would require: - Cell height calculation - Row height = max(cell heights) - Padding shorter cells

**Decision**: Out of scope for v1. Tables are single-line per row.

## D.3 Responsive Layout

**Current approach**: Layout is fixed. If content exceeds window width, it overflows.

**Potential enhancements**: - `h-stack :wrap t` — wrap overflowing children to next line - `ui-table :stretch t` — proportionally stretch/shrink columns - Breakpoint-based layout selection

``` elisp
(ui-responsive
  (>= (window-width) 80) (wide-layout)
  (>= (window-width) 40) (medium-layout)
  t (narrow-layout))
```

**Question**: When does responsive layout recalculate? On window resize? This would need hooks into `window-size-change-functions`.

## D.4 Undo Integration

React doesn't have undo. Neither do we, initially.

Future exploration: - State snapshots on each change - Integration with `undo-tree` - "Time travel" debugging

## D.5 Keyboard Navigation

**Current approach**: Rely on widget.el's overlay properties for TAB navigation.

**If that's insufficient**: Implement `ui-forward` / `ui-backward` that traverse instance tree for focusable components.

## D.6 Transient Integration

For complex command palettes, integration with `transient.el` could provide: - Rich keyboard-driven menus - Persisted prefixes - Better than basic completing-read for complex choices

``` elisp
(ui-transient :spec my-transient-spec
              :on-action handle-action)
```

## D.7 Animation

Currently no animation support. Potential future work: - Transition on show/hide - Smooth content changes - Uses idle timers + overlay manipulation

## D.8 Accessibility

Consider: - Screen reader compatibility - High contrast mode via face inheritance - Keyboard-only operation (already mostly there)

## D.9 Effects for Hidden Components

When a component is hidden via `ui-hidden`: - Should effects continue running? (Current: yes) - Should they be paused? - What about effects with visual side effects?

React's "offscreen" API has similar questions. For now, effects continue.

## D.10 Server Rendering / Serialisation

Not applicable to Emacs in the traditional sense, but: - Could we serialise a component tree? - Restore from serialisation? - Useful for session persistence

## D.11 Concurrent Rendering

Emacs is single-threaded, so no true concurrency. However: - Could we yield during large renders? - Interruptible reconciliation? - Priority-based scheduling (user interactions first)?

## D.12 Memory Management

Long-running UIs might accumulate: - Detached overlays - Orphaned instances - Callback closures

Need to ensure proper cleanup on unmount.

*End of Design Document*
