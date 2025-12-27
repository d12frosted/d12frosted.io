As your [vui.el](https://github.com/d12frosted/vui.el/) application grows, you'll face two challenges: deeply nested components that need access to shared data, and organising code to stay maintainable. This article tackles both.

``` related_posts
```

# The Prop Drilling Problem

Consider a theme switcher. The current theme needs to reach every component that renders styled content:

``` elisp
;;; -*- lexical-binding: t -*-

;; Create an inverted face to make theme changes visible
(copy-face 'default 'default-inverted)
(invert-face 'default-inverted)

(vui-defcomponent app ()
  :state ((theme 'light))
  :render
  (vui-component 'main-layout
    :theme theme
    :on-theme-change (lambda (new-theme) (vui-set-state :theme new-theme))))

(vui-defcomponent main-layout (theme on-theme-change)
  :render
  (vui-vstack
   (vui-component 'header :theme theme :on-theme-change on-theme-change)
   (vui-component 'content :theme theme)
   (vui-component 'footer :theme theme)))

(vui-defcomponent header (theme on-theme-change)
  :render
  (vui-hstack
   (vui-text "My App" :face (if (eq theme 'dark) 'default-inverted 'default))
   (vui-component 'theme-toggle :theme theme :on-change on-theme-change)))

(vui-defcomponent theme-toggle (theme on-change)
  :render
  (vui-button (if (eq theme 'dark) "☀️ Light" "🌙 Dark")
    :on-click (lambda ()
                (funcall on-change (if (eq theme 'dark) 'light 'dark)))))

;; Placeholder components
(vui-defcomponent content (theme)
  :render
  (vui-text "Main content area"
    :face (if (eq theme 'dark) 'default-inverted 'default)))

(vui-defcomponent footer (theme)
  :render
  (vui-text "© 2025"
    :face (if (eq theme 'dark) 'default-inverted 'default)))

;; Try it!
(vui-mount (vui-component 'app))
```

Look at the prop chain: `app` → `main-layout` → `header` → `theme-toggle`. Every intermediate component must accept and pass along `theme` and `on-theme-change`, even if it doesn't use them directly. `main-layout` doesn't care about themes - it's just a layout - but it has to know about theme props because its children need them.

This is "prop drilling." It works, but it's tedious, error-prone, and clutters components with props they don't actually use.

# Context: Implicit Data Flow

Context solves this by making data available to any descendant without explicit props:

``` elisp
;;; -*- lexical-binding: t -*-

;; Create inverted face (same as before)
(copy-face 'default 'default-inverted)
(invert-face 'default-inverted)

;; Define a context - generates:
;;   theme-context (the context object)
;;   theme-provider (macro to provide value)
;;   use-theme (function to consume value)
(vui-defcontext theme 'light)

;; Provide value at the top
(vui-defcomponent app ()
  :state ((theme 'light))
  :render
  (theme-provider
      (list :theme theme
            :toggle (lambda ()
                      (vui-set-state :theme (if (eq theme 'dark) 'light 'dark))))
    (vui-component 'main-layout)))

;; Consume anywhere below  -  no prop drilling!
(vui-defcomponent theme-toggle ()
  :render
  (let* ((ctx (use-theme))
         (theme (plist-get ctx :theme))
         (toggle (plist-get ctx :toggle)))
    (vui-button (if (eq theme 'dark) "☀️ Light" "🌙 Dark")
      :on-click toggle)))

;; main-layout doesn't need to know about theme at all
(vui-defcomponent main-layout ()
  :render
  (vui-vstack
   (vui-component 'header)
   (vui-component 'content)
   (vui-component 'footer)))

;; header uses context for styling and contains the toggle
(vui-defcomponent header ()
  :render
  (let* ((ctx (use-theme))
         (theme (plist-get ctx :theme)))
    (vui-hstack
     (vui-text "My App" :face (if (eq theme 'dark) 'default-inverted 'default))
     (vui-component 'theme-toggle))))

;; content and footer can use context too
(vui-defcomponent content ()
  :render
  (let* ((ctx (use-theme))
         (theme (plist-get ctx :theme)))
    (vui-text "Main content area"
      :face (if (eq theme 'dark) 'default-inverted 'default))))

(vui-defcomponent footer ()
  :render
  (let* ((ctx (use-theme))
         (theme (plist-get ctx :theme)))
    (vui-text "© 2025"
      :face (if (eq theme 'dark) 'default-inverted 'default))))

;; Try it!
(vui-mount (vui-component 'app))
```

Notice how `main-layout` is now clean - it just arranges its children without knowing anything about themes. Components that need the theme call `use-theme` directly. The data flows implicitly through the component tree.

# How Context Works

`defcontext` generates three things:

1.  **The context object** (`theme-context`) - stores the default value
2.  **A provider macro** (`theme-provider`) - wraps a subtree and provides a value
3.  **A consumer function** (`use-theme`) - retrieves the current value

When a component calls `use-theme`, vui.el walks up the component tree looking for the nearest `theme-provider`. If none is found, it uses the default value from `defcontext`.

You can provide any value - a symbol, a plist, a list of functions. In the theme example, we provide both the current theme and a toggle function, so consumers can both read and update the theme.

# When to Use Context

Context is powerful but can make data flow harder to trace. With props, you can follow the chain: "this value came from the parent, which got it from its parent…" With context, the source is implicit.

**Use context for:**

- **Theme/appearance settings** - affects many components across the tree
- **Localisation/i18n** - language strings needed everywhere
- **Current user/auth state** - accessed throughout the app
- **Feature flags** - conditional behaviour across components

**Avoid context for:**

- **Data specific to one subtree** - just pass props, it's clearer
- **Frequently changing data** - context changes re-render all consumers
- **Everything** - overusing context makes debugging harder

Rule of thumb: if you're passing the same prop through 3+ intermediate components that don't use it, consider context. Otherwise, explicit props are clearer.

# Multiple Contexts

You can have multiple contexts, each with a focused purpose:

``` elisp
;; Each defcontext generates: NAME-context, NAME-provider, use-NAME
(vui-defcontext user nil)
(vui-defcontext app-theme 'light)  ; Using app-theme to avoid conflict with state variable
(vui-defcontext i18n nil)

(vui-defcomponent app ()
  :state ((user nil) (theme 'light) (locale "en"))
  :render
  (user-provider user
    (app-theme-provider theme
      (i18n-provider (list :locale locale :t #'translate)
        (vui-component 'main-layout)))))
```

Each context is independent. Components subscribe only to what they need - a component that only cares about the user won't re-render when the theme changes.

# Composition Patterns

Context solves the problem of *getting* data to deeply nested components. Composition patterns solve a different problem: *structuring* components so they stay maintainable as complexity grows.

Here are patterns that work well in vui.el, each solving a specific problem.

## Container/Presentational Split

### The Problem

As components grow, they often mix data management with rendering. A user list component might fetch data, handle loading states, transform the response, *and* render the UI. This creates several issues:

- You can't test the rendering without triggering the data fetch
- You can't reuse the UI with different data sources
- The component does too many things, making it hard to understand

### The Pattern

Split into two components: a *container* that manages data, and a *presentational* component that just renders what it receives:

``` elisp
;; Container: manages data, decides what to render
(vui-defcomponent user-list-container ()
  :render
  (let ((result (vui-use-async 'users
                  (lambda (resolve _reject)
                    (fetch-users-async resolve)))))
    (pcase (plist-get result :status)
      ('pending (vui-component 'loading-spinner))
      ('ready (vui-component 'user-list-view
                :users (plist-get result :data))))))

;; Presentational: pure rendering, receives everything via props
(vui-defcomponent user-list-view (users)
  :render
  (vui-vstack
   (vui-text "Users" :face 'bold)
   (vui-newline)
   (vui-list users
     (lambda (user)
       (vui-component 'user-row :user user))
     (lambda (user) (plist-get user :id)))))

(vui-defcomponent user-row (user)
  :render
  (vui-hstack
   (vui-text (plist-get user :name))
   (vui-space)
   (vui-text (plist-get user :email) :face 'shadow)))
```

Note: `use-async` is a hook for asynchronous data loading - we'll cover it in detail in the Hooks article. For now, just notice the separation: the container handles the async complexity, the view just renders.

### When to Use

- When you have complex data logic (fetching, transforming, caching)
- When you want views you can test with mock data
- When the same view might be used with different data sources

### When to Avoid

- Simple components where the split adds overhead without benefit
- Components where data and display are tightly coupled by design

## Compound Components

### The Problem

Some UI patterns are conceptually one thing but require multiple cooperating pieces. Consider tabs: you need a tab bar that shows the labels, tab panels that hold the content, and shared state tracking which tab is active.

You *could* expose all this to the user:

``` elisp
;; Clunky: user manages all the wiring
(vui-defcomponent my-page ()
  :state ((active-tab 0))
  :render
  (vui-vstack
   (vui-hstack
    (vui-button "General"
      :on-click (lambda () (vui-set-state :active-tab 0)))
    (vui-button "Settings"
      :on-click (lambda () (vui-set-state :active-tab 1))))
   (pcase active-tab
     (0 (vui-component 'general-panel))
     (1 (vui-component 'settings-panel)))))
```

This works, but every time someone uses tabs they have to write the same state management and click handlers. The abstraction is leaky.

### The Pattern

Create compound components that manage their shared state internally:

``` elisp
;; Define a tabs context for internal communication
(vui-defcontext tabs nil)

(vui-defcomponent tabs (children)
  :state ((active-index 0))
  :render
  (let ((tab-labels (mapcar (lambda (child)
                              (plist-get child :label))
                            children)))
    (tabs-provider
        (list :active active-index
              :set-active (lambda (i) (vui-set-state :active-index i)))
      (vui-vstack
       ;; Tab bar - render a button for each label
       (vui-hstack
        :spacing 1
        (let ((idx 0))
          (mapcar (lambda (label)
                    (let ((current-idx idx))
                      (prog1
                          (vui-button label
                            :face (if (= current-idx active-index) 'bold 'default)
                            :on-click (lambda ()
                                        (vui-set-state :active-index current-idx)))
                        (setq idx (1+ idx)))))
                  tab-labels)))
       (vui-newline)
       ;; Active panel - render only the selected child
       (nth active-index children)))))

(vui-defcomponent tab-panel (label children)
  :render
  ;; Just renders its children - label is used by parent
  (vui-fragment children))

;; Usage: clean and declarative
(vui-defcomponent settings-page ()
  :render
  (vui-component 'tabs
    :children
    (list
     (vui-component 'tab-panel :label "General"
       :children (vui-text "General settings content"))
     (vui-component 'tab-panel :label "Appearance"
       :children (vui-text "Appearance settings content"))
     (vui-component 'tab-panel :label "Advanced"
       :children (vui-text "Advanced settings content")))))

(vui-mount (vui-component 'settings-page))
```

The user declares the structure, the compound component handles the behaviour. No manual state wiring required.

### When to Use

- Related elements that share internal state (tabs, accordions, dropdown menus)
- When you want a clean API that hides internal complexity
- Reusable UI patterns that appear in multiple places

### When to Avoid

- Single-purpose components that don't need internal coordination
- When users need fine-grained control over the internal state

## Render Props

### The Problem

Sometimes a component knows *what* to do - track some state, manage a timer, handle form validation - but shouldn't dictate *how* to display it. If you hardcode the display, you limit reusability.

Consider a simple toggle. The logic is always the same (track on/off, provide a way to flip it), but the display varies wildly: a button, a checkbox, a switch, styled text, an icon…

### The Pattern

Pass a function that receives the state and returns UI:

``` elisp
;; The component manages state, caller decides display
(vui-defcomponent toggle-state (render-fn)
  :state ((on nil))
  :render
  (funcall render-fn
           on
           (lambda () (vui-set-state :on (not on)))))

;; Usage: different displays, same logic
(vui-defcomponent button-toggle ()
  :render
  (vui-component 'toggle-state
    :render-fn (lambda (on toggle)
                 (vui-button (if on "[ON]" "[OFF]")
                   :on-click toggle))))

(vui-defcomponent text-toggle ()
  :render
  (vui-component 'toggle-state
    :render-fn (lambda (on toggle)
                 (vui-hstack
                  (vui-text (if on "Enabled" "Disabled")
                    :face (if on 'success 'shadow))
                  (vui-space)
                  (vui-button "Toggle" :on-click toggle)))))

(vui-defcomponent icon-toggle ()
  :render
  (vui-component 'toggle-state
    :render-fn (lambda (on toggle)
                 (vui-button (if on "✓" "✗")
                   :face (if on 'success 'error)
                   :on-click toggle))))
```

The `toggle-state` component is "headless" - it provides behaviour without opinions about presentation. The caller decides how to render.

### When to Use

- When the same logic needs different presentations
- Building reusable "headless" components
- When you want maximum flexibility for consumers

### When to Avoid

- When the display is always the same - just render it directly
- When the indirection adds complexity without benefit

## Slots Pattern

### The Problem

Layout components need flexibility. A card might have a header, content, and footer - but different uses need different things in each spot. You can't anticipate every combination:

``` elisp
;; Inflexible: what if someone needs two buttons? An icon? Nothing?
(vui-defcomponent card (title body button-text on-click)
  :render
  (vui-vstack
   (vui-text title :face 'bold)
   (vui-text body)
   (vui-button button-text :on-click on-click)))
```

### The Pattern

Define named "slots" that accept arbitrary content:

``` elisp
(vui-defcomponent card (header content footer)
  :render
  (vui-vstack
   ;; Header slot
   (when header
     (vui-fragment
      (vui-box header :face 'bold)
      (vui-newline)))
   ;; Content slot (required)
   content
   ;; Footer slot (optional)
   (when footer
     (vui-fragment
      (vui-newline)
      (vui-text "---")
      (vui-newline)
      footer))))

;; Usage: full card with all slots
(vui-component 'card
  :header (vui-text "Card Title")
  :content (vui-vstack
            (vui-text "Main content goes here.")
            (vui-text "Can be multiple elements."))
  :footer (vui-hstack
           :spacing 1
           (vui-button "Cancel")
           (vui-button "Save")))

;; Usage: minimal card, just content
(vui-component 'card
  :content (vui-text "Simple card with no header or footer"))

;; Usage: card with complex header
(vui-component 'card
  :header (vui-hstack
           (vui-text "Title")
           (vui-space)
           (vui-button "×"))
  :content (vui-text "Card with close button in header"))
```

Slots let you create flexible layout components without anticipating every use case. The component defines *where* things go, the caller decides *what* goes there.

### When to Use

- Flexible layouts with named regions (cards, modals, dialogs, page templates)
- When different uses need different content in the same structural positions
- Building a component library with composable pieces

### When to Avoid

- Simple, fixed layouts where a few props suffice
- When the flexibility isn't needed and adds cognitive overhead

## Pattern Summary

| Pattern | Problem it Solves | Use When | Avoid When |
|----|----|----|----|
| Container/Presentational | Data logic tangled with rendering | Complex data needs, testable views | Simple components |
| Compound Components | Related pieces need shared internal state | Tabs, accordions, menus | Single-purpose components |
| Render Props | Logic is reusable, display varies | Headless components, maximum flexibility | Display is always the same |
| Slots | Layouts need flexible content regions | Cards, modals, page templates | Fixed layouts, few variations |

# Organising Larger Applications

As your app grows, organise code by feature rather than by type:

``` text
my-app/
├── my-app.el           ; Entry point, main component
├── my-app-context.el   ; Shared contexts
├── my-app-users/       ; User feature
│   ├── user-list.el
│   ├── user-form.el
│   └── user-api.el
├── my-app-settings/    ; Settings feature
│   ├── settings-page.el
│   └── settings-form.el
└── my-app-shared/      ; Reusable components
    ├── card.el
    ├── modal.el
    └── form-fields.el
```

Each feature is self-contained. Shared components live separately. The main file wires everything together.

Why by feature instead of by type (all components in one folder, all APIs in another)? Because when you work on "users," you want all the user-related code together. You're more likely to change `user-list.el` and `user-api.el` together than `user-list.el` and `settings-page.el`.

# State Management Strategies

Where should state live? Here's a decision guide:

| State Type                | Location                | Example               |
|---------------------------|-------------------------|-----------------------|
| UI-only, single component | Component `:state`      | Form input, toggle    |
| Shared by siblings        | Lift to parent          | Selected item in list |
| App-wide, changes rarely  | Context                 | Theme, user, locale   |
| App-wide, changes often   | Top-level state + props | Search results, data  |

Avoid putting rapidly-changing state in context - every change re-renders all consumers. For frequently updating data, pass it through props or use a more targeted approach.

# Example: Refactoring with Context

Let's see context in action with a task app where filter state needs to reach multiple components.

## Before: Prop Drilling

``` elisp
;; Prop drilling: filter passed through every level
(vui-defcomponent task-app ()
  :state ((filter 'all))
  :render
  (vui-vstack
   (vui-component 'task-header)
   (vui-component 'task-list-container
     :filter filter
     :set-filter (lambda (f) (vui-set-state :filter f)))
   (vui-component 'filter-bar
     :filter filter
     :set-filter (lambda (f) (vui-set-state :filter f)))))
```

Both `task-list-container` and `filter-bar` need `filter` and `set-filter`. If we add more components or nesting, this gets tedious. Every new component in the chain has to pass these props through.

## After: Context

``` elisp
;;; -*- lexical-binding: t -*-

;; Generates: task-filter-context, task-filter-provider, use-task-filter
(vui-defcontext task-filter nil)

(vui-defcomponent task-app ()
  :state ((filter 'all))  ; all, active, completed
  :render
  (task-filter-provider
      (list :filter filter
            :set-filter (lambda (f) (vui-set-state :filter f)))
    (vui-vstack
     (vui-component 'task-header)
     (vui-component 'task-list-container)
     (vui-component 'filter-bar))))

;; Simple header - doesn't need filter, doesn't receive it
(vui-defcomponent task-header ()
  :render
  (vui-text "Tasks" :face 'bold))

;; Filter bar accesses context directly
(vui-defcomponent filter-bar ()
  :render
  (let* ((ctx (use-task-filter))
         (current (plist-get ctx :filter))
         (set-filter (plist-get ctx :set-filter)))
    (vui-hstack
     :spacing 1
     (mapcar
      (lambda (f)
        (vui-button (if (eq f current)
                        (concat "*" (symbol-name f) "*")
                      (symbol-name f))
          :face (if (eq f current) 'bold 'default)
          :on-click (lambda () (funcall set-filter f))))
      '(all active completed)))))

;; Task list accesses context directly
(vui-defcomponent task-list-container ()
  :state ((tasks '((:id 1 :text "Learn vui.el" :done t)
                   (:id 2 :text "Build something" :done nil))))
  :render
  (let* ((ctx (use-task-filter))
         (filter (plist-get ctx :filter))
         (filtered (pcase filter
                     ('all tasks)
                     ('active (seq-filter
                               (lambda (it) (not (plist-get it :done)))
                               tasks))
                     ('completed (seq-filter
                                  (lambda (it) (plist-get it :done))
                                  tasks)))))
    (vui-list filtered
      (lambda (task)
        (vui-text (format "%s %s"
                   (if (plist-get task :done) "✓" "○")
                   (plist-get task :text))))
      (lambda (task) (plist-get task :id)))))

;; Try it!
(vui-mount (vui-component 'task-app))
```

The filter state lives at the top, but `filter-bar` and `task-list-container` access it directly. `task-header` doesn't need to know about filters at all, and doesn't receive any filter-related props.

``` example
# Default view
Tasks
✓ Learn vui.el
○ Build something
[*all*] [active] [completed]

# Clicked on "active"
Tasks
○ Build something
[all] [*active*] [completed]

# Clicked on "completed"
Tasks
✓ Learn vui.el
[all] [active] [*completed*]
```

# Summary

Managing complexity in vui.el comes down to:

1.  **Use context** for truly global data (theme, user, locale) - but don't overuse it
2.  **Use props** for component-specific data flow - explicit is usually clearer
3.  **Split containers from presentational** components when data logic is complex
4.  **Compose with patterns** - compound components, render props, slots - when they solve real problems
5.  **Organise by feature** as the app grows
6.  **Lift state minimally** - only as high as needed, no higher

The goal is always clarity: when you look at a component, you should understand where its data comes from and how it flows. Context and composition patterns are tools to achieve that at scale, not ends in themselves.

# What's Next

We've covered the declarative layer of vui.el. The next articles dive deeper:

- **Hooks Deep Dive**: All available hooks and how to create custom ones
- **Async Data Loading**: Practical patterns for fetching and caching
- **Under the Hood**: How vui.el's rendering actually works
