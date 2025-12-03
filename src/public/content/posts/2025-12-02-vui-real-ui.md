The [quickstart](/posts/2025-12-01-vui-quickstart) showed you the primitives. Now let's build something real with [vui.el](https://github.com/d12frosted/vui.el) - a file browser. We'll build it incrementally, with something runnable after each step.

If you haven't read the [quickstart](/posts/2025-12-01-vui-quickstart), note that [vui.el](https://github.com/d12frosted/vui.el) requires lexical binding. Either evaluate code in an `.el` file with `;;; -*- lexical-binding: t -*-` at the top, or run `M-: (setq-local lexical-binding t)` in your buffer.

``` related_posts
```

# What We're Building

A file browser with navigation, metadata display, and preview. Here's the mental model:

``` example
┌─────────────────────────────────────────┐
│ ~/Projects/                             │  <- Header: current path
├─────────────────────────────────────────┤
│ [↑ Parent] [Refresh] [Open in Dired]    │  <- Action buttons
├─────────────────────────────────────────┤
│ 📁 src/                                 │  <- File list with icons
│ 📁 tests/                               │
│ 📄 README.md          2.1 KB   Dec 01   │  <- Size and date columns
│ 📄 init.el            4.3 KB   Nov 28   │
│   ▸ selected                            │
├─────────────────────────────────────────┤
│ Preview:                                │  <- Preview pane
│ ;; My init file...                      │
└─────────────────────────────────────────┘
```

This suggests a component structure:

- `file-browser` - root, owns all state
- `browser-actions` - the action buttons
- `file-table` - the file listing
- `file-preview` - shows selected file content

We'll build toward this incrementally, starting much simpler.

# Step 1: The Simplest File List

**Goal:** Display a list of filenames in the current directory.

Let's start with the absolute minimum:

``` elisp
;;; -*- lexical-binding: t -*-

(defcomponent file-browser ()
  :state ((path default-directory))
  :render
  (let ((files (directory-files path)))
    (vui-vstack
     (vui-text (format "Files in %s:" path) :face 'bold)
     (vui-newline)
     (vui-list files
       (lambda (file)
         (vui-text file))))))

;; Try it!
(vui-mount (vui-component 'file-browser))
```

Run this and you'll see a list of filenames. Not pretty, but it works.

**What you learned:** Basic component with state, using `vui-list` to render a collection.

# Step 2: Make Files Clickable

**Goal:** Add selection - clicking a file highlights it and shows which one is selected.

Let's make each file a button:

``` elisp
(defcomponent file-browser ()
  :state ((path default-directory)
          (selected nil))
  :render
  (let ((files (directory-files path)))
    (vui-vstack
     (vui-text (format "Files in %s:" path) :face 'bold)
     (vui-newline)
     (if selected
         (vui-text (format "Selected: %s" selected))
       (vui-text "Click a file to select it" :face 'shadow))
     (vui-newline)
     (vui-list files
       (lambda (file)
         (vui-button file
           :on-click (lambda ()
                       (vui-set-state :selected file))
           :face (if (equal file selected) 'highlight nil)))))))

(vui-mount (vui-component 'file-browser))
```

Now clicking a file highlights it and shows the selection above the list.

**What you learned:** Selection state, conditional styling with `:face`.

# Step 3: Navigate Into Directories

**Goal:** Clicking a directory navigates into it. Directories get a different colour and icon.

Let's detect directories and handle them differently:

``` elisp
(defcomponent file-browser ()
  :state ((path default-directory)
          (selected nil))
  :render
  (let ((files (directory-files path t)))  ; t = full paths
    (vui-vstack
     (vui-text (abbreviate-file-name path) :face 'bold)
     (vui-newline)
     (vui-list files
       (lambda (file)
         (let* ((name (file-name-nondirectory file))
                (dir-p (file-directory-p file))
                (icon (if dir-p "📁" "📄"))
                (display (if dir-p (concat name "/") name)))
           (vui-button (format "%s %s" icon display)
             :on-click (lambda ()
                         (if dir-p
                             (vui-set-state :path file)
                           (vui-set-state :selected file)))
             :face (cond ((equal file selected) 'highlight)
                         (dir-p 'font-lock-function-name-face)
                         (t nil)))))
       (lambda (file) file)))))  ; key function

(vui-mount (vui-component 'file-browser))
```

Click a directory to navigate into it. Directories show in a different colour.

**What you learned:** Conditional behaviour based on file type, using key functions for list identity.

# Step 4: Add Parent Navigation

**Goal:** Add a button to go up one directory level. We'll also learn how to batch state updates.

``` elisp
(defcomponent file-browser ()
  :state ((path (expand-file-name default-directory))
          (selected nil))
  :render
  (let ((files (directory-files path t))
        (at-root (string= path "/")))
    (vui-vstack
     (vui-text (file-name-as-directory (abbreviate-file-name path)) :face 'bold)
     (vui-newline)
     ;; Parent button
     (unless at-root
       (vui-button "↑ Parent Directory"
         :on-click (lambda ()
                     (vui-batch
                      (vui-set-state :path (file-name-directory
                                            (directory-file-name path)))
                      (vui-set-state :selected nil)))))
     (vui-newline)
     ;; File list
     (vui-list files
       (lambda (file)
         (let* ((name (file-name-nondirectory file))
                (dir-p (file-directory-p file))
                (icon (if dir-p "📁" "📄"))
                (display (if dir-p (concat name "/") name)))
           (vui-button (format "%s %s" icon display)
             :on-click (lambda ()
                         (if dir-p
                             (vui-batch
                               (vui-set-state :path file)
                               (vui-set-state :selected nil))
                           (vui-set-state :selected file)))
             :face (cond ((equal file selected) 'highlight)
                         (dir-p 'font-lock-function-name-face)
                         (t nil)))))
       (lambda (file) file)))))

(vui-mount (vui-component 'file-browser))
```

Now you can navigate up and down the directory tree. Notice `vui-batch` - without it, each `vui-set-state` would trigger its own re-render. Wrapping them combines the updates into one. We'll cover re-rendering in more detail in a later article.

**What you learned:** Conditional rendering with `unless`, batching state updates with `vui-batch`.

# Step 5: Add File Metadata

**Goal:** Show file sizes and modification dates in aligned columns using `vui-table`.

``` elisp
(defun file-browser--format-size (size)
  "Format SIZE in human-readable form."
  (cond
   ((> size (* 1024 1024))
    (format "%.1f MB" (/ size (* 1024.0 1024))))
   ((> size 1024)
    (format "%.1f KB" (/ size 1024.0)))
   (t (format "%d B" size))))

(defcomponent file-browser ()
  :state ((path (expand-file-name default-directory))
          (selected nil))
  :render
  (let ((files (directory-files path t))
        (at-root (string= path "/")))
    (vui-vstack
     (vui-text (file-name-as-directory (abbreviate-file-name path)) :face 'bold)
     (vui-newline)
     (unless at-root
       (vui-fragment
        (vui-button "↑ Parent Directory"
          :on-click (lambda ()
                      (vui-batch
                       (vui-set-state :path (file-name-directory
                                             (directory-file-name path)))
                       (vui-set-state :selected nil))))
        (vui-newline)))
     ;; File table
     (vui-table
      :columns '((:min-width 25)            ; Name
                 (:width 10 :align :right)  ; Size
                 (:width 12))               ; Modified
      :rows
      (mapcar
       (lambda (file)
         (let* ((name (file-name-nondirectory file))
                (attrs (file-attributes file))
                (dir-p (file-directory-p file))
                (size (file-attribute-size attrs))
                (mtime (file-attribute-modification-time attrs))
                (icon (if dir-p "📁" "📄"))
                (display (if dir-p (concat name "/") name)))
           (list
            ;; Name column
            (vui-button (format "%s %s" icon display)
              :on-click (lambda ()
                          (if dir-p
                              (vui-batch
                                (vui-set-state :path file)
                                (vui-set-state :selected nil))
                            (vui-set-state :selected file)))
              :face (cond ((equal file selected) 'highlight)
                          (dir-p 'font-lock-function-name-face)
                          (t nil)))
            ;; Size column
            (if dir-p "" (file-browser--format-size size))
            ;; Date column
            (format-time-string "%b %d %H:%M" mtime))))
       files)))))

(vui-mount (vui-component 'file-browser))
```

Now files show with properly aligned size and date columns.

**What you learned:** Using `vui-table` for aligned columnar data, accessing file attributes.

# Step 6: Extract Components

**Goal:** The render function is getting unwieldy. Extract the file table into its own component for clarity and reusability.

``` elisp
(defun file-browser--format-size (size)
  "Format SIZE in human-readable form."
  (cond
   ((> size (* 1024 1024))
    (format "%.1f MB" (/ size (* 1024.0 1024))))
   ((> size 1024)
    (format "%.1f KB" (/ size 1024.0)))
   (t (format "%d B" size))))

(defcomponent file-table (files selected on-select on-navigate)
  :render
  (vui-table
   :columns '((:min-width 25)
              (:width 10 :align :right)
              (:width 12))
   :rows
   (mapcar
    (lambda (file)
      (let* ((name (file-name-nondirectory file))
             (attrs (file-attributes file))
             (dir-p (file-directory-p file))
             (size (file-attribute-size attrs))
             (mtime (file-attribute-modification-time attrs))
             (icon (if dir-p "📁" "📄"))
             (display (if dir-p (concat name "/") name)))
        (list
         (vui-button (format "%s %s" icon display)
           :on-click (lambda ()
                       (if dir-p
                           (funcall on-navigate file)
                         (funcall on-select file)))
           :face (cond ((equal file selected) 'highlight)
                       (dir-p 'font-lock-function-name-face)
                       (t nil)))
         (if dir-p "" (file-browser--format-size size))
         (format-time-string "%b %d %H:%M" mtime))))
    files)))

(defcomponent file-browser ()
  :state ((path (expand-file-name default-directory))
          (selected nil))
  :render
  (let ((files (directory-files path t))
        (at-root (string= path "/")))
    (vui-vstack
     (vui-text (file-name-as-directory (abbreviate-file-name path)) :face 'bold)
     (vui-newline)
     (unless at-root
       (vui-fragment
        (vui-button "↑ Parent Directory"
          :on-click (lambda ()
                      (vui-batch
                       (vui-set-state :path (file-name-directory
                                             (directory-file-name path)))
                       (vui-set-state :selected nil))))
        (vui-newline)))
     ;; Use the extracted component
     (vui-component 'file-table
       :files files
       :selected selected
       :on-select (lambda (file) (vui-set-state :selected file))
       :on-navigate (lambda (dir)
                      (vui-batch
                       (vui-set-state :path dir)
                       (vui-set-state :selected nil)))))))

(vui-mount (vui-component 'file-browser))
```

Same functionality, but now `file-table` is reusable and `file-browser` is cleaner.

**What you learned:** Extracting components, passing callbacks for child→parent communication.

# Step 7: Add File Preview

**Goal:** Show a preview of selected text files. We'll use `:on-mount` to load file contents when the component appears.

The `:on-mount` hook runs once when the component first renders - perfect for loading data:

``` elisp
(defcomponent file-preview (file)
  :state ((content nil)
          (error nil))
  :on-mount
  (condition-case err
      (vui-set-state :content
        (with-temp-buffer
          (insert-file-contents file nil 0 500)  ; First 500 bytes
          (buffer-string)))
    (error
     (vui-set-state :error (error-message-string err))))
  :render
  (vui-vstack
   (vui-text "Preview:" :face 'bold)
   (cond
    (error (vui-text error :face 'error))
    (content (vui-text content :face 'shadow))
    (t (vui-text "Loading..." :face 'shadow)))))

;; Update file-browser to include preview
(defcomponent file-browser ()
  :state ((path (expand-file-name default-directory))
          (selected nil))
  :render
  (let ((files (directory-files path t))
        (at-root (string= path "/")))
    (vui-vstack
     (vui-text (file-name-as-directory (abbreviate-file-name path)) :face 'bold)
     (vui-newline)
     (unless at-root
       (vui-fragment
        (vui-button "↑ Parent Directory"
          :on-click (lambda ()
                      (vui-batch
                       (vui-set-state :path (file-name-directory
                                             (directory-file-name path)))
                       (vui-set-state :selected nil))))
        (vui-newline)))
     (vui-component 'file-table
       :files files
       :selected selected
       :on-select (lambda (file) (vui-set-state :selected file))
       :on-navigate (lambda (dir)
                      (vui-batch
                       (vui-set-state :path dir)
                       (vui-set-state :selected nil))))
     ;; Preview pane
     (when selected
       (vui-fragment
        (vui-newline)
        (vui-component 'file-preview :file selected))))))

(vui-mount (vui-component 'file-browser))
```

Select a text file and see its contents below.

**What you learned:** Component lifecycle with `:on-mount`, error handling.

# Step 8: Add Action Buttons

**Goal:** Add a toolbar with refresh and "open in Dired" buttons. We'll also see the disabled state and a trick for forcing re-renders.

``` elisp
(defcomponent browser-actions (on-parent on-refresh on-dired at-root)
  :render
  (vui-hstack
   :spacing 1
   (vui-button "↑ Parent"
     :on-click on-parent
     :disabled at-root)
   (vui-button "Refresh"
     :on-click on-refresh)
   (vui-button "Open in Dired"
     :on-click on-dired)))

(defcomponent file-browser ()
  :state ((path (expand-file-name default-directory))
          (selected nil)
          (refresh-counter 0))  ; Changing this forces reload
  :render
  (let ((files (directory-files path t))
        (at-root (string= path "/")))
    (vui-vstack
     (vui-text (file-name-as-directory (abbreviate-file-name path)) :face 'bold)
     (vui-newline)
     ;; Action buttons
     (vui-component 'browser-actions
       :at-root at-root
       :on-parent (lambda ()
                    (vui-batch
                     (vui-set-state :path (file-name-directory
                                           (directory-file-name path)))
                     (vui-set-state :selected nil)))
       :on-refresh (lambda ()
                     (vui-set-state :refresh-counter (1+ refresh-counter)))
       :on-dired (lambda () (dired path)))
     (vui-newline)
     ;; File table
     (vui-component 'file-table
       :files files
       :selected selected
       :on-select (lambda (file) (vui-set-state :selected file))
       :on-navigate (lambda (dir)
                      (vui-batch
                        (vui-set-state :path dir)
                        (vui-set-state :selected nil))))
     ;; Preview
     (when selected
       (vui-fragment
        (vui-newline)
        (vui-component 'file-preview :file selected))))))

(vui-mount (vui-component 'file-browser))
```

**What you learned:** Disabled button state, forcing re-render with a counter trick.

# The Complete Browser

Here's everything together, with a helper function for easy launching:

``` elisp
;;; -*- lexical-binding: t -*-

(defun file-browser--format-size (size)
  "Format SIZE in human-readable form."
  (cond
   ((> size (* 1024 1024))
    (format "%.1f MB" (/ size (* 1024.0 1024))))
   ((> size 1024)
    (format "%.1f KB" (/ size 1024.0)))
   (t (format "%d B" size))))

(defcomponent browser-actions (on-parent on-refresh on-dired at-root)
  :render
  (vui-hstack
   :spacing 1
   (vui-button "↑ Parent"
     :on-click on-parent
     :disabled at-root)
   (vui-button "Refresh"
     :on-click on-refresh)
   (vui-button "Open in Dired"
     :on-click on-dired)))

(defcomponent file-table (files selected on-select on-navigate)
  :render
  (vui-table
   :columns '((:min-width 25)
              (:width 10 :align :right)
              (:width 12))
   :rows
   (mapcar
    (lambda (file)
      (let* ((name (file-name-nondirectory file))
             (attrs (file-attributes file))
             (dir-p (file-directory-p file))
             (size (file-attribute-size attrs))
             (mtime (file-attribute-modification-time attrs))
             (icon (if dir-p "📁" "📄"))
             (display (if dir-p (concat name "/") name)))
        (list
         (vui-button (format "%s %s" icon display)
           :on-click (lambda ()
                       (if dir-p
                           (funcall on-navigate file)
                         (funcall on-select file)))
           :face (cond ((equal file selected) 'highlight)
                       (dir-p 'font-lock-function-name-face)
                       (t nil)))
         (if dir-p "" (file-browser--format-size size))
         (format-time-string "%b %d %H:%M" mtime))))
    files)))

(defcomponent file-preview (file)
  :state ((content nil) (error nil))
  :on-mount
  (condition-case err
      (vui-set-state :content
        (with-temp-buffer
          (insert-file-contents file nil 0 500)
          (buffer-string)))
    (error (vui-set-state :error (error-message-string err))))
  :render
  (vui-vstack
   (vui-text "Preview:" :face 'bold)
   (cond
    (error (vui-text error :face 'error))
    (content (vui-text content :face 'shadow))
    (t (vui-text "Loading..." :face 'shadow)))))

(defcomponent file-browser (initial-path)
  :state ((path (or initial-path default-directory))
          (selected nil)
          (refresh-counter 0))
  :render
  (let ((files (directory-files path t))
        (at-root (string= path "/")))
    (vui-vstack
     (vui-text (file-name-as-directory (abbreviate-file-name path)) :face 'bold)
     (vui-newline)
     (vui-component 'browser-actions
       :at-root at-root
       :on-parent (lambda ()
                    (vui-batch
                     (vui-set-state :path (file-name-directory
                                           (directory-file-name path)))
                     (vui-set-state :selected nil)))
       :on-refresh (lambda ()
                     (vui-set-state :refresh-counter (1+ refresh-counter)))
       :on-dired (lambda () (dired path)))
     (vui-newline)
     (vui-component 'file-table
       :files files
       :selected selected
       :on-select (lambda (file) (vui-set-state :selected file))
       :on-navigate (lambda (dir)
                      (vui-batch
                       (vui-set-state :path dir)
                       (vui-set-state :selected nil))))
     (when selected
       (vui-fragment
        (vui-newline)
        (vui-component 'file-preview :file selected))))))

(defun file-browser-open (&optional path)
  "Open file browser at PATH or current directory."
  (interactive)
  (vui-mount (vui-component 'file-browser
               :initial-path (or path default-directory))))

;; Launch it!
(file-browser-open "~/")
```

# What We Built

In 8 incremental steps, we built a file browser with:

- Directory listing with metadata
- Navigation (into directories, up to parent)
- File selection with visual feedback
- Text file preview
- Action buttons

# Lessons Learned

## Start With the Mental Model

Before writing code, sketch what you're building. The ASCII diagram at the start wasn't decoration - it revealed the component structure. Each visual region suggested a component:

| Visual Region | Component         | Responsibility                 |
|---------------|-------------------|--------------------------------|
| Path display  | (inline)          | Show current directory         |
| Button row    | `browser-actions` | Navigation, refresh, dired     |
| File listing  | `file-table`      | Display entries, handle clicks |
| Preview pane  | `file-preview`    | Load and show file content     |

This decomposition emerged from the UI, not from code structure.

## State Lives at the Top

The root `file-browser` component owns all shared state: `path`, `selected`, `refresh-counter`. Child components receive data through props and communicate back through callbacks.

Why not let `file-table` own `selected`? Because the preview pane also needs it. When multiple components need the same data, lift it to their common ancestor.

## Extract When Complexity Grows

We didn't start with four components. We started with one and extracted as it grew:

- Step 5: Render function hit ~40 lines → extracted `file-table`
- Step 7: Added preview → new `file-preview` component
- Step 8: Added buttons → extracted `browser-actions`

Rule of thumb: extract when a render function exceeds 20-30 lines, or when you'd copy-paste code.

## Callbacks Enable Child→Parent Communication

Children can't modify parent state directly. Instead, parents pass callbacks:

``` elisp
;; Parent passes callback
(vui-component 'file-table
  :on-select (lambda (file) (vui-set-state :selected file)))

;; Child calls it
(vui-button name
  :on-click (lambda () (funcall on-select file)))
```

This is unidirectional data flow: data flows down (props), events flow up (callbacks).

## Components Own Their Concerns

`file-preview` loads its own content in `:on-mount`. The parent doesn't fetch preview data and pass it down - that would couple the parent to preview implementation details.

Ask: "Who needs to know about this?" If only one component cares, keep it local.

## The Refresh Trick

To force a re-render that re-reads the filesystem:

``` elisp
:state ((refresh-counter 0))

;; In refresh callback:
(vui-set-state :refresh-counter (1+ refresh-counter))
```

Changing any state triggers re-render. The counter itself isn't used - it just forces the refresh.

# What's Next

This browser works but has rough edges. The next article covers:

- Context for deeply shared state
- More composition patterns
- Organising larger applications
