I have around 13,800 org files. Every time I started Emacs, `vulpea-db-autosync-mode` would freeze for a noticeable moment while it set up file monitoring and scanned for changes. Not catastrophic, but annoying enough to investigate.

The fix turned out to be straightforward once I actually measured where the time was going. The result: **1371ms down to 8ms** - a 171x improvement.

<!--more-->

# Measure first

I've learned the hard way that optimising without measurements is just guessing. So the first step was adding timing instrumentation to `vulpea-db-sync--start`, gated behind a `vulpea-db-sync-debug` variable:

``` commonlisp
(setq vulpea-db-sync-debug t)
(vulpea-db-autosync-mode 1)
```

This logs timestamps for each phase:

``` example
[vulpea-sync] cleanup-deleted-files: 135ms (0 removed)
[vulpea-sync] list+enqueue: 1008ms (13813 files)
[vulpea-sync] watch-directory: 224ms (97 watchers)
[vulpea-sync] setup-external-monitoring: 0ms
[vulpea-sync] start complete: 1371ms total
```

The breakdown immediately told me what to fix. Three synchronous operations were blocking the startup path:

1.  **File listing** (1008ms) - `directory-files-recursively` walking 13,800 files
2.  **Filenotify setup** (224ms) - 97 recursive `file-notify-add-watch` calls
3.  **Deleted file cleanup** (135ms) - calling `file-exists-p` on every path in the database

# Eliminating the file listing

The biggest cost was `directory-files-recursively`. This is Emacs Lisp walking the filesystem tree synchronously. But we already have tools that do this much faster: `fd` and `find`.

The fix: launch a subprocess via `make-process` and handle the result in a sentinel callback.

``` commonlisp
(defun vulpea-db-sync--scan-files-async (dirs callback)
  "List org files in DIRS asynchronously via subprocess."
  (let* ((buffer "")
         (dir (car dirs))
         (cmd (if (executable-find "fd")
                  (list "fd" "--type" "f" "--extension" "org"
                        "--hidden" "--no-ignore"
                        "--exclude" ".*"
                        "." (expand-file-name dir))
                (list "find" (expand-file-name dir)
                      "-type" "f" "-name" "*.org"
                      "-not" "-path" "*/.*"))))
    (make-process
     :name "vulpea-scan"
     :command cmd
     :connection-type 'pipe
     :noquery t
     :filter (lambda (_proc output)
               (setq buffer (concat buffer output)))
     :sentinel (lambda (_proc event)
                 (when (string-prefix-p "finished" event)
                   (let ((files (split-string buffer "\n" t)))
                     (funcall callback files)))))))
```

The subprocess returns immediately. When `fd` finishes (about 340ms later), the sentinel enqueues all files for change detection. But that 340ms happens in the background - Emacs is fully interactive the whole time.

# Eliminating the cleanup

The old cleanup function queried every path from the database and called `file-exists-p` on each one. With 13,800 files, that's 13,800 stat calls.

Since the `fd` subprocess already gives us the complete list of existing files, we can use it for cleanup too. Instead of checking each DB path against the filesystem, we build a hash set of the `fd` output and remove DB entries that aren't in it:

``` commonlisp
(defun vulpea-db-sync--cleanup-deleted-files-using (existing-files)
  "Remove DB entries for files not in EXISTING-FILES list."
  (let* ((db (vulpea-db))
         (existing-set (make-hash-table :test 'equal
                                        :size (length existing-files)))
         (all-paths (mapcar #'car
                     (emacsql db [:select path :from files]))))
    (dolist (f existing-files)
      (puthash f t existing-set))
    (emacsql-with-transaction db
      (dolist (path all-paths)
        (unless (gethash path existing-set)
          (vulpea-db--delete-file-notes path)
          (emacsql db [:delete :from files
                       :where (= path $s1)] path))))))
```

One subprocess, two purposes. Cleanup dropped from 135ms to 48ms and runs in the background.

# Skipping redundant filenotify

With the subprocess changes, the remaining blocker was `file-notify-add-watch` setup: 237ms to create 97 watchers across all subdirectories. But then I looked at my config:

``` commonlisp
(setq vulpea-db-sync-external-method 'fswatch)
```

I'm using `fswatch` for external change detection. And `fswatch` monitors the entire directory tree - including changes made from within Emacs. So filenotify is completely redundant.

What about programmatic changes? `vulpea-create` and `vulpea-utils-with-note-sync` both call `vulpea-db-update-file` directly. They never relied on filenotify in the first place.

The fix: skip filenotify setup when fswatch is active.

``` commonlisp
(if vulpea-db-sync--fswatch-process
    ;; fswatch handles all filesystem monitoring
    nil
  ;; No external monitor, use filenotify
  (dolist (dir vulpea-db-sync-directories)
    (vulpea-db-sync--watch-directory dir)))
```

# Results

``` example
[vulpea-sync] setup-external-monitoring: 7ms
[vulpea-sync] watch-directory: skipped (fswatch active)
[vulpea-sync] launching async scan subprocess...
[vulpea-sync] start complete: 8ms total (sync portion)
```

| Phase                     | Before     | After                         |
|---------------------------|------------|-------------------------------|
| cleanup-deleted-files     | 135ms      | 0ms (deferred to subprocess)  |
| list+enqueue              | 1008ms     | 0ms (async subprocess)        |
| watch-directory           | 224ms      | 0ms (skipped, fswatch active) |
| setup-external-monitoring | 0ms        | 7ms (fswatch launch)          |
| **Total blocking**        | **1371ms** | **8ms**                       |

The total work hasn't decreased - files still get listed, hashes still get compared, changes still get detected. But none of it blocks the startup path. Emacs is interactive in 8 milliseconds.

# Takeaways

The changes are small in terms of code. The insights are even simpler:

- **Measure before optimising.** I assumed the queue processing was the problem. It wasn't - it was the setup.
- **Use subprocesses.** Emacs is single-threaded, but `fd` and `fswatch` aren't. Let them do the work.
- **Don't duplicate monitoring.** If `fswatch` is watching the filesystem, filenotify is just burning CPU for nothing.

The instrumentation (`vulpea-db-sync-debug`) stays in the codebase. Next time something feels slow, the first step is `(setq vulpea-db-sync-debug t)`.
