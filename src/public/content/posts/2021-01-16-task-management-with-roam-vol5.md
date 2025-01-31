In previous articles ([Vol. 1](/posts/2020-06-23-task-management-with-roam-vol1) and [Vol. 2](/posts/2020-06-24-task-management-with-roam-vol2)) we talked about moving tasks from regular `org-mode` files to `org-roam` notes. This relied upon adding all `org-roam` files to `org-agenda-files`, which doesn't scale well, as when you build an agenda buffer, it needs to traverse each file. Once you have more than 1k notes, things become sluggish.

In my experience, once I reached 1200 note files, `org-agenda` constantly took more than 50 seconds to build, rendering this tool completely useless. But then I realised that only 3% of those files actually contain any `TODO` entries, so there is no need to traverse whole `org-roam-directory`!

In this article we are going to optimise `org-agenda` back to less than 1 second by dynamically building `org-agenda-files` list to include only files with `TODO` entries. All thanks to the power of `org-roam` and some hooks I am going to describe.

**Change Log:**

- `[2021-03-02 Tue]`: Update naming convention to match [personal configurations](https://github.com/d12frosted/environment/tree/master/emacs).
- `[2021-03-08 Mon]`: [Gustav](https://github.com/Whil-) shared that `org-element-map` has an optional parameter `first-match` that works like `seq-find`, meaning that `vulpea-project-p` can be optimised.
- `[2021-05-10 Mon]`: Update post to reflect changes in [org-roam v2](https://github.com/org-roam/org-roam/pull/1401). Previous version of this article is available on [GitHub](https://github.com/d12frosted/d12frosted.io/blob/c16870cab6ebbaafdf73c7c3589abbd27c20ac52/posts/2021-01-16-task-management-with-roam-vol5.org).
- `[2021-08-19 Thu]`: [Gustav](https://github.com/Whil-) proposed to modify buffer only when tags have changed. Code was updated accordingly (both in the post and on [GitHub Gist](https://gist.github.com/d12frosted/a60e8ccb9aceba031af243dff0d19b2e)).
- `[2021-09-07 Tue]`: [rngesus-wept](https://github.com/rngesus-wept) proposed an interesting [solution](https://github.com/d12frosted/d12frosted.io/issues/15#issuecomment-910213001) on how to make sure that any extra stuff in `org-agenda-files` are not wiped out.

``` related_posts
```

<!--more-->

The core idea is very simple - optimising reads during writes. So every time a file is modified, we check if it contains any `TODO` entries, and depending on that we either add or remove a `project` tag from `filetags` property. And then, before calling `org-agenda`, we simply `org-roam-db-query` for files that have a `project` tag.

Since `filetags` are [inherited](https://orgmode.org/manual/Tag-Inheritance.html) by default (see the value of `org-use-tag-inheritance`), every heading in your file will inherit `project` tag, which is not desirable. Since tag inheritance is useful in general, my advice is to disable inheritance specifically for `project` tag by adding it to `org-tags-exclude-from-inheritance`:

``` commonlisp
(add-to-list 'org-tags-exclude-from-inheritance "project")
```

<img src="/images/2021-01-16-task-management-with-roam-vol5/2022-07-19_21-14-37_org-notes-project-tag-update.gif" class="d12-image-3/4" />

# Marking a Project

In order to mark a note as a `project`, we need to check if it contains any `TODO` entries. One of the way to do it is to use [Org Element API](https://orgmode.org/worg/dev/org-element-api.html), a set of parsing functions.

``` commonlisp
(defun vulpea-project-p ()
  "Return non-nil if current buffer has any todo entry.

TODO entries marked as done are ignored, meaning the this
function returns nil if current buffer contains only completed
tasks."
  (org-element-map                          ; (2)
       (org-element-parse-buffer 'headline) ; (1)
       'headline
     (lambda (h)
       (eq (org-element-property :todo-type h)
           'todo))
     nil 'first-match))                     ; (3)
```

This might look a little bit too much, so let me explain the code step by step.

1.  We parse the buffer using `org-element-parse-buffer`. It returns an abstract syntax tree of the current Org buffer. But sine we care only about headings, we ask it to return only them by passing a `GRANULARITY` parameter - `'headline`. This makes things faster.
2.  Then we extract information about `TODO` keyword from `headline` AST, which [contains a property](https://orgmode.org/worg/dev/org-element-api.html#org658999f) we are interested in - `:todo-type`, which returns the type of `TODO` keyword according to `org-todo-keywords` - `'done`, `'todo` or `nil` (when keyword is not present).
3.  Now all we have to do is to check if the buffer list contains at least one keyword with `'todo` type. We could use `seq=find` on the result of `org-element-map`, but it turns out that it provides an optional `first-match` argument that can be used for our needs. Thanks [Gustav](https://github.com/Whil-) for pointing that out.

Now we need to use this function to add or to remove `project` tag from a note. I think that it should be done in two places - when visiting a note and in `before-save-hook`. This way you leave no room for missing a file with `TODO` entries. It uses [vulpea-buffer-tags-get](https://github.com/d12frosted/vulpea/blob/6a735c34f1f64e1f70da77989e9ce8da7864e5ff/vulpea-buffer.el#L69) and [vulpea-buffer-tags-add](https://github.com/d12frosted/vulpea/blob/6a735c34f1f64e1f70da77989e9ce8da7864e5ff/vulpea-buffer.el#L79) from [vulpea](https://github.com/d12frosted/vulpea) library (for now you should use [org-roam-v2 branch](https://github.com/d12frosted/vulpea/pull/92)).

``` commonlisp
(add-hook 'find-file-hook #'vulpea-project-update-tag)
(add-hook 'before-save-hook #'vulpea-project-update-tag)

(defun vulpea-project-update-tag ()
      "Update PROJECT tag in the current buffer."
      (when (and (not (active-minibuffer-window))
                 (vulpea-buffer-p))
        (save-excursion
          (goto-char (point-min))
          (let* ((tags (vulpea-buffer-tags-get))
                 (original-tags tags))
            (if (vulpea-project-p)
                (setq tags (cons "project" tags))
              (setq tags (remove "project" tags)))

            ;; cleanup duplicates
            (setq tags (seq-uniq tags))

            ;; update tags if changed
            (when (or (seq-difference tags original-tags)
                      (seq-difference original-tags tags))
              (apply #'vulpea-buffer-tags-set tags))))))

(defun vulpea-buffer-p ()
  "Return non-nil if the currently visited buffer is a note."
  (and buffer-file-name
       (string-prefix-p
        (expand-file-name (file-name-as-directory org-roam-directory))
        (file-name-directory buffer-file-name))))
```

That's it. Now whenever we modify or visit a notes buffer, this code will update the presence of `project` tag. See it in action:

<img src="/images/2021-01-16-task-management-with-roam-vol5/2022-07-19_21-14-37_org-notes-project-tag-update.gif" class="d12-image-3/4" />

# Building agenda

In order to dynamically build `org-agenda-files`, we need to query all files containing `project` tag. `org-roam` uses uses [skeeto/emacsql](https://github.com/skeeto/emacsql), and provides a convenient function `org-roam-db-query` to execute SQL statements against `org-roam-db-location` file.

``` commonlisp
(defun vulpea-project-files ()
  "Return a list of note files containing 'project' tag." ;
  (seq-uniq
   (seq-map
    #'car
    (org-roam-db-query
     [:select [nodes:file]
      :from tags
      :left-join nodes
      :on (= tags:node-id nodes:id)
      :where (like tag (quote "%\"project\"%"))]))))
```

This function simply returns a list of files containing `project` tag. Sure enough it can be generalised for other needs, but it's good enough for our simple use case. The query is run against the following schemes:

``` commonlisp
(nodes
 ([(id :not-null :primary-key)
   (file :not-null)
   (level :not-null)
   (pos :not-null)
   todo
   priority
   (scheduled text)
   (deadline text)
   title
   properties
   olp]
  (:foreign-key [file] :references files [file] :on-delete :cascade)))

(tags
 ([(node-id :not-null)
   tag]
  (:foreign-key [node-id] :references nodes [id] :on-delete :cascade)))
```

Now we can set the list of agenda files:

``` commonlisp
(setq org-agenda-files (vulpea-project-files))
```

But the real question is - when to do it? Some might put it in the `init.el` file and call it a day, but unless you are restarting Emacs like crazy, I would argue that it's not the best place to do it. Because we need an up to date list of files exactly when we build agenda.

``` commonlisp
(defun vulpea-agenda-files-update (&rest _)
  "Update the value of `org-agenda-files'."
  (setq org-agenda-files (vulpea-project-files)))

(advice-add 'org-agenda :before #'vulpea-agenda-files-update)
(advice-add 'org-todo-list :before #'vulpea-agenda-files-update)
```

And that's all. You `org-agenda` is up to date and fast again!

# Migration

So far we covered what to do with notes we edit. But when you have more than 10 notes it becomes tedious to visit each of them and make sure that they have update state of `Project` tag. Fortunately, this task is easily automated.

``` commonlisp
(dolist (file (org-roam-list-files))
  (message "processing %s" file)
  (with-current-buffer (or (find-buffer-visiting file)
                           (find-file-noselect file))
    (vulpea-project-update-tag)
    (save-buffer)))
```

This will visit each of your files and update the presence of `Project` tag according to presence of `TODO` entry. Now you are ready to go.

# Result

With little amount of `emacs-lisp` code we dramatically optimized `org-agenda` loading from $> 50$ seconds to $< 1$ second. Effectiveness of this approach depends on amount of files with `TODO` entries (the more you have, the less effective this approach becomes). One of the drawbacks is small (in my experience, neglectable) performance degradation of note visiting and note saving. Obviously, if a file contains thousands of headings, it affects performance. In defence, I would argue that such files are against the philosophy of `org-roam`, where you keep lots of small files as opposed to few huge files.

For you convenience, the full code is displayed below. It is also available as [GitHub Gist](https://gist.github.com/d12frosted/a60e8ccb9aceba031af243dff0d19b2e).

``` commonlisp
(defun vulpea-project-p ()
  "Return non-nil if current buffer has any todo entry.

TODO entries marked as done are ignored, meaning the this
function returns nil if current buffer contains only completed
tasks."
  (seq-find                                 ; (3)
   (lambda (type)
     (eq type 'todo))
   (org-element-map                         ; (2)
       (org-element-parse-buffer 'headline) ; (1)
       'headline
     (lambda (h)
       (org-element-property :todo-type h)))))

(defun vulpea-project-update-tag ()
    "Update PROJECT tag in the current buffer."
    (when (and (not (active-minibuffer-window))
               (vulpea-buffer-p))
      (save-excursion
        (goto-char (point-min))
        (let* ((tags (vulpea-buffer-tags-get))
               (original-tags tags))
          (if (vulpea-project-p)
              (setq tags (cons "project" tags))
            (setq tags (remove "project" tags)))

          ;; cleanup duplicates
          (setq tags (seq-uniq tags))

          ;; update tags if changed
          (when (or (seq-difference tags original-tags)
                    (seq-difference original-tags tags))
            (apply #'vulpea-buffer-tags-set tags))))))

(defun vulpea-buffer-p ()
  "Return non-nil if the currently visited buffer is a note."
  (and buffer-file-name
       (string-prefix-p
        (expand-file-name (file-name-as-directory org-roam-directory))
        (file-name-directory buffer-file-name))))

(defun vulpea-project-files ()
    "Return a list of note files containing 'project' tag." ;
    (seq-uniq
     (seq-map
      #'car
      (org-roam-db-query
       [:select [nodes:file]
        :from tags
        :left-join nodes
        :on (= tags:node-id nodes:id)
        :where (like tag (quote "%\"project\"%"))]))))

(defun vulpea-agenda-files-update (&rest _)
  "Update the value of `org-agenda-files'."
  (setq org-agenda-files (vulpea-project-files)))

(add-hook 'find-file-hook #'vulpea-project-update-tag)
(add-hook 'before-save-hook #'vulpea-project-update-tag)

(advice-add 'org-agenda :before #'vulpea-agenda-files-update)
(advice-add 'org-todo-list :before #'vulpea-agenda-files-update)
```

Thank you for your patience.

# References

- [Org Element API](https://orgmode.org/worg/dev/org-element-api.html)
- [skeeto/emacsql](https://github.com/skeeto/emacsql)
- Code from this article is available as [GitHub Gist](https://gist.github.com/d12frosted/a60e8ccb9aceba031af243dff0d19b2e)
