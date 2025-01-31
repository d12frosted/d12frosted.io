In the [previous article](/posts/2020-06-25-task-management-with-roam-vol3) we covered automatic tagging of notes related to a specific person, and today we are going to cover automatic tagging of an org-mode heading upon insertion of link related to a person. To put it simple, when I mention someone in the task, I would love this task to be automatically tagged with that persons name. As they say, it's better to see once, than imagine multiple times, so here is a screencast.

<img src="/images/2020-07-07-task-management-with-roam-vol4/2022-07-19_21-13-09_org-notes-insert.gif" class="d12-image-3/4" />

**Change Log:**

- `[2021-01-24 Sun]`: Since some of the functionality mentioned in the original article was merged to `org-roam`, all code is updated to reflect the current state of affairs.
- `[2021-03-02 Tue]`: Update naming convention to match [personal configurations](https://github.com/d12frosted/environment/tree/master/emacs).
- `[2021-05-10 Mon]`: Update post to reflect changes in [org-roam v2](https://github.com/org-roam/org-roam/pull/1401). Previous version of this article is available on [GitHub](https://github.com/d12frosted/d12frosted.io/blob/c16870cab6ebbaafdf73c7c3589abbd27c20ac52/posts/2020-07-07-task-management-with-roam-vol4.org).
- `[2021-11-19 Fri]`: Update post to reflect [inclusion](https://github.com/d12frosted/vulpea/commit/8ff428f2e9561fdc448627fe780be03a661cc52e) of `vulpea-insert` function to `vulpea` library. You can find previous version of this article in [git history](https://github.com/d12frosted/d12frosted.io/blob/2d3dad81988e838b8159761cd420bb95ed5bdd83/posts/2020-07-07-task-management-with-roam-vol4.org).

``` related_posts
```

<!--more-->

Once could just write an advice for `org-roam-node-insert` by using a relatively recent [change](https://github.com/org-roam/org-roam/pull/839) that makes this function to return what was inserted. This also uses name manipulation from the [previous article](/posts/2020-06-25-task-management-with-roam-vol3) and tags lookup from [Org-roam tags](/posts/2020-06-10-org-roam-tags) article.

``` commonlisp
(defun org-roam-node-insert-wrapper (fn)
  "Insert a link to the note using FN.

If inserted node has PEOPLE tag on it, tag the current outline
accordingly."
  (interactive)
  (when-let*
      ((node (funcall fn))
       (title (org-roam-node-title node))
       (tags (org-roam-node-tags node)))
    (when (seq-contains-p tags "people")
      (save-excursion
        (ignore-errors
          (org-back-to-heading)
          (org-set-tags
           (seq-uniq
            (cons
             (vulpea--title-to-tag title)
             (org-get-tags nil t)))))))))

(advice-add
 #'org-roam-node-insert
 :around
 #'org-roam-node-insert-wrapper)
```

The implementation is straight-forward. We start with calling `fn` (e.g. `org-roam-node-insert`) that asks for the note to insert. Then we parse result and query the roam tags to understand if the inserted note is related to a person. And if the answer is yes, we use `org-set-tags` to automatically tag the heading.

And while advicing is powerful tool and allows us to solve the problem, there is slightly different, less intrusive and composable solution provided by `vulpea` library - `vulpea-insert` function that acts like `org-roam-node-insert`, but provides ability setup hooks on insertion. First, we define a handler (pretty much the same as `org-roam-node-insert-wrapper` but without any calls to insertion function).

``` commonlisp
(defun my-vulpea-insert-handle (note)
  "Hook to be called on NOTE after `vulpea-insert'."
  (when-let* ((title (vulpea-note-title note))
              (tags (vulpea-note-tags note)))
    (when (seq-contains-p tags "people")
      (save-excursion
        (ignore-errors
          (org-back-to-heading)
          (when (eq 'todo (org-element-property
                           :todo-type
                           (org-element-at-point)))
            (org-set-tags
             (seq-uniq
              (cons
               (vulpea--title-to-tag title)
               (org-get-tags nil t))))))))))
```

And then you just need to add it as a hook:

``` commonlisp
(add-hook 'vulpea-insert-handle-functions
          #'my-vulpea-insert-handle)
```

With this approach you can add as many handlers as you wish without the need to grow your advice/wrapper too much.

# Complete solution

``` commonlisp
(defun my-vulpea-insert-handle (note)
  "Hook to be called on NOTE after `vulpea-insert'."
  (when-let* ((title (vulpea-note-title note))
              (tags (vulpea-note-tags note)))
    (when (seq-contains-p tags "people")
      (save-excursion
        (ignore-errors
          (org-back-to-heading)
          (when (eq 'todo (org-element-property
                           :todo-type
                           (org-element-at-point)))
            (org-set-tags
             (seq-uniq
              (cons
               (vulpea--title-to-tag title)
               (org-get-tags nil t))))))))))

(defun vulpea--title-to-tag (title)
  "Convert TITLE to tag."
  (concat "@" (s-replace " " "" title)))

(add-hook 'vulpea-insert-handle-functions
          #'my-vulpea-insert-handle)

```

# References

- `org-roam` documentation on [GitHub](https://github.com/org-roam/org-roam).
- `org-mode` documentation on the [official site](https://orgmode.org).
- [Org-roam tags](/posts/2020-06-10-org-roam-tags) post.
- personal configurations on [GitHub](https://github.com/d12frosted/environment/blob/master/emacs/lisp/%2Borg-notes.el).
