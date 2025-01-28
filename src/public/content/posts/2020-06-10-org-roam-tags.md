[Org-roam](https://github.com/org-roam/org-roam) is a note-taking tool built on top of Emacs and Org. Essentially, it's a replica of [Roam Research](https://roamresearch.com). These tools provide an easy way to create and manage non-hierarchical notes. If you wish to learn more, just take a look at the [Org-roam manual](https://org-roam.github.io/org-roam/manual/) or watch [Making Connections in your Notes](https://www.youtube.com/watch?v=Lg61ocfxk3c) video by Matt Williams. Believe me, Org-roam and Roam Research are game-changers. Or even better, don't believe me and validate my claim by yourself.

Since I am already [addicted](/posts/2016-12-20-Being-an-org-mode-addict), it was only natural to prefer `org-roam` over some web application. Apart from being developed on top of mature Org ecosystem, Emacs brings many merits and extensibility is one of them. Once `org-roam` introduced tags system in [v1.1.1](https://github.com/org-roam/org-roam/blob/master/CHANGELOG.md#111-18-05-2020) I felt the lack of functions to manage them. Adding and removing them by hand is not nice. So in this article I am sharing a snippet that I've forged to ease the unbearable lightness of being.

**Change Log:**

- `[2020-10-12 Mon]`: Functionality described in this post (and similar functionality to manage aliases) is [merged to the upstream](https://github.com/org-roam/org-roam/pull/1183). Now simply use one of the following functions:
  - `org-roam-tag-add`
  - `org-roam-tag-delete`
  - `org-roam-alias-add`
  - `org-roam-alias-delete`
- `[2021-07-31 Sat]`: With [release of org-roam v2](https://github.com/org-roam/org-roam/releases/tag/v2.0.0) you should use the following functions:
  - `org-roam-tag-add`
  - `org-roam-tag-remove`
  - `org-roam-alias-add`
  - `org-roam-alias-remove`

<!--more-->

![](/images/2020-06-10-org-roam-tags/2022-07-19_21-01-06_org-roam-tags-demo.gif)

When it comes to tags removal, I just want to have a list of tags set in the current buffer and chose one of them to remove. Important thing here is that it should not allow me to remove tag set by directories (see `org-roam-tag-sources`).

On the other hand, when I add a tag, I want to see the list of all tags set either by buffer property or by directory. I can chose one of them (otherwise I tend to mistype) or add a completely new one.

So let's implement these two functions. But before that, we need to have a function to get the list of buffer wide tags. For this we can write some simple helper (that uses regexps) or reuse internal API from `org-roam` (that does all the dirty work for us).

``` commonlisp
(defun +org-notes-tags-read ()
  "Return list of tags as set in the buffer."
  (org-roam--extract-tags-prop (buffer-file-name (buffer-base-buffer))))
```

Now it's easy to implement the function to delete one of the buffer tags.

``` commonlisp
(defun +org-notes-tags-delete ()
  "Delete a tag from current note."
  (interactive)
  (unless (+org-notes-buffer-p)
    (user-error "Current buffer is not a note"))
  (let* ((tags (+org-notes-tags-read))
         (tag (completing-read "Tag: " tags nil 'require-match)))
    (+org-buffer-prop-set
     "ROAM_TAGS"
     (combine-and-quote-strings (delete tag tags)))
    (org-roam-db--update-tags)))
```

Since it works only in the context of `org-roam` it's good to have a meaningful error when this function used in the invalid context. Next we read the buffer tags and select one of them using `completing-read`. The `'require-match` is just a dummy non-nil value used instead of `t` as it improves readability.

Next we remove the selected tag from tags and set the result to the buffer property `ROAM_TAGS`, each tag quoted. Simple as that. I will provide implementation of `+org-notes-buffer-p` and `+org-buffer-prop-set` later on.

And the most important function is for adding tags.

``` commonlisp
(defun +org-notes-tags-add ()
  "Add a tag to current note."
  (interactive)
  (unless (+org-notes-buffer-p)
    (user-error "Current buffer is not a note"))
  (let* ((tags (seq-uniq
                (+seq-flatten
                 (+seq-flatten
                  (org-roam-db-query [:select tags :from tags])))))
         (tag (completing-read "Tag: " tags)))
    (when (string-empty-p tag)
      (user-error "Tag can't be empty"))
    (+org-buffer-prop-set
     "ROAM_TAGS"
     (combine-and-quote-strings (seq-uniq (cons tag (+org-notes-tags-read)))))
    (org-roam-db--update-tags)))
```

It also errors out when called outside of `org-roam` buffer. Then we query all tags from the `org-roam-db`. Since this is the list of lists of lists, we have two double flatten the result and then leave only unique entries. After that everything is straightforward.

Now the missing functions.

``` commonlisp
(defun +org-notes-buffer-p ()
  "Return non-nil if the currently visited buffer is a note."
  (and buffer-file-name
       (string-equal (file-name-as-directory org-roam-directory)
                     (file-name-directory buffer-file-name))))

(defun +seq-flatten (list-of-lists)
  "Flatten LIST-OF-LISTS."
  (apply #'append list-of-lists))

(defun +org-buffer-prop-set (name value)
  "Set a buffer property called NAME to VALUE."
  (save-excursion
    (widen)
    (goto-char (point-min))
    (if (re-search-forward (concat "^#\\+" name ": \\(.*\\)") (point-max) t)
        (replace-match (concat "#+" name ": " value))
      ;; find the first line that doesn't begin with ':' or '#'
      (let ((found))
        (while (not (or found (eobp)))
          (beginning-of-line)
          (if (or (looking-at "^#")
                  (looking-at "^:"))
              (line-move 1 t)
            (setq found t)))
        (insert "#+" name ": " value "\n")))))
```

That's it. You can find all solution as a gist on [GitHub](https://gist.github.com/d12frosted/4a55f3d072a813159c1d7b31c21bac9a). Have fun!
