In the [previous article](/posts/2020-06-23-task-management-with-roam-vol1), we set the groundwork for moving tasks to [org-roam](https://github.com/org-roam/org-roam) and encountered an issue with visual clutter in the agenda buffer - namely, the `org-roam` file ID appearing as part of the category. In this article, we're going to explore ways to overcome this issue.

<div class="d12-images-block-[100%]">

![](/images/2020-06-24-task-management-with-roam-vol2/2022-07-19-21-08-14-org-roam-task-management-vol2-1.webp)

![](/images/2020-06-24-task-management-with-roam-vol2/2022-07-19-21-08-14-org-roam-task-management-vol2-2.webp)

</div>

**Change Log**

- **\[2025-11\]:** This series was written for vulpea v1 with org-roam. See [Vulpea v2: breaking up with org-roam](/posts/2025-11-28-vulpea-v2-breaking-up-with-org-roam) for context on vulpea v2, which no longer depends on org-roam. Updated guides are coming.
- **\[2021-03-02\]:** Updated category extraction function to use `TITLE` of the note and enforce length limit. Kudos to [Tim Ruffing](https://github.com/real-or-random/) for the idea.
- **\[2021-03-02\]:** Updated naming convention to match [personal configurations](https://github.com/d12frosted/environment/tree/master/emacs).
- **\[2021-05-10\]:** Updated post to reflect changes in [org-roam v2](https://github.com/org-roam/org-roam/pull/1401). Previous version of this article is available on [GitHub](https://github.com/d12frosted/d12frosted.io/blob/c16870cab6ebbaafdf73c7c3589abbd27c20ac52/posts/2020-06-24-task-management-with-roam-vol2.org).

``` related_posts
```

<!--more-->

One of the simplest solutions is to mimic the approach used for headlines by setting the `CATEGORY` property at the file level (manually or using `org-set-property`).

``` org
:PROPERTIES:
:CATEGORY:               emacs-plus
:END:
#+title: emacs-plus

...
```

``` org
:PROPERTIES:
:CATEGORY:               blog
:END:
#+title: Blog

...
```

<img src="/images/2020-06-24-task-management-with-roam-vol2/2022-07-19-21-08-37-org-roam-task-management-vol2-2.webp" class="d12-image-3/4" />

While this works, it is a manual labor. And in most cases we want to use `TITLE` as `CATEGORY`, at least for agenda buffer. Fortunately, we can help agenda to properly parse the category by modifying the value of `org-agenda-prefix-format`, which allows to specify how to render each line in the different agenda buffers (e.g. regular agenda, in the list of todo tasks etc). We are looking for the capability to evaluate arbitrary lisp expressions. The default value of this variable is

``` commonlisp
((agenda . " %i %-12:c%?-12t% s")
 (todo . " %i %-12:c")
 (tags . " %i %-12:c")
 (search . " %i %-12:c"))
```

The interesting part is `%-12:c` which means:

- Give the category (because of `c`) a 12 chars wide field, padded with whitespace on the right (because of `-`).
- Append a colon if there is a category (because of `:`).
- Finally, append the category of the item, or as given by the `CATEGORY` property, or derived from the file name.

Instead of `c` we can use any expression.

``` commonlisp
(setq org-agenda-prefix-format
      '((agenda . " %i %-12(vulpea-agenda-category)%?-12t% s")
        (todo . " %i %-12(vulpea-agenda-category) ")
        (tags . " %i %-12(vulpea-agenda-category) ")
        (search . " %i %-12(vulpea-agenda-category) ")))

(defun vulpea-agenda-category ()
  "Get category of item at point for agenda.

Category is defined by one of the following items:

- CATEGORY property
- TITLE keyword
- TITLE property
- filename without directory and extension

Usage example:

  (setq org-agenda-prefix-format
        '((agenda . \" %(vulpea-agenda-category) %?-12t %12s\")))

Refer to `org-agenda-prefix-format' for more information."
  (let* ((file-name (when buffer-file-name
                      (file-name-sans-extension
                       (file-name-nondirectory buffer-file-name))))
         (title (vulpea-buffer-prop-get "title"))
         (category (org-get-category)))
    (or (if (and
             title
             (string-equal category file-name))
            title
          category)
        "")))
```

In order to extract title, I am using `vulpea-buffer-prop-get` from [vulpea](https://github.com/d12frosted/vulpea) library. It's [defined](https://github.com/d12frosted/vulpea/blob/feature/org-roam-v2/vulpea.el#L239) as:

``` commonlisp
(defun vulpea-buffer-prop-get (name)
  "Get a buffer property called NAME as a string."
  (org-with-point-at 1
    (when (re-search-forward (concat "^#\\+" name ": \\(.*\\)")
                             (point-max) t)
      (buffer-substring-no-properties
       (match-beginning 1)
       (match-end 1)))))
```

<img src="/images/2020-06-24-task-management-with-roam-vol2/2022-07-19-21-08-55-org-roam-task-management-vol2-3.webp" class="d12-image-3/4" />

Now if we remove the manually set `CATEGORY` property from both files we will get the same result with nicely parsed categories. Please note that these two approaches can be mixed. For example, if you wish to override the category, just set this property explicitly and call it a day.

Additionally, it's easy to extend this function to truncate overly long categories (in the screenshot above, `Some project with ridiculously long title` and `Frodo Baggins` are examples of long categories). We will use [s.el](https://github.com/magnars/s.el) library to achieve this.

``` commonlisp
(setq org-agenda-prefix-format
      '((agenda . " %i %(vulpea-agenda-category 12)%?-12t% s")
        (todo . " %i %(vulpea-agenda-category 12) ")
        (tags . " %i %(vulpea-agenda-category 12) ")
        (search . " %i %(vulpea-agenda-category 12) ")))

(defun vulpea-agenda-category (&optional len)
  "Get category of item at point for agenda.

Category is defined by one of the following items:

- CATEGORY property
- TITLE keyword
- TITLE property
- filename without directory and extension

When LEN is a number, resulting string is padded right with
spaces and then truncated with ... on the right if result is
longer than LEN.

Usage example:

  (setq org-agenda-prefix-format
        '((agenda . \" %(vulpea-agenda-category) %?-12t %12s\")))

Refer to `org-agenda-prefix-format' for more information."
  (let* ((file-name (when buffer-file-name
                      (file-name-sans-extension
                       (file-name-nondirectory buffer-file-name))))
         (title (vulpea-buffer-prop-get "title"))
         (category (org-get-category))
         (result
          (or (if (and
                   title
                   (string-equal category file-name))
                  title
                category)
              "")))
    (if (numberp len)
        (s-truncate len (s-pad-right len " " result))
      result)))
```

<img src="/images/2020-06-24-task-management-with-roam-vol2/2022-07-19-21-09-09-org-roam-task-management-vol2-4.webp" class="d12-image-3/4" />

Now the agenda is clean.

In the [next article](/posts/2020-06-25-task-management-with-roam-vol3) we are going to talk about tagging tasks related to a person. Stay tuned and keep roaming!

# References

- `org-roam` documentation on [GitHub](https://github.com/org-roam/org-roam).
- `org-mode` documentation on the [official site](https://orgmode.org).
- personal configurations on [GitHub](https://github.com/d12frosted/environment/blob/master/emacs/lisp/%2Borg-notes.el).
