In previous articles - [Vol. 3](/posts/2020-06-25-task-management-with-roam-vol3) to be precise - we discussed automatic setup of `filetags`, so each task in a note related to a person is automatically tagged thanks to [tag inheritance](https://orgmode.org/manual/Tag-Inheritance.html). Then, in [Vol. 4](/posts/2020-07-07-task-management-with-roam-vol4), we covered automatic tagging of tasks whenever a person is mentioned in either the title or the body of a task. This makes `org-agenda`'s [matching capabilities](https://orgmode.org/manual/Matching-tags-and-properties.html#Matching-tags-and-properties) particularly useful when we want to see all tasks related to a specific person.

In this article, we're going to write a small utility function that asks the user to select a person and then presents an `org-agenda` buffer with tasks related to the selected person.

The introduction is actually longer than the implementation!

![](/content/2021-01-24-task-management-with-roam-vol6/2022-07-19-21-16-52-org-notes-person-agenda.mp4)

## Change Log

- **\[2025-11\]:** This series was written for vulpea v1 with org-roam. See [Vulpea v2: breaking up with org-roam](/posts/2025-11-28-vulpea-v2-breaking-up-with-org-roam) for context on vulpea v2, which no longer depends on org-roam. Updated guides are coming.
- **\[2021-03-02\]:** Updated naming convention to match [personal configurations](https://github.com/d12frosted/environment/tree/master/emacs).
- **\[2021-05-10\]:** Updated post to reflect changes in [org-roam v2](https://github.com/org-roam/org-roam/pull/1401). Previous version of this article is available on [GitHub](https://github.com/d12frosted/d12frosted.io/blob/c16870cab6ebbaafdf73c7c3589abbd27c20ac52/posts/2021-01-24-task-management-with-roam-vol6.org).
- **\[2022-07-11\]:** Adapted code to changes in Org mode and new functions in `vulpea`. The combination of `vulpea-select-from` and `vulpea-db-query-by-tags-some` works faster than generic `vulpea-select`.

``` related_posts
```

<!--more-->

Here's the function you can bind or call by name using `M-x`:

``` commonlisp
(defun vulpea-agenda-person ()
  "Show main `org-agenda' view."
  (interactive)
  (let* ((person (vulpea-select-from
                  "Person"
                  (vulpea-db-query-by-tags-some '("people"))))
         (node (org-roam-node-from-id (vulpea-note-id person)))
         (names (cons (org-roam-node-title node)
                      (org-roam-node-aliases node)))
         (tags (seq-map #'vulpea--title-to-tag names))
         (query (string-join tags "|")))
    (let ((org-agenda-overriding-arguments (list t query)))
      (org-agenda nil "M"))))
```

Here are some explanations:

1.  This code uses the [vulpea](https://github.com/d12frosted/vulpea) library[^1] to select a person. You can achieve the same result without `vulpea`, of course, but this saves some effort. `vulpea-select-from` asks the user to select a note from a list of notes returned by `vulpea-db-query-by-tags-some`. In this case, we simply present only people notes. The same result can be achieved by using `vulpea-select`, which takes a predicate, but `vulpea-db-query-by-tags-some` works faster. See the [Performance](https://github.com/d12frosted/vulpea#orgb0b2734) section of `vulpea` documentation.
2.  Once we have a selected `vulpea-note`, we can get all titles for that file (e.g. main title and aliases). This is important for alias users. For example, in some notes I want to use Mr. Frodo instead of Frodo Baggins, but I want to see tasks tagged as `@Mr.Frodo` and `@FrodoBaggins` at the same time. It's the same person after all (don't ask me about Gollum - better use `M-x doctor`)!
3.  We then convert those names into tags using `vulpea--title-to-tag` from [Vol. 4](/posts/2020-07-07-task-management-with-roam-vol4).
4.  Next, we join the tags using the `|` separator into a single query string.
5.  The final step is to execute `org-agenda` with the `M` argument (match for tags, but list only `TODO` items). To pass a query to the relevant agenda function, we use `org-agenda-overriding-arguments`. I'm not sure if it's documented anywhere, but you can read the sources of `org-agenda` to work out how to use it. `dlet` is used here for dynamic binding. If you're not using lexical scope, you can use regular `let` instead.

That's it! Now see it in action again.

![](/content/2021-01-24-task-management-with-roam-vol6/2022-07-19-21-16-52-org-notes-person-agenda.mp4)

# References

- [Org Element API](https://orgmode.org/worg/dev/org-element-api.html)
- [skeeto/emacsql](https://github.com/skeeto/emacsql)

[^1]: Yikes, I advertise my own libraries on this blog!
