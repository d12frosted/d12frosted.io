In one of the previous articles ([Vol. 3](/posts/2020-06-25-task-management-with-roam-vol3) to be precise) we talked about automatic setup of `filetags`, so each of the task in note related to a person is automatically tagged thanks to [tag inheritance](https://orgmode.org/manual/Tag-Inheritance.html). Then, in [Vol. 4](/posts/2020-07-07-task-management-with-roam-vol4) we talked about automatic tagging of tasks whenever a person is mentioned either in the title or the body of some task. This all makes `org-agenda` [matching capabilities](https://orgmode.org/manual/Matching-tags-and-properties.html#Matching-tags-and-properties) really useful for when we want to see the list of all tasks related to specific person.

In this article, we are going to write a small utility function that asks user to select a person and then presents and `org-agenda` buffer with tasks related to selected person.

Believe me, intro is longer than the content!

![](/content/2021-01-24-task-management-with-roam-vol6/2022-07-19_21-16-52_org-notes-person-agenda.mp4)

**Change Log:**

- `[2021-03-02 Tue]`: Update naming convention to match [personal configurations](https://github.com/d12frosted/environment/tree/master/emacs).
- `[2021-05-10 Mon]`: Update post to reflect changes in [org-roam v2](https://github.com/org-roam/org-roam/pull/1401). Previous version of this article is available on [GitHub](https://github.com/d12frosted/d12frosted.io/blob/c16870cab6ebbaafdf73c7c3589abbd27c20ac52/posts/2021-01-24-task-management-with-roam-vol6.org).
- `[2022-07-11 Mon]`: Adapt code to changes in Org mode and new functions in `vulpea`. Combination of `vulpea-select-from` and `vulpea-db-query-by-tags-some` works faster than generic `vulpea-select`.

``` related_posts
```

<!--more-->

Long story short, here is the function you can bind or call by name using `M-x`:

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

Now some explanations.

1.  This code uses [vulpea](https://github.com/d12frosted/vulpea) library[^1] to select a person. You can achieve the same result without `vulpea`, of course, but it saves some effort. `vulpea-select-from` asks the use to select a note from a list of notes, returned by `vulpea-db-query-by-tags-some`. In this case, we simply present only people notes. The same result can be achieved by using `vulpea-select` that takes a predicate, but `vulpea-db-query-by-tags-some` works faster. See [Performance](https://github.com/d12frosted/vulpea#orgb0b2734) section of `vulpea` documentation.
2.  Once we have a selected `vulpea-note`, we can get all titles on that file (e.g. main title and aliases). This is important for alias users. For example, in some notes I want use Mr. Frodo instead of Frodo Baggins, but I want to see tasks tagged as `@Mr.Forod` and `@FrodoBaggins` at the same time. It's the same person after all (don't ask me about Gollum, better use `M-x doctor`)!
3.  Now we simply convert those names into tags using `vulpea--title-to-tag` from [Vol. 4](/posts/2020-07-07-task-management-with-roam-vol4).
4.  Then we join the tags using `|` separator into single query string.
5.  The last step is to execute `org-agenda` with `M` argument (match for tags, but list only `TODO` items). In order to pass a query to relevant agenda function, we use `org-agenda-overriding-arguments`. Not sure if it's documented anywhere, but you can read the sources of `org-agenda` to figure out how to use it. `dlet` here is used for dynamic binding. If you are not using lexical scope, you can use regular `let` here.

That's it! Now see it in action, again.

![](/content/2021-01-24-task-management-with-roam-vol6/2022-07-19_21-16-52_org-notes-person-agenda.mp4)

# References

- [Org Element API](https://orgmode.org/worg/dev/org-element-api.html)
- [skeeto/emacsql](https://github.com/skeeto/emacsql)

[^1]: Yikes, I advertise my own libraries on this blog!
