[org-mode](https://orgmode.org) is a simple outliner for note taking and list management. It [doesn't impose](/posts/2016-12-20-Being-an-org-mode-addict) any complex features on users. This is something users excel at by themselves. My suggestion is to continue this tradition and discover task management capabilities in [org-roam](https://github.com/org-roam/org-roam), an [org-mode](https://orgmode.org) extension for non-hierarchical notes management.

![](/images/2020-06-23-task-management-with-roam-vol1/2022-07-19-21-03-45-org-roam-task-management-vol1-3.webp)

**Change Log:**

- `[2021-05-10 Mon]`: Update post to reflect changes in [org-roam v2](https://github.com/org-roam/org-roam/pull/1401). Previous version of this article is available on [GitHub](https://github.com/d12frosted/d12frosted.io/blob/c16870cab6ebbaafdf73c7c3589abbd27c20ac52/posts/2020-06-23-task-management-with-roam-vol1.org).

<!--more-->

Since org-mode gives a lot of freedom, everyone implements tasks management system differently. In the nutshell, most of the implementations I've seen consist of the following elements.

- Task - the simplest actionable item with some `TODO` state.
- Project - a `TODO` item with outcome requiring multiple steps to achieve it.
- Meta project - long lasting project without a final outcome. It consists of projects or tasks. Think of it as of an area or a category. For example, personal blog is a meta project, as it doesn't have any terminating outcome. It does have projects with specific outcome (like setting up your blog or writing a post) or simple tasks (like renewing SSL certificate), but the blog itself doesn't have an outcome.
- Resource - a person, a tool, a location or whatever possibly related to a task. In plain org-mode usually implemented as a tag.

Take a look at the following example of `org-mode` file with all these elements.

``` org
#+title: Work

* emacs-plus
:PROPERTIES:
:CATEGORY:               emacs-plus
:END:

Emacs Plus is [[https://www.gnu.org/software/emacs/emacs.html][→ GNU Emacs]]
formulae for macOS [[https://brew.sh][→ Homebrew]] package manager. It offers a
wide rage of extra functionality over regular
[[https://formulae.brew.sh/formula/emacs#default][→ Emacs]] package. Emacs Plus
intent is to give the most of 'plus' stuff by default, leaving only
controversial options as opt-in.

** TODO Restore icons selection for =emacs-plus@26=          :@FrodoBaggins:
Frodo Baggins volunteered to help with this.

** TODO [0/3] Automate bottle producing                            :PROJECT:
*** TODO Create a project on Bintray
*** TODO Investigate if =brew test-bot= can be used for =emacs-plus=
*** TODO Write a CI job that creates and uploads bottles

* Blog
:PROPERTIES:
:CATEGORY:               blog
:END:

https://d12frosted.io

** TODO Write a post about org-roam and agenda
** TODO [0/2] Get rid of embedded HTML for images in posts         :PROJECT:
*** TODO Write a custom handler of image links
This should transform link to images into proper HTML.

#+begin_example
  <div class="post-image post-image">
  <img src="/images/some-image.png" />
  </div>
#+end_example

*** TODO Replace all image exports by regular org links
```

<img src="/images/2020-06-23-task-management-with-roam-vol1/2022-07-19-21-04-20-org-roam-task-management-vol1-2.webp" class="img-half img-float-left" />

<img src="/images/2020-06-23-task-management-with-roam-vol1/2022-07-19-21-04-20-org-roam-task-management-vol1-1.webp" class="img-half img-float-right" />

As you can see, it has 2 meta projects: `emacs-plus` and personal blog, each containing a task and a project. Meta projects have their own category property (set by hand or using `org-set-property` which is bound to `C-c C-x p` by default), so in agenda view one can quickly distinguish (and filter) tasks from different meta projects (for example, using `org-agenda-filter-by-category` which is bound to `<` by default).

![](/images/2020-06-23-task-management-with-roam-vol1/2022-07-19_21-05-00_org-agenda-filter-by-category.gif)

Formally (and technically) projects might be defined as a task having sub-tasks, but in this rare case I vote for simplicity - project is a task tagged as `PROJECT`.

When a task relates to a person or some location, I tag it respectively, just use `@` symbol as a prefix to distinguish from utilitarian tags. Agenda helps to quickly find all tasks with specific tag. Use `M-x org-agenda m` or `M-x org-agenda M` (to list only TODO items).

![](/images/2020-06-23-task-management-with-roam-vol1/2022-07-19_21-05-13_org-agenda-filter-by-tags.gif)

With `org-roam` it makes sense to create a separate file for each meta project. In the endless debate 'many small' vs 'few big' org files, `org-roam` favours the former approach (though even this is debatable).

``` org
#+title: emacs-plus

Emacs Plus is [[https://www.gnu.org/software/emacs/emacs.html][→ GNU Emacs]] formulae for macOS [[https://brew.sh][→ Homebrew]] package manager. It
offers a wide rage of extra functionality over regular [[https://formulae.brew.sh/formula/emacs#default][→ Emacs]] package. Emacs
Plus intent is to give the most of 'plus' stuff by default, leaving only
controversial options as opt-in.

* Tasks
** TODO Restore icons selection for =emacs-plus@26=          :@FrodoBaggins:
Frodo Baggins volunteered to help with this.

** TODO [0/3] Automate bottle producing                            :PROJECT:
*** TODO Create a project on Bintray
*** TODO Investigate if =brew test-bot= can be used for =emacs-plus=
*** TODO Write a CI job that creates and uploads bottles
```

``` org
#+TITLE: Blog

https://d12frosted.io

* Tasks
** TODO Write a post about org-roam and agenda
** TODO [0/2] Get rid of embedded HTML for images in posts         :PROJECT:
*** TODO Write a custom handler of image links
This should transform link to images into proper HTML.

#+begin_example
  <div class="post-image post-image">
  <img src="/images/some-image.png" />
  </div>
#+end_example

*** TODO Replace all image exports by regular org links
```

<img src="/images/2020-06-23-task-management-with-roam-vol1/2022-07-19-21-05-28-org-roam-task-management-vol1-4.webp" class="img-half img-float-left" />

<img src="/images/2020-06-23-task-management-with-roam-vol1/2022-07-19-21-05-28-org-roam-task-management-vol1-3.webp" class="img-half img-float-right" />

![](/images/2020-06-23-task-management-with-roam-vol1/2022-07-19-21-05-55-org-roam-task-management-vol1-5.webp)

So far the transition was simple. However as you can see, in agenda buffer the inferred category contains garbage - note id.

In the [next article](/posts/2020-06-24-task-management-with-roam-vol2) we are going to explore the means to fix the agenda buffer from the unwanted visual garbage. Stay tuned!

# Task Management with org-roam Series

1.  [Path to Roam](/posts/2020-06-23-task-management-with-roam-vol1)
2.  [Categories](/posts/2020-06-24-task-management-with-roam-vol2)
3.  [FILETAGS](/posts/2020-06-25-task-management-with-roam-vol3)
4.  [Automatic tagging](/posts/2020-07-07-task-management-with-roam-vol4)
5.  [Dynamic and fast agenda](/posts/2021-01-16-task-management-with-roam-vol5)
6.  [Select a person and view related tasks](/posts/2021-01-24-task-management-with-roam-vol6)
7.  [Capture](/posts/2021-05-21-task-management-with-roam-vol7)

# References

- `org-roam` documentation on [GitHub](https://github.com/org-roam/org-roam).
- `org-mode` documentation on the [official site](https://orgmode.org).
- Org Mode - Organize Your Life In Plain Text! by Bernt Hansen.
