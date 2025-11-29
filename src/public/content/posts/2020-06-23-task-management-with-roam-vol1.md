[org-mode](https://orgmode.org) is a simple outliner for note-taking and list management. It [doesn't impose](/posts/2016-12-20-Being-an-org-mode-addict) any complex features on users - this is something users excel at themselves. My suggestion is to continue this tradition and discover task management capabilities in [org-roam](https://github.com/org-roam/org-roam), an [org-mode](https://orgmode.org) extension for non-hierarchical note management.

<img src="/images/2020-06-23-task-management-with-roam-vol1/2022-07-19-21-03-45-org-roam-task-management-vol1-3.webp" class="d12-image-3/4" />

**Change Log**

- **\[2025-11\]:** This series was written for vulpea v1 with org-roam. See [Vulpea v2: breaking up with org-roam](/posts/2025-11-28-vulpea-v2-breaking-up-with-org-roam) for context on vulpea v2, which no longer depends on org-roam. Updated guides are coming.
- **\[2021-05-10\]:** Updated post to reflect changes in [org-roam v2](https://github.com/org-roam/org-roam/pull/1401). Previous version of this article is available on [GitHub](https://github.com/d12frosted/d12frosted.io/blob/c16870cab6ebbaafdf73c7c3589abbd27c20ac52/posts/2020-06-23-task-management-with-roam-vol1.org).

``` related_posts
```

<!--more-->

Since org-mode gives a lot of freedom, everyone implements their task management system differently. In a nutshell, most implementations I've seen consist of the following elements:

- Task - the simplest actionable item with some `TODO` state.
- Project - a `TODO` item with an outcome requiring multiple steps to achieve.
- Meta project - a long-lasting project without a final outcome. It consists of projects or tasks. Think of it as an area or category. For example, a personal blog is a meta project, as it doesn't have a terminating outcome. It does have projects with specific outcomes (like setting up your blog or writing a post) or simple tasks (like renewing an SSL certificate), but the blog itself doesn't have an outcome.
- Resource - a person, tool, location, or anything possibly related to a task. In plain org-mode, this is usually implemented as a tag.

Here's an example of an `org-mode` file with all these elements:

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

<div class="d12-images-block-[100%]">

![](/images/2020-06-23-task-management-with-roam-vol1/2022-07-19-21-04-20-org-roam-task-management-vol1-2.webp)

![](/images/2020-06-23-task-management-with-roam-vol1/2022-07-19-21-04-20-org-roam-task-management-vol1-1.webp)

</div>

As you can see, it has 2 meta projects: `emacs-plus` and a personal blog, each containing a task and a project. Meta projects have their own category property (set by hand or using `org-set-property`, which is bound to `C-c C-x p` by default), so in the agenda view you can quickly distinguish (and filter) tasks from different meta projects (for example, using `org-agenda-filter-by-category`, which is bound to `<` by default).

<img src="/images/2020-06-23-task-management-with-roam-vol1/2022-07-19_21-05-00_org-agenda-filter-by-category.gif" class="d12-image-3/4" />

Formally (and technically), projects might be defined as a task having sub-tasks, but in this rare case I vote for simplicity - a project is a task tagged as `PROJECT`.

When a task relates to a person or location, I tag it accordingly, using the `@` symbol as a prefix to distinguish it from utilitarian tags. The agenda helps you quickly find all tasks with a specific tag. Use `M-x org-agenda m` or `M-x org-agenda M` (to list only TODO items).

<img src="/images/2020-06-23-task-management-with-roam-vol1/2022-07-19_21-05-13_org-agenda-filter-by-tags.gif" class="d12-image-3/4" />

With `org-roam`, it makes sense to create a separate file for each meta project. In the endless debate of 'many small' vs 'few big' org files, `org-roam` favours the former approach (though even this is debatable).

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

<div class="d12-images-block-[100%]">

![](/images/2020-06-23-task-management-with-roam-vol1/2022-07-19-21-05-28-org-roam-task-management-vol1-4.webp)

![](/images/2020-06-23-task-management-with-roam-vol1/2022-07-19-21-05-28-org-roam-task-management-vol1-3.webp)

</div>

<img src="/images/2020-06-23-task-management-with-roam-vol1/2022-07-19-21-05-55-org-roam-task-management-vol1-5.webp" class="d12-image-1/2" />

So far, the transition was simple. However, as you can see, in the agenda buffer the inferred category contains garbage - the note ID.

In the [next article](/posts/2020-06-24-task-management-with-roam-vol2), we're going to explore ways to fix the agenda buffer and remove this unwanted visual clutter. Stay tuned!

# References

- `org-roam` documentation on [GitHub](https://github.com/org-roam/org-roam).
- `org-mode` documentation on the [official site](https://orgmode.org).
- Org Mode - Organize Your Life In Plain Text! by Bernt Hansen.
