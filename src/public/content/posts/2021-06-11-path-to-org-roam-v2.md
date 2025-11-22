<div class="d12-images-block-[75%]">

![](/images/2021-06-11-path-to-org-roam-v2/2022-07-19-22-10-52-org-roam-logo.webp)

![](/images/2021-06-11-path-to-org-roam-v2/2022-07-19-22-11-08-vulpea-logo.webp)

</div>

In my opinion, [org-roam](https://github.com/org-roam/org-roam/) is among the best things that have happened to Emacs during the last 5 years. Forget the Zettelkasten method - `org-roam` is a solution for non-hierarchical note-taking regardless of the methodology you're using. The most important and appealing feature of `org-roam` is its database! Just imagine: in less than a second, I can filter a collection of 10,000+ notes by tags, title, aliases, links, and other attributes. This is what made my [wine tracking](https://github.com/d12frosted/vino) solution usable with an ever-growing collection of tasting notes, and this is what I used to build various tools for presenting non-trivial views around my notes ([example of a public one](https://github.com/d12frosted/environment/blob/master/emacs/README.org#managing-litnotes)).

Now `org-roam` has [released](https://github.com/org-roam/org-roam/releases/tag/v2.0.0) `V2`, the first major redesign affecting both users and authors of libraries extending `org-roam`. Whilst there are breaking changes requiring some effort from users, `V2` is so amazing and so much more stable than `V1` that my advice is to drop the chains of old `org-roam` and embrace all the goodness `V2` brings.

Unfortunately, a major redesign means lots of breaking changes, and in this post I'm going to describe my migration experience, the code I used to migrate my notes, and share some thoughts after migrating a code base comparable to `org-roam` itself. Of course, this touches my precious [vulpea](https://github.com/d12frosted/vulpea) library (the fox image is its icon), so expect some advertisement™.

NB. The migration process means adapting to and overcoming breaking changes. Since `V2` is full of them, this article might look pessimistic or depressing. But don't be fooled - I think that `V2` is amazing. I'm very happy to use it on a daily basis, have been for several months, and I've never looked back. [Jethro](https://github.com/jethrokuan) is doing amazing work! As are all the [contributors](https://github.com/org-roam/org-roam/graphs/contributors) (with extra kudos to [Noboru Ota](https://github.com/nobiot), because Noboru rocks).

## Change Log

- **\[2021-07-24\]:** Updated post to reflect [release of org-roam v2](https://github.com/org-roam/org-roam/releases/tag/v2.0.0).

<!--more-->

# Key changes

`org-roam` `V2` has [many changes](https://github.com/org-roam/org-roam/pull/1401), and some of them are UX-related (e.g. new completing read, revamped roam buffer), whilst others are fundamental in a way that they affect interactive and programmatic usage. So I'm going to focus on these changes. I might miss some very important things that I've never used, and if so, just let me know and I'll update the list.

## Structured note

One of the first things I experimented with in `vulpea` was the definition of a note. Initially, it was a [property list](https://github.com/d12frosted/vulpea/commit/8820c9af2c71c1e995856432c5106aac2774d162), but I quickly started to dislike this approach (even though it was well defined) and switched to a [struct](https://github.com/d12frosted/vulpea/commit/e38a1353c068bf28203ca3ebe93e5a3d6cfd7262). Why? Because a formal definition of data types makes it easier to build on top of. You get completion for fields, you get the byte compiler to warn you about missing fields, etc. So even if, as an author of a library, you're totally fine with not having a formal definition of your data types, they're a game-changer for people using it.

This is why I'm so happy that `org-roam` finally formalised `org-roam-node`. And one of the best parts about this change is that there's no distinction between file-level notes and headings anymore. This is what I did in `vulpea`, and now with `V2` I can remove all those `(if (= level 0) (treat-as-file) (treat-as-heading))` indirections.

The only thing that you should keep in mind, having `org-roam-node` doesn't mean that all fields are populated, you might need to call `org-roam-populate`. As far as I can tell, the reason is that all data is scattered across multiple tables and gathering everything is an expensive operation. This is the main difference between `org-roam-node` and `vulpea-note`, which is always populated when returned from `vulpea` functions (achieved by horrific [SQL expression](https://github.com/d12frosted/vulpea/blob/c606b33e25c0240ca68350163a0327a0bf08d20d/vulpea-db.el#L85)). On the other hand, `vulpea-note` contains less information than `org-roam-node`.

## Mandatory ID

Following note type formalisation, the `ID` property became mandatory for a node to be part of the `org-roam` database. This means that if your heading doesn't have an `ID`, it won't be picked up by `org-roam`.

And I'm happy about this requirement, because IDs are the only way to identify a note (I know, tautology). `vulpea` [required](https://github.com/d12frosted/vulpea/commit/8820c9af2c71c1e995856432c5106aac2774d162#diff-ecbc1aa90e9ff97a00b0b2aab1551bceee0c4d21993146bdcb1af4de31c9cac6R26) them from day one. In my view, a missing `ID` means that the note doesn't exist yet (or at least, the system doesn't know about it yet).

This is important because, again, it reduces any indirection in library code related to note/node identification. Just `ID`, and that's it.

Another reason to have mandatory ids is linking.

## ID Links

Initially, `org-roam` used file links. This means that whenever you linked a note, it used an absolute (or relative???[^1]) file path to said note. But these paths are very sensitive to file system changes, which should be irrelevant in a non-hierarchical note-taking tool. Right? You change the file name - now all links must change. Have fun if for some reason they weren't changed automatically. You moved a directory - now all links must be updated. You synchronise `org-roam-directory` across multiple computers with different paths to said directory (e.g. `/home/boris/org` vs `/Users/boris/notes/org`) - now there are no links between your notes! Task failed successfully, as they say! It's easy to become paranoid with such an approach, because you can't trust a system that misses links.

Now, IDs don't change (quite a philosophical question about identity, but let's skip this one for another day). And Org mode supports `ID` links. Perfect! With `V2`, all links are ID-based, and less maintenance is required when you move something around. Great - users of `org-roam` and its maintainers are both winners.

I'm not sure if this brings much to authors of libraries, but in general it's a very good change that allows removal of most safeguards related to links. If you still believe that path links are somehow superior, please let me know why! I might be missing something here.

## Tags

In my opinion, one of the biggest breaking changes is how things are tagged now. In `V2`, there's no separate tagging system. In `V1`, there are several [sources](https://github.com/org-roam/org-roam/blob/8ad141403065bebd5a72f0ef53cf5ef8f2034419/org-roam.el#L195) of `org-roam` tags - path elements (e.g. sub-directories relative to `org-roam-directory`), properties, and Org mode tags. With `V2`, there are only Org mode tags.

This is quite a complex topic, because with this kind of freedom, it's easy to exploit and over-complicate tag usage in `org-roam`. The key thing to remember is that whilst Org mode tags are great when doing agenda searches, the agenda doesn't work with files but with headings only. And if you were using roam tags for categorisation (so you could filter stuff from the database based on these tags), now you have to deal with [Tag Inheritance](https://orgmode.org/manual/Tag-Inheritance.html#Tag-Inheritance), which isn't a big deal in some cases, but still.

Another thing to keep in mind: a valid tag for `#+roam_tags` isn't necessarily a valid tag for `#+file_tags`. For example, spaces and colons aren't allowed - tags are normal words containing letters, numbers, '\_', and '@' (see `org-tag-re`). So you need to keep that in mind during migration (covered in the attached script).

Since I was abusing the tagging system, I had to go over every such case and find another way to achieve my goal. As an example, I was tagging all my `litnotes` with two types of tags: content type (e.g. `content:book`, `content:article`, etc.) and status (e.g. `status:new`, `status:ongoing`, `status:done`, `status:dropped`). I didn't want to use `filetags` for this purpose, because I would need to disable inheritance for all possible values. So my [solution](https://github.com/d12frosted/environment/blob/master/emacs/README.org#managing-litnotes) is to move this kind of metadata to… a [metadata list](https://github.com/d12frosted/vulpea/tree/c606b33e25c0240ca68350163a0327a0bf08d20d#vulpea-meta).

## Capture

When you're migrating to `org-roam` or when you're automating stuff, having the ability to programmatically create a new note synchronously and get it as a result is priceless. Whilst it was possible with `V1`, it required [a certain hack](https://github.com/d12frosted/vulpea/blob/0f73528e603b1901cbe36eccd536a9113ef0439d/vulpea.el#L167). With `V2`, it's no longer needed because you can [provide](https://github.com/org-roam/org-roam/pull/1523) the `id` of a note during the capture process and then simply query it (just don't forget to [pass](https://github.com/d12frosted/vulpea/blob/c606b33e25c0240ca68350163a0327a0bf08d20d/vulpea.el#L350) the `immediate-finish` property).

And the coolest thing about the new capture process is that `org-roam-capture-templates` became even closer to Org mode capture templates, meaning that you can configure capture in so many fancy ways… I've yet to discover this new wonderful world.

# Notes migration

Migration to `V2` requires changes to your notes:

1.  Make sure that each file contains an ID;
2.  Move `roam_key` to `roam_ref` property;
3.  Move `roam_alias` to `roam_aliases` property;
4.  Move `roam_tags` to `filetags` and
    1.  make sure they don't contain ':' or spaces;
    2.  extract path tags;

This can be achieved by using helpers from [vulpea](https://github.com/d12frosted/vulpea/) and the following script (you must be on `org-roam-v2`):

``` commonlisp
(defun vulpea-migrate-buffer ()
  "Migrate current buffer note to `org-roam' v2."
  ;; Create file level ID if it doesn't exist yet
  (org-with-point-at 1
    (org-id-get-create))

  ;; update title (just to make sure it's lowercase)
  (vulpea-buffer-title-set (vulpea-buffer-prop-get "title"))

  ;; move roam_key into properties drawer roam_ref
  (when-let* ((ref (vulpea-buffer-prop-get "roam_key")))
    (org-set-property "ROAM_REFS" ref)
    (let ((case-fold-search t))
      (org-with-point-at 1
        (while (re-search-forward "^#\\+roam_key:" (point-max) t)
          (beginning-of-line)
          (kill-line 1)))))

  ;; move roam_alias into properties drawer roam_aliases
  (when-let* ((aliases (vulpea-buffer-prop-get-list "roam_alias")))
    (org-set-property "ROAM_ALIASES"
                      (combine-and-quote-strings aliases))
    (let ((case-fold-search t))
      (org-with-point-at 1
        (while (re-search-forward "^#\\+roam_alias:" (point-max) t)
          (beginning-of-line)
          (kill-line 1)))))

  ;; move roam_tags into filetags
  (let* ((roam-tags (vulpea-buffer-prop-get-list "roam_tags"))
         (file-tags (vulpea-buffer-prop-get-list "filetags"))
         (path-tags (seq-filter
                     (lambda (x) (not (string-empty-p x)))
                     (split-string
                      (string-remove-prefix
                       org-roam-directory
                       (file-name-directory (buffer-file-name)))
                      "/")))
         (tags (seq-map
                (lambda (tag)
                  (setq tag (replace-regexp-in-string
                             ;; see `org-tag-re'
                             "[^[:alnum:]_@#%]"
                             "_"        ; use any valid char - _@#%
                             tag))
                  (if (or
                       (string-prefix-p "status" tag 'ignore-case)
                       (string-prefix-p "content" tag 'ignore-case)
                       (string-equal "Project" tag))
                      (setq tag (downcase tag)))
                  tag)
                (seq-uniq (append roam-tags file-tags path-tags)))))
    (when tags
      (apply #'vulpea-buffer-tags-set tags)
      (let ((case-fold-search t))
        (org-with-point-at 1
          (while (re-search-forward "^#\\+roam_tags:" (point-max) t)
            (beginning-of-line)
            (kill-line 1))))))

  (save-buffer))

(defun vulpea-migrate-db ()
  "Migrate all notes."
  (interactive)
  (dolist (f (org-roam--list-all-files))
    (with-current-buffer (find-file f)
      (message "migrating %s" f)
      (vulpea-migrate-buffer)))

  ;; Step 2: Build cache
  (org-roam-db-sync 'force))
```

Simply run `M-x vulpea-migrate-db` and wait until the dirty work is done. Let me know if it doesn't work for you. More discussion around migration can be found on [official guide](https://www.orgroam.com/manual.html#Migrating-from-Org_002droam-v1), [discourse](https://org-roam.discourse.group/t/the-org-roam-v2-great-migration/1505), [GitHub Gist](https://gist.github.com/jethrokuan/02f41028fb4a6f81787dc420fb99b6e4) and [GitHub Wiki](https://github.com/org-roam/org-roam/wiki/Hitchhiker's-Rough-Guide-to-Org-roam-V2).

# Code migration

And this is the trickiest part, because `V2` is taking a revolutionary approach instead of an evolutionary approach - you just have to deal with a huge number of breaking changes. Migrating my notes took around an hour and a half (including patching the migration script, iteratively fixing things, pouring more wine, and fighting with issues caused by misusing `git-gutter`[^2]). Migration of [vulpea](https://github.com/d12frosted/vulpea), [vino](https://github.com/d12frosted/vino), and [personal configs](https://github.com/d12frosted/environment) took more than a week of work. And it wasn't that bad thanks to (1) having `vulpea` and `vino` covered with tests (so I was simply fixing them), (2) using the byte compiler, which caught many changes, and (3) having most of my code based on `vulpea`, so lots of stuff got fixed transitively[^3].

Now, the real question is: what are the major changes code-wise? Different database scheme, formalised `org-roam-node` data type, new capture flow, new utilities (some are gone, but there are many good new ones).

Since `V1` lacked a good abstraction over the database, it was very common to… well, you know, use `org-roam-db-query` directly to execute arbitrary SQL queries[^4], which of course relies on *knowing* the scheme. But this coupling bites really hard with `V2`, because the scheme changed dramatically. So you just have to sit down and work with **every** query you perform. Some changes are trivial, some are not. Unfortunately, there's no pill here. My advice is to minimise the number of direct queries by using functions provided by `org-roam` or extracting them into your own helpers.

With `org-roam-node` and some other data types, everything becomes really nice. There's no need for indirection for file-level or heading-level nodes anymore. You know exactly what fields you have. The only thing you need to keep in mind is that not all fields are populated, so there's `org-roam-populate` that works with various types.

You can check my PRs related to migration to `V2` for inspiration (unfortunately most of them already have new unrelated features):

- [vulpea#92](https://github.com/d12frosted/vulpea/pull/92)
- [vino#100](https://github.com/d12frosted/vino/pull/100)
- [environment#87d23cd](https://github.com/d12frosted/environment/commit/87d23cd71800b8976d1c40927b6a345b8eac40ac), with follow-ups:
  - [environment#57e6fe4](https://github.com/d12frosted/environment/commit/57e6fe4417ff367754d3df31eb144efa76ea1073)
  - [environment#1febc7a](https://github.com/d12frosted/environment/commit/1febc7a5b015d98602cd48cb0143b4424a5e8c03)
  - … and many more

Before you jump into changing your code base, I suggest going over the [Developer's Guide to Org-roam](https://github.com/org-roam/org-roam/blob/ed16ca75d7556b4d831326804543591836b466d5/doc/org-roam.org#developers-guide-to-org-roam).

# What about vulpea?

Some of the core ideas of `vulpea` have been implemented in `org-roam` now (definitely not claiming that I played any role in this - I'm sure Jethro had this in mind for a long time). And I asked myself: is there still a need for `vulpea`? Like, there's `org-roam-node`, why do I need `vulpea-note`? There's a new and wonderful capture process, why do I need `vulpea-create`? Many questions like this. But fundamentally, I think there are only two questions. Is `vulpea` still useful for **me**? And is `vulpea` still useful for **others**?

For me - definitely yes, because it gives me abstractions and utilities to build other stuff on top. The migration process illustrated that I can do all the heavy lifting in `vulpea` and get most of the stuff fixed in many other places. Tests[^5] made this migration really smooth (of course I haven't covered every single thing).

For others - it's for you to decide, and I believe so :) Forget abstractions; `vulpea` provides various utilities for working with Org Mode and Org Roam files that you might want to use (for dealing with buffer properties, e.g. `#+key: value`, and for dealing with description lists). It also provides a custom configurable interface for selecting notes - `vulpea-select` with interactive functions using it (`vulpea-find`, `vulpea-find-backlink`, and `vulpea-insert`). I'll cover them in a separate article. And of course, my favourite functions - `vulpea-db-query` and `vulpea-create`.

So I'm going to continue supporting and developing the `vulpea` library. If you encounter any issues (with code, documentation, etc.) or have a feature request, just don't hesitate to [open an issue](https://github.com/d12frosted/vulpea/issues), [start a discussion](https://github.com/d12frosted/vulpea/discussions), comment on this post, or write me an email.

# Any lessons learned?

The most important lesson for me is that `vulpea` has a huge ROI. The primary goal of `vulpea`[^6] is to be a *tested* library for building solutions for note-taking. And this migration process simply proved that the goal is worth the effort.

Another lesson I'm taking from this situation is that a revolutionary approach is stressful and time-consuming for users (not sure about Jethro - I hope he's all right). Even though in this case it's totally worth all the nerves and doubts. It's stable, polished, and better. It also implies another lesson: a sequel might be even better than the original :)

I'm also annoyed by the fact that I keep abusing the tagging system whenever I encounter it. Whenever it becomes multidimensional, it's a sign to stop and take a step back. Tags shouldn't turn into a total mess[^7].

Of course, the `org-roam` community is fantastic. I don't read all the messages on [org-roam.discourse.group](https://org-roam.discourse.group/), but when I do, I'm simply amazed by all the use cases you folks have, and how helpful you are! Seriously, this is amazing. And it's also credit to [Jethro](https://github.com/jethrokuan/). Without a doubt, `org-roam` is the best thing that's happened in my note-taking life.

# Resources

- [org-roam#1401](https://github.com/org-roam/org-roam/pull/1401)
- [Org-roam major redesign](https://org-roam.discourse.group/t/org-roam-major-redesign/1198)
- [Developer's Guide to Org-roam](https://github.com/org-roam/org-roam/blob/ed16ca75d7556b4d831326804543591836b466d5/doc/org-roam.org#developers-guide-to-org-roam)
- [Hitchhiker's Rough Guide to Org roam V2](https://github.com/org-roam/org-roam/wiki/Hitchhiker's-Rough-Guide-to-Org-roam-V2)
- [The Org-roam v2 Great Migration](https://org-roam.discourse.group/t/the-org-roam-v2-great-migration/1505)

[^1]: I don't even remember, because I quickly switched to ID links once they landed to `org-roam`.

[^2]: Silly, but I've adopted an approach by Doom Emacs where the gutter is updated asynchronously. During migration, I was opening and modifying too many org files (hey, more than 10,000) which exhausted Emacs. I spent too much time trying to figure out why it was happening.

[^3]: Whatever was using `org-roam` internals was migrated to `vulpea`.

[^4]: Again, tautology! I love them.

[^5]: Just for the reference, I'm not a TDD advocate. I usually write tests after features and mostly before bug fixes.

[^6]: To be precise, `vulpea` (and sometimes `KitsuneBook`) is just a name I'm using for note-taking utilities. At some point, it was a Haskell command-line application… Now it's an Emacs Lisp library leveraging Org Mode and Org Roam! It definitely has a more active life than me.

[^7]: Maybe a partial mess, but definitely not total.
