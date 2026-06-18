I've tried GTD more than once. Taken seriously, it slowly drains the joy out of my life - the endless inbox processing, the contexts, the weekly review that turns into an obligation I start avoiding. So I stopped. Over the next few years I drifted into my own way of keeping things - tasks and notes sorted by how live they were, not by subject - without ever giving it a name.

Then I came across [PARA](https://fortelabs.com/blog/para/), Tiago Forte's method: Projects, Areas, Resources, Archives, organized by how actionable something is rather than by topic. It clicked immediately, because it was, more or less, the system I already had. That is less impressive than it sounds - the shape is fairly natural once you sort by action - but a name and a clean four-way split sharpened something I had only felt my way into.

I didn't adopt it wholesale, though. I don't take a method off the shelf and reshape my life to fit it; I keep the parts that match how I already work and quietly leave the rest. So what I run is PARA with a few caveats of my own - and the biggest one is the twist that makes it pleasant to live with: my notes don't live in PARA folders. They live wherever they live, and a note's bucket is something [vulpea](https://github.com/d12frosted/vulpea) derives and queries on demand. Most write-ups present PARA as four folders you file things into; that framing never worked for me.

This post is about the idea, not the implementation - I'm pulling the implementation into a package (more on that at the end). If you read the old [task management with org-roam](/posts/2020-06-23-task-management-with-roam-vol1) series, this is where that approach quietly landed.

<!--more-->

# The four buckets, recognized not filed

A quick recap, in the terms I actually use:

- **Projects** - things with a finish line and active work in them, like shipping a post or fixing a regression. These don't get their own notes in my setup; they live as headings inside the area they belong to (more on that below).
- **Areas** - ongoing responsibilities with no end, only a standard to keep up. An area is never "done"; it just gets maintained.
- **Resources** - reference I might want later: people, places, topics. Most of my notes live here, and I mostly reach them by searching or following links rather than by tending them.
- **Archives** - anything from the first three that has gone quiet. Real archives, in my case - org's own archiving plus an `.archive/` directory - not deletion. Out of the active view, still in the database, still searchable.

Here is the part that matters: which bucket a note is in is *not* the same as where the file sits on disk. I do keep folders - `area/`, `people/`, `journal/` - but they are about domain and storage, not PARA. A note's bucket is carried by tags and metadata, and vulpea reads it back out with a query. Storage is one axis; actionability is another; I refuse to make one stand in for the other.

# Areas are the pillars

This is the part of PARA that actually clicked for me: areas are the pillars of my life. Not folders, not topics - the handful of things I am responsible for keeping up. The blog. Finances. A garden. At work I wear several hats, and because they barely overlap, each is its own area rather than one lumped "work". Every one of them is a single note.

They are not uniform, and that is the point. Some areas are mostly notes - things I am thinking about. Some are mostly actionable - a running list of what needs doing. Some are basically a stack of projects. It doesn't matter: whatever that part of my life needs lives under its one area file, in whatever shape suits it.

Once the areas are right, everything else falls out almost mechanically. A project is just work under an area. A task is a line under an area. Reference attaches to the area it serves. My capture templates even start by asking which area I am in, so new things land in the right place by construction rather than by later filing.

Which means the real work was never filing. It is figuring out what my areas even are - and keeping that list honest as life shifts. That is the one decision that carries weight, and, conveniently, the only one I have to actually think about.

# Classification you don't maintain by hand

Two things make this pleasant rather than a filing chore, and both come down to not moving things by hand.

First, projects don't get their own files. A project is a `TODO` heading that lives inside its area's note. Capturing one asks which area it belongs to and drops the heading straight in, with a category like `blog > ship the v2.1 post` so the agenda reads cleanly. The project lives where the area lives - I never refile it into place, and when it is done it gets archived where it sits rather than moved across folders.

Second, I don't maintain the list of files the agenda even looks at. Walking thousands of notes to find the few with open tasks is hopeless, so I let it be derived: on save, vulpea checks whether the file holds any live `TODO` and tags the file `agenda` (or drops the tag when the last task is done).

``` commonlisp
;; does this file still hold open work?
(defun vulpea-buffer-open-work-p ()
  (org-element-map (org-element-parse-buffer 'headline) 'headline
    (lambda (h) (eq (org-element-property :todo-type h) 'todo))
    nil 'first-match))
```

The agenda is then just a query: give me the files currently tagged `agenda`. No global `org-agenda-files` to maintain, no traversal - vulpea answers from its database in a few milliseconds, and a file slips in and out of the agenda on its own as work appears and finishes. I wrote up the full version years ago in [Vol. 5 of the org-roam series](/posts/2021-01-16-task-management-with-roam-vol5); the mechanics still hold, only the library underneath changed.

The point isn't the snippet. It's that a file earns its place by what's in it, and the system keeps itself sorted. That is the difference between PARA-as-a-chore and PARA you forget is running.

# What an area actually looks like

Enough idea. Here is the blog area, trimmed to the bone:

``` org
:PROPERTIES:
:ID:       8c1d0e2a-7b3f-4a6c-9d11-2e5f4a8b0c34
:END:
#+title: Blog
#+filetags: :agenda:area:

- short name :: blog

* Notes

** Post ideas
- a follow-up on the mobile reader
- the economics of self-hosting

* Tasks

** TODO Ship the v2.1 post                                        :project:
:PROPERTIES:
:CATEGORY:               blog > ship the v2.1 post
:END:

*** TODO Draft the PARA write-up
*** TODO Trim it where it drifts into theory

** TODO Fix the broken RSS enclosure

* Archive                                                         :ARCHIVE:

** DONE Ship the v2.0 post                                        :project:
:PROPERTIES:
:CATEGORY:               blog > ship the v2.0 post
:END:
```

Read it top to bottom and almost none of it is plumbing. `area` is the bucket, set once and never touched again. `agenda` I do not set at all: the save hook from earlier added it because the file holds a live `TODO`, and it falls off when the last one closes. The file then holds whatever this area needs: `Notes` for reference, `Tasks` for the actionable side, and an area is free to be all of one or all of the other. The projects are ordinary `TODO` headings under `Tasks`, each tagged `project` and carrying a `CATEGORY` like `blog > ship the v2.1 post`, which is what makes the agenda line read as `blog > ship the v2.1 post` rather than a bare `Ship the v2.1 post`. A task that belongs to no project is just a heading. Finished work drops into `Archive` and out of the live view. One file, readable at a glance, and every bucket in it is a tag or a heading, never a folder it had to be filed into.

# Lifecycle: how much to write, and when to archive

If choosing areas is the first hard thing, lifecycle is the second - and the one I am least settled on. Two questions, really: how much to write down, and when to let it go.

The writing question is a discipline problem. An area invites you to pour everything into it, and an area that records every passing thought becomes unreadable. I try to keep one to what is live plus a light history - enough to know what happened, not a transcript of it.

The letting-go question is archiving. Because projects live inside the area file, a long-lived area fills up, so finished work moves into the area's own `Archive` section and history log - out of the active part I actually read, though it stays in the file and in the database, still searchable. Folding hides the rest, so the *live* surface of an area stays small even when its history doesn't.

I won't pretend this is solved - a years-old area is still a big file, and I go back and forth on whether some should split. But "write less than you want to, archive what's done, keep the live part short" has carried me a long way. It is the same instinct as everything else here: the thing you act on should be small and in front of you; everything else can recede.

# Why a query database changes PARA

Step back and it is the same idea showing up again and again: don't let location encode meaning.

PARA's textbook weakness is that it is a filing system, and filing fights you the moment priorities move - which is constantly. A resource becomes a project becomes an archive; under folders that is a treadmill of moving files and repairing links. The usual advice is "just maintain it", which is the advice that has killed every system I have owned.

When the bucket is metadata over a stable note graph, none of that happens. Notes don't move. Links never break. Reclassifying is a tag flip, often an automatic one. PARA stops being a place you put things and becomes a view you compute - active projects, areas that need attention, the resources feeding a project, an archive that is dormant but still searchable. This is exactly what vulpea is for: it is not a folder tool, it is a query layer over typed metadata, tags, and links. PARA is just one nice thing you can build on that.

People show this best. A person is a resource - someone I keep notes on - and an area in their own right: a relationship I maintain, with meetings and tasks filed under them. In a folder system I would have to pick one drawer and feel wrong about it. As metadata there is nothing to resolve - the note is tagged `people`, it gathers meetings like any area, and it surfaces in whichever view asks: reference when I am reading, an area when I am about to see someone. The buckets are facets, not walls.

And it stays light. No weekly ritual holds it together - the classification maintains itself, and what I actually do is just work, and occasionally look at a view. That is the part GTD never gave me.

# What's next

Two threads I am pulling on.

First, I read all of this on my phone. These same notes build into a small, private, login-gated site - a quiet, paper-like reader I open more than I expected to. Areas already show up there as a first-class thing; Projects, Resources, and a proper Archive view are what I want to add next. If a write-up of that mobile and web view sounds interesting, say so and I'll do one.

Second, the implementation. The pieces here - deriving buckets, auto-tagging on save, building agenda and area views from queries, capture that files itself - are scattered across my public and personal configs in a way that is too personal to copy-paste. I am slowly extracting the reusable core into a package, `vulpea-para`. It is not ready, and I won't pretend a date. When there is something worth sharing, I'll share it.

Until then, if you take one thing from this: the work that matters isn't filing, it's choosing your areas - the few pillars your life actually rests on. Get those right, let tools handle the rest, and PARA stops being a system you maintain and becomes one you just live in - and, finally, enjoy.
