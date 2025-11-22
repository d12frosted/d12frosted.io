A project management tool, or build tool, is an instrument for building, linting, and testing packages. When it comes to Emacs, there are many options available. Arguably, the most popular options are [cask](https://cask.readthedocs.io/en/latest/) and [alphapapa/makem.sh](https://github.com/alphapapa/makem.sh), both being wonderful tools with different approaches. There's also a relatively new star in the field - [doublep/eldev](https://github.com/doublep/eldev/).

In my [recent post](/posts/2021-04-09-emacs-d), I described an approach to convert `emacs.d` into a proper project with a useful byte compiler, linting, and tests, but I never really explored other options.

<!--more-->

[alphapapa/makem.sh](https://github.com/alphapapa/makem.sh) is a rather small Bash script (around 1,200 LoC) aiming for simplicity and transparency. Many projects have adopted it recently because it's easy to set up (no configuration or initialisation needed), and it claims to have zero dependencies (except for the latest `bash`, which isn't always clear from the errors). You simply add a Bash script to your repository, and that's it!

I don't use it in my projects anymore simply because there's no good way to extend `makem.sh` without modifying its sources. This makes it hard to upgrade `makem.sh` to the latest version, as you need to deal with potential merge conflicts. In addition, it's written in Bash, meaning that Emacs Lisp interop is very dirty and hard to maintain. I've hit the `makem.sh` limit and decided to switch. Though for many use cases, it's a very nice project!

[Cask](https://cask.readthedocs.io/en/latest/) is a classic Emacs package project management tool. It's sophisticated, powerful, and well documented. It requires installation on each developer's machine, making it slightly less approachable, though many package managers provide `cask`, so it's as easy as installing the latest `bash`.

| tool    | language   | extensible | requires installation |
|---------|------------|------------|-----------------------|
| `makem` | bash       | no         | no, but yes           |
| `cask`  |            | yes        | yes                   |
| `eldev` | Emacs Lisp | yes        | yes                   |

Further readings:

- [makem comparisons](https://github.com/alphapapa/makem.sh#comparisons)
