Project management tool, or a build tool, is an instrument for building, linting and testing packages. When it comes to Emacs, there are many options out in the wild. Arguably, the most popular options are [cask](https://cask.readthedocs.io/en/latest/) and [alphapapa/makem.sh](https://github.com/alphapapa/makem.sh), both being wonderful tools with different approach. There is also a relatively new star in the field - [doublep/eldev](https://github.com/doublep/eldev/).

In my [recent post](/posts/2021-04-09-emacs-d) I have described an approach to convert `emacs.d` into proper project with useful byte compiler, linting and tests, but I never really explored other options.

<!--more-->

[alphapapa/makem.sh](https://github.com/alphapapa/makem.sh) is a rather small bash script (around 1200 LoC) with aim on simplicity and transparency. Many projects have adopted it recently, because it is easy to setup (no configuration or initialization needed) and it claims to have zero dependencies (except for the latest `bash`, which sometimes is not clear from the errors). You simply add a bash script to your repository and that's it!

I don't use it in my projects anymore simply because there is no good way to extend `makem.sh` without modifying its sources. This makes it hard to upgrade `makem.sh` to the latest version as you need to deal with potential merge conflicts. In addition, it is written in bash, meaning that Emacs Lisp interop is very dirty and hard to maintain. I've hit `makem.sh` limit and decided to switch. Though for many use cases it is a very nice project!

[Cask](https://cask.readthedocs.io/en/latest/) is a classic Emacs package project management tool. It is sophisticated, powerful and well documented. It requires to be installed on each developers machine, making it a little bit less approachable, though many package managers provide `cask`, so it's as easy as installing latest `bash`.

| tool    | language   | extensible | requires installation |
|---------|------------|------------|-----------------------|
| `makem` | bash       | no         | no, but yes           |
| `cask`  |            | yes        | yes                   |
| `eldev` | Emacs Lisp | yes        | yes                   |

Further readings:

- [makem comparisons](https://github.com/alphapapa/makem.sh#comparisons)
