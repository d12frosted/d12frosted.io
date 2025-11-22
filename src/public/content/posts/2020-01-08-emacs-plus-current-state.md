Not everyone knows this, but [emacs-plus](https://github.com/d12frosted/homebrew-emacs-plus) is almost an April Fool's joke that was a day late. Its [initial release](https://github.com/d12frosted/homebrew-emacs-plus/commit/6c85ae34d2649512a3088207b5f2a81c23cd8630) was on 2nd April 2016, and the only feature it had compared to the official Emacs formula back then was the Spacemacs icon (installed by default). Homebrew maintainers didn't want to increase the complexity of the Emacs formula, so I took that burden on myself. I created a tap with one additional icon, and after several years, the info command for `emacs-plus` now prints a wall of options.

I'm very happy to see so many users of this feature-inclusive (and sometimes experimental) formula. I'm grateful to all of you - I would've abandoned this project a year ago if it weren't for you! I think it's fair to share the current state of the project and shed some light on my plans.

<!--more-->

# The problem

Over the last year (or perhaps even longer), `emacs-plus` has accumulated several issues. Here's my top list:

1.  The experimental `xwidgets` build would break from time to time, and I would only learn about it thanks to users (mostly [Ag Ibragimov](https://github.com/agzam)).
2.  Whilst I state that `HEAD` builds aren't supported, I still want this option to work. First, so Emacs users can easily get the latest features. Second, so Emacs maintainers can get feedback from users.
3.  Patches are located outside the project, and their future is unclear.
4.  It's hard to contribute a new patch or patch fix to the `emacs-plus` repository because you have to mess with URLs and branch names.
5.  Travis builds are very slow. The default build takes 7 minutes, builds from source take 13 minutes, and HEAD builds were impossible. I'd like to test many more build options and their combinations.
6.  Too many options are given to users, which contradicts my initial idea.

Build failures can be divided into two categories: patch application failures or upstream issues.

# Visibility and stability

To address these issues, I've taken the following actions:

I moved from Travis to GitHub Workflows, which gave a decent boost in compilation time. It also enabled me to add the `HEAD` and `xwidgets` options to the build matrix. I'd love to add `x11` as well, but I haven't figured out how to set up XQuartz on build machines. I haven't really measured the build time precisely, but here are some examples I pulled from the build history. Also, I've heard that Travis has improved their machines, so more computing power is now available to users.

| Options                  | Travis  | GitHub  |
|--------------------------|---------|---------|
|                          | 6m 30s  | 3m 14s  |
| `--with-no-titlebar`     | 11m 16s | 5m 18s  |
| `--HEAD`                 | \-      | 11m 18s |
| `--HEAD --with-xwidgets` | \-      | 11m 15s |

This gives me better visibility into build breakages (both when patch application fails and when it's an upstream issue).

All the patches I use have been moved to the `emacs-plus` repository, so they're always bundled with the formula. I've also added support for running CI builds with patches from pull requests.

To better maintain the patches, I've created the [emacs-plus-basis](https://github.com/d12frosted/emacs-plus-basis) repository, which is basically a fork of the Emacs repository with patches applied on top. Every day, CI fetches the latest updates from upstream and applies the patches. If it fails, I get a notification. This gives me visibility into patch application failures and provides an environment to fix these patches.

Whilst all these steps should improve the stability of `emacs-plus`, I'm aware that build breakages are impossible to avoid (upstream changes very quickly). What I'm achieving here is better visibility and better tools to address breakages more quickly.

# Build options zoo

What I haven't covered yet is the sheer number of build options. The initial idea behind `emacs-plus` was that users shouldn't need to pass many custom options to the `install` command. If you remember, back then everyone recommended installing Emacs from Homebrew using the following command:

``` bash
$ brew install emacs --with-cocoa --with-gnutls --with-rsvg --with-imagemagick
```

All of these options were recommended to most users, so I decided to turn them on by default:

``` bash
$ brew install emacs-plus
```

In my ideal world, the only options users should pass are the icon choice and experimental or controversial features.

I still don't have a solution, but I'm willing to decrease the number of available options. For example, I don't think anyone actually installs without Cocoa or without X11 - so the choice should be between these two options. GnuTLS? Well, why would anyone want to install Emacs without it? `librsvg` and `libxml2`? Does it hurt to have them included? I want `emacs-plus` to be as simple as possible whilst still providing things that aren't available from the official Emacs formula.

Unfortunately, I don't have anonymous statistics about option usage, so I still don't have any decisions in this area. I'll keep you posted!

# Cask

This is something people are waiting for. I understand why, but I don't have time to invest in this right now. If someone wants to step in, I'd be super glad and grateful!

# Final words

All contributions are welcome! Let's keep in touch.
