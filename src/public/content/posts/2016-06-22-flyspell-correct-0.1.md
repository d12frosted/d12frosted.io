I'm pleased to announce the first official release of [flyspell-correct](https://github.com/d12frosted/flyspell-correct) (v0.1)! If you're not familiar with the package, it provides distraction-free spell-checking correction for Emacs through various completion interfaces. You can read the [introduction post](https://d12frosted.io/posts/flyspell-correct-intro) for more details.

# What's new in v0.1

Since the initial implementation, I've made several important changes:

## Package restructuring

I've split the monolithic package into separate core and interface packages. This means `flyspell-correct` is now the core package, whilst the interfaces (Ivy, Helm, Popup, and Ido) are distributed as separate packages. This restructuring allows for:

- Proper dependency management
- Cleaner separation of concerns
- Fixes for some issues with the Helm interface

## New function for backwards correction

I've added `flyspell-correct-previous-word-generic`, which makes it easier to correct misspelt words that are behind the cursor. Whilst this function works, I've already identified some areas for improvement, which I'll address in the next release.

# Installation

You can install `flyspell-correct` from MELPA, and find the source code on [GitHub](https://github.com/d12frosted/flyspell-correct). For detailed documentation and usage examples, please refer to the [README](https://github.com/d12frosted/flyspell-correct) file.
