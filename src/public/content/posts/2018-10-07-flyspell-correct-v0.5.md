[flyspell-correct](https://github.com/d12frosted/flyspell-correct) is a package for distraction-free word correction with flyspell via your chosen interface. It's available on [GitHub](https://github.com/d12frosted/flyspell-correct) and MELPA.

I'm pleased to announce version 0.5 of `flyspell-correct`. This release introduces rapid mode for fixing multiple words in one run, along with some breaking function renames and new test coverage. Read on for the complete changelog.

<!--more-->

## What's New

### Rapid Mode

- Implement a 'rapid mode' for `flyspell-correct-wrapper`, allowing the user to fix multiple words in one run (the `skip` action can be used to skip the current incorrect word and continue to the next one).
- Create a wrapper function for most cases, namely `flyspell-correct-wrapper`.

### Breaking Changes

- Rename several functions (with deprecation warnings):
  - `flyspell-correct-next-word-generic` → `flyspell-correct-next`
  - `flyspell-correct-previous-word-generic` → `flyspell-correct-previous`
  - `flyspell-correct-word-generic` → `flyspell-correct-at-point`

### Improvements

- Skip words that don't need to be fixed (e.g. duplicates).
- Fix issue when `flyspell-correct-next` doesn't correct word at point.
- Add simple tests to cover the most important use cases.

## Additional Notes

The [introduction post](/posts/2016-05-09-flyspell-correct-intro) was also updated to better explain the key points of `flyspell-correct`.

Hopefully you'll enjoy the new version. If something doesn't work for you, please let me know!
