[flyspell-correct](https://github.com/d12frosted/flyspell-correct) is a package for distraction-free words correction with flyspell via selected interface. It's available on [GitHub](https://github.com/d12frosted/flyspell-correct) and MELPA.

Recently, there was a major release of `flyspell-correct`. Version 0.5 introduces rapid mode for fixing multiple words in one run, breaking renames and tests. Read further for the complete change list.

<!--more-->

- Skip words that don't need to be fixed (e.g. duplicates).
- Rename several functions (via name deprecation)
  - `flyspell-correct-next-word-generic` -\> `flyspell-correct-next`
  - `flyspell-correct-previous-word-generic` -\> `flyspell-correct-previous`
  - `flyspell-correct-word-generic` -\> `flyspell-correct-at-point`
- Create a wrapper function for most of the cases, namely `flyspell-correct-wrapper`.
- Implement a 'rapid mode' for `flyspell-correct-wrapper`, allowing user to fix multiple words in one run (`skip` action can be used to skip current incorrect word and continue to the next one).
- Add some simple tests to cover the most important use cases.
- Fix issue when `flyspell-correct-next` doesn't correct word at point.

[Intro post](/posts/2016-05-09-flyspell-correct-intro) also was updated to better explain key points of `flyspell-correct`.

Hopefully, you'll like the new version. If something doesn't work for you, please let me know!
