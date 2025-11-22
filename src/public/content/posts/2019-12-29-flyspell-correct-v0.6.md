[flyspell-correct](https://github.com/d12frosted/flyspell-correct) is a package for distraction-free word correction with flyspell via your chosen interface. It's available on [GitHub](https://github.com/d12frosted/flyspell-correct) and MELPA.

I'm pleased to announce version 0.6 of `flyspell-correct`. This release introduces a new interface (`avy-menu`), fixes point movements, warnings, and autoloads, and switches to lexical binding. Read on for the complete changelog.

<!--more-->

## What's New

### New Interface

- Add `avy-menu` interface. Thanks to [@clemera](https://github.com/clemera)!

### Improvements

- Do not move point by correction functions.
- Mark custom interfaces as autoload.
- Add `use-package` examples.
- Switch to `lexical-binding`.

### Bug Fixes

- Fix errors in transient mode. Thanks to [@Ergus](https://github.com/Ergus)!
- Fix many warnings.

## Additional Notes

The [introduction post](/posts/2016-05-09-flyspell-correct-intro) was also updated to better explain the key points of `flyspell-correct`.

Hopefully you'll enjoy the new version. If something doesn't work for you, please let me know!
