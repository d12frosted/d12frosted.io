[flyspell-correct](https://github.com/d12frosted/flyspell-correct) is a package for distraction-free words correction with flyspell via selected interface. It's available on [GitHub](https://github.com/d12frosted/flyspell-correct) and MELPA.

Not long ago I have released the first version of `flyspell-correct`. There are several interesting changes compared to initial implementation. First of all, I divided this package into four different packages - core and interfaces. This allowed me to properly setup dependencies and also solved some problems with `helm` interface. Second important change - I've added a function called `flyspell-correct-previous-word-generic`, which is useful when the misspelled word is not under point, but long behind it. Right now I realise that it doesn't work as perfect as I want, so I'll improve it in the next release.

You can find more information about `flyspell-correct` on [GitHub](https://github.com/d12frosted/flyspell-correct).
