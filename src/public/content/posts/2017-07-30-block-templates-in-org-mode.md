I think everyone would agree that manually typing `#+begin_src ... #+end_src` blocks feels tedious.

To make life easier, some people implement helpers to insert these blocks. For example, Chris Done has an `org-begin-template` function in his [configs](https://github.com/chrisdone/chrisdone-emacs/blob/master/config/org.el#L30), whilst others use [yatemplate](https://github.com/mineo/yatemplate) to create generic or specific templates for blocks.

What's most fascinating, though, is that `org-mode` bundles templates for structural elements like these blocks. Instead of copy-pasting solutions into your `.emacs` file, take a look at [Structure Templates](https://orgmode.org/org.html#Structure-Templates) - a built-in feature that solves this problem elegantly.

<!--more-->

With just a few keystrokes, you can insert an empty structural block (such as `#+begin_src ... #+end_src`) or wrap existing text in such a block. Simply type `C-c C-,` (`org-insert-structure-template`) to prompt for the type of block structure, and the block will be inserted at point. If the region is active, it will be wrapped in the block. Available structure types are defined in `org-structure-template-alist` - see the docstring for details on adding or changing values.

You can also use the old Easy Template system, which is disabled by default in Org mode 9.2 and above. You can re-enable it by enabling the `org-tempo` module, either by adding it to the `org-modules` variable or by calling `(require 'org-tempo)`.

With this feature, you type `<` followed by a template selector, then hit `TAB`, and voilà! For source blocks, it's just `<s<TAB>`.

For reference, here's a list of available templates and their corresponding selectors. Please note that this table might become outdated, so it's best to refer to the official documentation.

| Key | Template                            |
|-----|-------------------------------------|
| `s` | `#+BEGIN_SRC ... #+END_SRC`         |
| `e` | `#+BEGIN_EXAMPLE ... #+END_EXAMPLE` |
| `q` | `#+BEGIN_QUOTE ... #+END_QUOTE`     |
| `v` | `#+BEGIN_VERSE ... #+END_VERSE`     |
| `c` | `#+BEGIN_CENTER ... #+END_CENTER`   |
| `l` | `#+BEGIN_LaTeX ... #+END_LaTeX`     |
| `L` | `#+LaTeX:`                          |
| `h` | `#+BEGIN_HTML ... #+END_HTML`       |
| `H` | `#+HTML:`                           |
| `a` | `#+BEGIN_ASCII ... #+END_ASCII`     |
| `A` | `#+ASCII:`                          |
| `i` | `#+INDEX: line`                     |
| `I` | `#+INCLUDE: line`                   |

That's it.
