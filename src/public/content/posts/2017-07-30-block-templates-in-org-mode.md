Anyone would agree that typing `#+begin_src ... #+end_src` blocks manually feels **strange**.

In order to make life easier, some people implement helpers to insert these blocks. For example, Chris Done has `org-begin-template` function in his [configs](https://github.com/chrisdone/chrisdone-emacs/blob/master/config/org.el#L30). Other people use [yatemplate](https://github.com/mineo/yatemplate) to create generic or specific templates for blocks.

But what is most fascinating, is that `org-mode` bundles with templates for some structural elements like these blocks. And instead of copy-pasting solutions into your `.emacs` file, take a look at [Structure Templates](https://orgmode.org/org.html#Structure-Templates).

<!--more-->

With just a few keystrokes, it is possible to insert empty an structural block, such as `#+begin_src ... #+end_src`, or to wrap existing text in such a block. Just type `C-c C-,` (`org-insert-structure-template`) to prompt for a type of block structure, and insert the block at point. If the region is active, it is wrapped in the block. Available structure types are defined in `org-structure-template-alist`, see the docstring for adding or changing values.

You can also use an old Easy Template system, which is disabled by default in Org mode 9.2 and upper. One can turn it back by enabling `org-tempo` module either by adding it to `org-modules` variable or by `(require 'org-tempo)`.

With this feature, you type `<` followed by template selector, then hit `TAB` and voilà. For source blocks, it's just `<s<TAB>`.

Just for reference, here is a list of available templates and their corresponding selectors. Please note that this table might become out of date, so it's better to refer to the official documentation.

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
