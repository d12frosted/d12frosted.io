Have you ever found yourself writing in Emacs, spotted a spelling mistake a few lines back, and felt annoyed at having to manually navigate back to fix it? Whilst [Flyspell](https://www.emacswiki.org/emacs/FlySpell) does an excellent job of highlighting these errors, correcting them can feel clunky - especially when you're used to the sleek completion interfaces offered by modern frameworks like [Ivy](https://github.com/abo-abo/swiper), [Helm](https://github.com/emacs-helm/helm), or [popup.el](https://github.com/auto-complete/popup-el).

When I switched from Helm to Ivy, I discovered that whilst correction packages existed for popup.el ([wiki](https://www.emacswiki.org/emacs/FlySpell#toc11), [flyspell-popup](https://github.com/xuchunyang/flyspell-popup)) and Helm ([helm-flyspell](https://github.com/pronobis/helm-flyspell)), there wasn't a unified solution that worked across different completion frameworks. Each package essentially did the same thing, just with different front-ends. This seemed wasteful.

That's why I created [flyspell-correct](https://github.com/d12frosted/flyspell-correct) - a unified package for distraction-free word correction that works with any completion interface you prefer. It's available on [GitHub](https://github.com/d12frosted/flyspell-correct) and MELPA.

**What you'll learn in this post:**

- How to set up flyspell-correct with your preferred completion framework
- How to correct spelling mistakes without leaving your current position
- How to implement your own custom correction interface
- How to use rapid mode to fix multiple mistakes efficiently

**Note:** This is an introduction post from the initial release. For the most up-to-date documentation, please refer to the [README](https://github.com/d12frosted/flyspell-correct) file on GitHub.

<!--more-->

To get started, install the package from MELPA and set the `flyspell-correct-interface` variable to one of the predefined interfaces (`flyspell-correct-ivy`, `flyspell-correct-helm`, `flyspell-correct-popup`, `flyspell-correct-ido`, or `flyspell-correct-dummy`). Alternatively, you can provide your own custom interface function. Once configured, simply call `flyspell-correct-wrapper` from anywhere in your buffer to correct the nearest misspelt word. I recommend binding it to a convenient key for quick access.

``` commonlisp
;; set ivy as correcting interface
(setq flyspell-correct-interface #'flyspell-correct-ivy)

;; bind flyspell-correct-wrapper
(define-key flyspell-mode-map (kbd "C-;") #'flyspell-correct-wrapper)
```

# Implementing custom interfaces

One of the key features of flyspell-correct is its extensibility. You can implement your own correction interface to match your workflow preferences. A custom interface is simply a function that accepts two arguments - a list of correction candidates and the incorrect word - and returns either a replacement word or a `(command, word)` tuple.

The available commands are:

- `skip` – no action needed for the current incorrect word
- `save` – add the word to your personal dictionary permanently
- `session` – accept the word for the current Emacs session only
- `buffer` – accept the word for the current buffer only

Let's examine `flyspell-correct-ivy` as a reference implementation.

``` commonlisp
(defun flyspell-correct-ivy (candidates word)
  "Run `ivy-read' for the given CANDIDATES.

List of CANDIDATES is given by flyspell for the WORD.

Return a selected word to use as a replacement or a tuple
of (command, word) to be used by `flyspell-do-correct'."
  (let* (result
         (action-default (lambda (x) (setq result x)))
         (action-save-word (lambda (_) (setq result (cons 'save word))))
         (action-accept-session (lambda (_) (setq result (cons 'session word))))
         (action-accept-buffer (lambda (_) (setq result (cons 'buffer word))))
         (action-skip-word (lambda (_) (setq result (cons 'skip word))))
         (action `(1
                   ("o" ,action-default "correct")
                   ("s" ,action-save-word "Save")
                   ("S" ,action-accept-session "Accept (session)")
                   ("b" ,action-accept-buffer "Accept (buffer)")
                   ("k" ,action-skip-word "Skip"))))
    (ivy-read (format "Suggestions for \"%s\" in dictionary \"%s\": "
                      word (or ispell-local-dictionary
                               ispell-dictionary
                               "Default"))
              candidates
              :action action
              :caller 'flyspell-correct-ivy)
    result))
```

As you can see, the implementation is quite straightforward. We simply call `ivy-read` with the candidates provided by Flyspell for the incorrect word. The default action returns the selected replacement word, whilst the other actions return the `(command, word)` tuple we discussed earlier.

# Distraction-free meaning

The most important feature of `flyspell-correct` is the ability to correct words without leaving your current position. By default, `flyspell-correct-wrapper` jumps to the nearest incorrect word before the point, corrects it, and returns you to where you were. This means you don't have to manually navigate backwards to fix a typo you made several lines ago.

If you have multiple mistakes to fix, you can use 'rapid mode' by calling `flyspell-correct-wrapper` with a universal argument (e.g., `C-u C-;`). This allows you to correct all mistakes one by one in the current direction. Simply use the `skip` command for any words you want to leave unchanged.

You can change the correction direction by calling `flyspell-correct-wrapper` with `C-u C-u`.

# Screenshots

## Ivy interface

<div class="d12-images-block-[100%]">

![](/images/2016-05-09-flyspell-correct-intro/2022-07-19-17-56-25-screenshot-ivy-1.webp)

![](/images/2016-05-09-flyspell-correct-intro/2022-07-19-17-56-25-screenshot-ivy-2.webp)

</div>

## Popup interface

<img src="/images/2016-05-09-flyspell-correct-intro/2022-07-19-17-56-45-screenshot-popup.webp" class="d12-image-1/2" />

## Helm interface

<img src="/images/2016-05-09-flyspell-correct-intro/2022-07-19-17-56-59-screenshot-helm.webp" class="d12-image-1/2" />

## Ido interface

<img src="/images/2016-05-09-flyspell-correct-intro/2022-07-19-17-57-10-screenshot-ido.webp" class="d12-image-1/2" />

# Final thoughts

I hope flyspell-correct makes spell-checking in Emacs a more pleasant experience for you. The package is [open source](https://github.com/d12frosted/flyspell-correct), and contributions are warmly welcome - whether that's bug reports, feature requests, or pull requests. Happy writing!
