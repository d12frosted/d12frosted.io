[flyspell-correct](https://github.com/d12frosted/flyspell-correct) is a package for distraction-free words correction with flyspell via selected interface. It's available on [GitHub](https://github.com/d12frosted/flyspell-correct) and MELPA.

It's very common to make mistakes while writing notes, comments or whatever. And a desire to have spell checking software is natural. In Emacs, we usually use [Flyspell](https://www.emacswiki.org/emacs/FlySpell) for that purpose. It's a great piece of software, but sometimes it lacks better interface for correcting words. Especially since there are completion packages like [popup.el](https://github.com/auto-complete/popup-el), [helm](https://github.com/emacs-helm/helm), [ivy](https://github.com/abo-abo/swiper) etc. There are already available separate packages for correcting words via `popup.el` ([wiki](https://www.emacswiki.org/emacs/FlySpell#toc11), [flyspell-popup](https://github.com/xuchunyang/flyspell-popup)) and `helm` ([helm-flyspell](https://github.com/pronobis/helm-flyspell)). Since recently I have switched from `helm` to `ivy`, I've found a lack of similar package for `ivy`, so I decided to write my own.

But available packages are all the same, except the part that calls completion front-end, so my goal was to provide a generic package allowing usage of any interface. Either from the predefined set (`ivy`, `helm`, `popup` or `dummy`) or any user-defined.

This is an introduction post, for more up-to-date documentation, please refer to [README](https://github.com/d12frosted/flyspell-correct) file.

<!--more-->

In order to use this package, just install it from MELPA, setup `flyspell-correct-interface` variable to one of predefined (`flyspell-correct-ivy`, `flyspell-correct-helm`, `flyspell-correct-popup`, `flyspell-correct-ido`, `flyspell-correct-dummy`) or your own interface function. Then all you have to do is to call `flyspell-correct-wrapper` from any point of the buffer. You can also bind `flyspell-correct-wrapper` to any key binding.

``` commonlisp
;; set ivy as correcting interface
(setq flyspell-correct-interface #'flyspell-correct-ivy)

;; bind flyspell-correct-wrapper
(define-key flyspell-mode-map (kbd "C-;") #'flyspell-correct-wrapper)
```

# Implementing custom interfaces

As it was already said, you can implement your own interface for correcting words. It has to be a function that takes two arguments - candidates and incorrect word. It has to return either replacement word or `(command, word)` tuple, where `command` can be one of the following:

- `skip` - meaning that no action is required for current incorrect `word`;
- `save` - meaning that the `word` must be saved in a dictionary;
- `session` - meaning that the `word` must be saved for the current session;
- `buffer` - meaning that the `word` must be saved for the current buffer.

Let's check `flyspell-correct-ivy` as an example of a custom interface.

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

As you see, it's pretty straightforward. Just run `ivy-read` with candidates provided by `flyspell` for the incorrect word. Default action just returns selected replacement word. While other actions return mentioned tuple of `(command, word)`.

# Distraction-free meaning

One of the most important parts of the `flyspell-correct` package is the ability to correct words that are far from the point. By default, `flyspell-correct-wrapper` jumps to the first incorrect word before the point. So you don't have to move the point manually to correct a mistake made several words or lines before.

Also, if you happen to have multiple mistakes before the point, you can use `flyspell-correct-wrapper` in a so-called 'rapid mode'. Just call it with a universal argument (e.g. `C-u C-;`) and you'll get an option to correct all mistakes one by one in a current direction. If you want to leave incorrect word as is, just use `skip` command.

The direction of the spelling can be changed by calling `flyspell-correct-wrapper` with `C-u C-u`.

# Screenshots

## Ivy interface

![](/images/2016-05-09-flyspell-correct-intro/2022-07-19-17-56-25-screenshot-ivy-1.webp)

![](/images/2016-05-09-flyspell-correct-intro/2022-07-19-17-56-25-screenshot-ivy-2.webp)

## Popup interface

![](/images/2016-05-09-flyspell-correct-intro/2022-07-19-17-56-45-screenshot-popup.webp)

## Helm interface

![](/images/2016-05-09-flyspell-correct-intro/2022-07-19-17-56-59-screenshot-helm.webp)

## Ido interface

![](/images/2016-05-09-flyspell-correct-intro/2022-07-19-17-57-10-screenshot-ido.webp)

# Last few words

Contributions are warmly welcome!
