Ironically, I find the [helpful](https://github.com/Wilfred/helpful) package quite helpful. It enhances Emacs' **help** buffer with much more contextual information. If you haven't tried it yet, I advise you to do so.

By default, though, it doesn't play nicely with windows.

Usually when I'm writing Elisp and want to read the documentation of a function or variable, I hit `C-h f` or `C-h v` respectively, and the help buffer appears in a separate window. This is convenient because I can see both the code and the help.

Sometimes the help contains links to other entries that I need to navigate. When I hit `<RET>`, the window containing my code shows another help buffer. This might be fine for some people, but I hate this behaviour because I usually want to keep seeing the code I'm editing.

This is also annoying if you set `helpful-max-buffers` to `1`. The help window and the code window swap positions on every navigation.

Fortunately, it's configurable (as almost everything in Emacs).

``` commonlisp
(setq helpful-switch-buffer-function #'+helpful-switch-to-buffer)

(defun +helpful-switch-to-buffer (buffer-or-name)
  "Switch to helpful BUFFER-OR-NAME.

The logic is simple: if we're currently in the helpful buffer,
reuse its window; otherwise, create a new one."
  (if (eq major-mode 'helpful-mode)
      (switch-to-buffer buffer-or-name)
    (pop-to-buffer buffer-or-name)))
```

<!--more-->
