Ironically, I find the [helpful](https://github.com/Wilfred/helpful) package quite helpful. It boosts Emacs **help** buffer with much more contextual information. If you haven't tried it out yet, I advice you to do so.

However, by default, it doesn't play nicely with windows.

Usually when I write some Elisp and I want to read the documentation of some function or variable, I hit `C-h f` or `C-h v` respectively and the help buffer is shown in the separate window. Which is convenient in my opinion, because I can see the code and the help.

Sometimes help contains links to other entries that I need to navigate. And when I hit `<RET>` window containing code shows another help buffer. Which might be good for some people, but I hate this behaviour, because usually I want to see the code that I am editing.

This is also annoying if you set the value of `helpful-max-buffers` to `1`. Help window and the window with code are swapped on every navigation.

The good thing, it's configurable (as almost everything in Emacs land).

``` commonlisp
(setq helpful-switch-buffer-function #'+helpful-switch-to-buffer)

(defun +helpful-switch-to-buffer (buffer-or-name)
  "Switch to helpful BUFFER-OR-NAME.

The logic is simple, if we are currently in the helpful buffer,
reuse it's window, otherwise create new one."
  (if (eq major-mode 'helpful-mode)
      (switch-to-buffer buffer-or-name)
    (pop-to-buffer buffer-or-name)))
```

<!--more-->
