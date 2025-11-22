We all know that networks are unreliable. I particularly enjoy those moments when I decide to upgrade all my Emacs packages whilst connected via 3G. Since I'm using [raxod502/straight.el](https://github.com/raxod502/straight.el/), the process needs to check each repository one by one. Now imagine my frustration when, due to an unstable connection, it fails on one of the repositories and I need to start the process again… from the beginning.

So I decided: no more! [raxod502/straight.el](https://github.com/raxod502/straight.el/) should retry read operations over the network. Unfortunately, there's no such option out of the box (or at least I couldn't find it). Luckily, it's Emacs!

<!--more-->

First, we configure how many times we want to retry before actually failing.

``` commonlisp
(defvar elpa-straight-retry-count 3
  "Amount of retries for `straight' operations.")
```

Secondly, we write a generic function that retries some `orig-fn`. It simply evaluates a function, and if it fails, tries again in a `while` loop. It doesn't report intermediate failures (though if you wish to do so, it's easy to add). Once it reaches the final error, it signals it back to the user.

``` commonlisp
(defun elpa-straight-with-retry (orig-fn &rest args)
  "Wrapper around ORIG-FN supporting retries.

ORIG-FN is called with ARGS and retried
`elpa-straight-retry-count' times."
  (let ((n elpa-straight-retry-count)
        (res nil))
    (while (> n 0)
      (condition-case err
          (progn
            (setq res (apply orig-fn args)
                  n 0)
            res)
        (error
         (setq n (- n 1))
         (unless (> n 0)
           (signal (car err) (cdr err))))))))
```

Lastly, we need to wrap functions that do some networking. Thanks to `advice-add`, this is straightforward!

``` commonlisp
(advice-add #'straight-fetch-package
            :around
            #'elpa-straight-with-retry)
(advice-add #'straight--clone-repository
            :around
            #'elpa-straight-with-retry)
```

You might wonder whether it's possible to apply `elpa-straight-with-retry` to other functions. Of course! Just give it a better name and enjoy your Emacs life full of retries.

Safe travels!
