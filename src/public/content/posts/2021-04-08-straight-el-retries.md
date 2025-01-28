We all know, network is unreliable. I love those moments when I decide to upgrade all my Emacs packages while connected via 3G. And since I am using [raxod502/straight.el](https://github.com/raxod502/straight.el/), the process requires to process each repository one by one. Now imagine my frustration, when due to unstable connection it fails on one of the repositories and I need to start the process again… from the beginning.

So I say, no more, [raxod502/straight.el](https://github.com/raxod502/straight.el/) should retry read operations over network! Unfortunately, there is no such option out of box (or at least I could not find it). Luckily, it's Emacs!

<!--more-->

First, we want to configure how many times we want to retry before actually failing.

``` commonlisp
(defvar elpa-straight-retry-count 3
  "Amount of retries for `straight' operations.")
```

Secondly, we write a 'generic' function that simply retries some `orig-fn`. It simply evaluates a function, and if it fails, tries again in a `while` loop. It doesn't report intermediate failures, if you wish to, it's easy to do. But once it gets to the final error, it signals it back to the user.

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

Lastly, we need to wrap functions that do some networking. You see, thanks to `advice-add`, it's so easy to do!

``` commonlisp
(advice-add #'straight-fetch-package
            :around
            #'elpa-straight-with-retry)
(advice-add #'straight--clone-repository
            :around
            #'elpa-straight-with-retry)
```

Now you might wonder, is it possible to apply `elpa-straight-with-retry` to other functions? Of course! Just give it a better name and enjoy your Emacs life full of retries.

Safe travels!
