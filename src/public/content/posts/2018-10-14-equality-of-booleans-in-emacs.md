<img src="/images/2018-10-14-equality-of-booleans-in-emacs/2022-07-19-20-34-19-emacs-booleans.webp" class="d12-image-3/4" />

There is a fun story about booleans in Emacs Lisp - there are no booleans in Emacs Lisp. Sort of. Because we have a symbol `nil`, which means an empty list. You can write it as `nil` or `()` - they both stand for the same object, the symbol `nil`.

``` commonlisp
(eq nil ())                             ; => t
```

Since LISP is all about list processing, empty list is something very `false`. So `false` that we don't have special symbol for `false` values, as empty list serves this purpose well.

Everything that is not an empty list has a meaning of `true`. However, there is a symbol `t` which is the preferred way to represent the truth value `true`.

So `nil` and `t` are considered canonical boolean values. There is a function `booleanp` that returns `t` if the argument is a canonical boolean value and `nil` otherwise.

The fun begins when you need to check if two boolean values are equal. Since non-nil (or not an empty list) can mean many different things (like `"Emacs is the only true editor"`) you can't just do regular equality check.

``` commonlisp
(equal t "Emacs is the only true editor") ; => nil
```

There are, however, several tricks to get it working. The most obvious solution is to convert value to a canonical boolean value.

<!--more-->

# if/when

We can directly use `if` function.

``` commonlisp
(if "Some truth" t nil)                 ; => t
(if 42 t nil)                           ; => t
(if t t nil)                            ; => t
(if nil t nil)                          ; => nil

(let ((a t)
      (b "Emacs is the only true editor"))
  (equal (if a t nil) (if b t nil)))    ; => t

(defun boolean-eq (a b)
  (equal (if a t nil)
         (if b t nil)))

(let ((a t)
      (b "Emacs is the only true editor"))
  (boolean-eq a b))                     ; => t
```

Directly using `if` is a little bit cumbersome, but when we hide it inside of a helper function it's not that bad, actually.

The same result can be achieved by using `when`.

``` commonlisp
(when "Some truth" t)                   ; => t
(when 42 t)                             ; => t
(when t t)                              ; => t
(when nil t)                            ; => nil

(let ((a t)
      (b "Emacs is the only true editor"))
  (equal (when a t) (when b t)))        ; => t

(defun boolean-eq (a b)
  (equal (when a t)
         (when b t)))

(let ((a t)
      (b "Emacs is the only true editor"))
  (boolean-eq a b))                     ; => t
```

# not

There is another function we can use - `not`, which returns `t` if the argument is `nil`, and returns `nil` otherwise. Yes, it negates the value, but the result is one of the canonical booleans, so we are good.

Since $a \equiv b$ is equivalent to $\neg a \equiv \neg b$, we can just compare negated values.

| $a$ | $b$ | $\neg a$ | $\neg b$ | $a \equiv b$ | $\neg a \equiv \neg b$ |
|-----|-----|----------|----------|--------------|------------------------|
| 0   | 0   | 1        | 1        | 1            | 1                      |
| 0   | 1   | 1        | 0        | 0            | 0                      |
| 1   | 0   | 0        | 1        | 0            | 0                      |
| 1   | 1   | 0        | 0        | 1            | 1                      |

``` commonlisp
(not "Some truth")                      ; => nil
(not 42)                                ; => nil
(not t)                                 ; => nil
(not nil)                               ; => t

(let ((a t)
      (b "Emacs is the only true editor"))
  (equal (not a) (not b)))              ; => t

(defun boolean-eq (a b)
  (equal (not a) (not b)))

(let ((a t)
      (b "Emacs is the only true editor"))
  (boolean-eq a b))                     ; => t
```

This one looks a little bit better when used without a helper function, at least in my opinion.

# xor/or/and

Sometimes you want to do something when two 'boolean' values are not equal.

``` commonlisp
(let ((a nil)
      (b "Emacs is the only true editor"))
  (unless (equal (not a) (not b))
    (message "Some real work begins"))) ; => Some real work begins
```

For such situations, there is a `xor` function, which returns `nil` when both arguments are equal in the canonical boolean form and `t` otherwise.

``` commonlisp
(xor nil nil)                           ; => nil
(xor nil t)                             ; => t
(xor t nil)                             ; => t
(xor t t)                               ; => nil

(xor "Some truth" nil)                  ; => t
(xor "Some truth" t)                    ; => nil

(xor 42 nil)                            ; => t
(xor 42 t)                              ; => nil

(let ((a nil)
      (b "Emacs is the only true editor"))
  (when (xor a b)
    (message "Some real work begins"))) ; => Some real work begins
```

Other functions (like `or`, `and`) also convert values to canonical boolean values. So you can keep it in mind.

# Epilogue

The sole purpose of this post is fun. If you didn't get your portion of fun, then it's not funny at all. Please fix it somehow.
