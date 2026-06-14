<img src="/images/2026-06-14-vui-pomodoro/pomodoro-hero.webp" class="d12-image" />

``` related_posts
```

A while ago I stumbled on a [stream](https://www.youtube.com/watch?v=7H3Lvo9LCvs) where David of [System Crafters](https://systemcrafters.net/) builds a small Pomodoro app with vui.el. I wrote [vui.el](https://github.com/d12frosted/vui.el) mostly for my own needs, and watching someone else reach for it - live, without me whispering in their ear - was the most useful hour of feedback I've had on the library. David, huge kudos for recording it.

Two things happened. David hit a few confusing errors, the kind that are obvious once you know the trick and baffling when you don't. And when I sat down to build the same app myself, I discovered the library had real bugs - a timer that kept ticking after I killed its buffer, teardown that never ran. The exercise was genuinely harder than I expected, and that is exactly why it makes a good walkthrough.

So let's build a Pomodoro timer from zero. Not the polished version first - we'll walk into the walls you hit on the way, and a few I only found by building it myself, and I'll explain why each one is a wall before we climb over it. If at any point you just want the finished article, it lives in the repo as [`docs/examples/10-pomodoro.el`](https://github.com/d12frosted/vui.el/blob/master/docs/examples/10-pomodoro.el) (`M-x vui-example-pomodoro`).

<div class="info">

**What you'll learn:**

- Why a timer belongs in `vui-use-effect`, not in a click handler
- How to key an effect on a flag so it starts and stops exactly once
- Why async callbacks need `vui-with-async-context` and functional `vui-set-state`
- Why a countdown should measure time from the clock, not count timer ticks
- How to derive state transitions instead of hand-managing them
- The teardown bug this example uncovered, and how cleanup works now

</div>

If you haven't read the [quickstart](/posts/2025-12-01-vui-quickstart): vui.el requires lexical binding. Every snippet below assumes a file starting with `;;; -*- lexical-binding: t -*-`. Hold that thought - it's also the very first wall.

# The Wall Before Step 1: lexical-binding

Here's the kind of thing you write first - a clock and a button that reports the time:

<div class="tabs">

<div class="tab">

**Example**

``` elisp
(vui-defcomponent pomodoro-timer ()
  :state ((time-text "25:00")
          (button-label "Start"))
  :render
  (vui-vstack
   (vui-text time-text :face 'error)
   (vui-button button-label
     :on-click (lambda ()
                 (message "Time is %s" time-text)))))

(vui-mount (vui-component 'pomodoro-timer))
```

</div>

<div class="tab">

**Screenshot**

![](/images/2026-06-14-vui-pomodoro/pomodoro-first-example.webp)

</div>

</div>

It renders fine. The clock shows `25:00`. Then you click the button and Emacs spits out:

``` example
Warning (vui): VUI event error in on-click (pomodoro-timer):
  Symbol's value as variable is void: time-text
```

Render works, click fails. That split is the whole tell. `vui-defcomponent` compiles `:render` into a function that binds your state keys with `let`:

``` elisp
(let ((time-text    (plist-get state :time-text))
      (button-label (plist-get state :button-label)))
  ;; ...your render form, including the :on-click lambda...
  )
```

During render the `let` is active, so `(vui-text time-text ...)` reads `time-text` happily. But your `:on-click` lambda runs **later**, long after that `let` has returned. For the lambda to still remember `time-text`, it has to be a lexical closure - and closures only capture when the buffer has `lexical-binding` turned on. Without it, the lambda looks `time-text` up as a global variable at click time, finds nothing, and throws.

The fix is one line at the top of the file:

``` elisp
;;; -*- lexical-binding: t -*-
```

vui.el and all its examples carry this header; a fresh scratch buffer or a hand-typed snippet might not. If you ever see *void variable* for something that clearly exists in `:state`, this is almost always why. (You can confirm with `C-h v lexical-binding` in the buffer you're evaluating in.)

With that out of the way, let's build for real.

# Step 1: A Static Clock

**Goal:** Show `25:00` and nothing else. Get the shape of the component right before any behaviour.

We'll hold the remaining time as a number of seconds and format it for display. Seconds, not a string - we're going to do arithmetic on it soon, and storing `MM:SS` as text would mean parsing it back constantly.

<div class="tabs">

<div class="tab">

**Example**

``` elisp
;;; -*- lexical-binding: t -*-

(require 'vui)

(defun pomodoro--format (seconds)
  "Format SECONDS as MM:SS."
  (format "%02d:%02d" (/ seconds 60) (% seconds 60)))

(vui-defcomponent pomodoro-timer ()
  :state ((remaining (* 25 60)))
  :render
  (vui-vstack
   (vui-text "Pomodoro" :face 'outline-1)
   (vui-text (pomodoro--format remaining) :face 'bold)))

(vui-mount (vui-component 'pomodoro-timer))
```

</div>

<div class="tab">

**Screenshot**

![](/images/2026-06-14-vui-pomodoro/pomodoro-step1-static-clock.webp)

</div>

</div>

A title and a clock frozen at `25:00`.

**What you learned:** State as a number, a pure helper for presentation, `vui-vstack` for layout. Nothing moves yet.

# Step 2: A Start Button, and the Tempting Wrong Way

**Goal:** Make the clock count down when you click Start. This is where almost everyone reaches for the obvious thing, and the obvious thing is wrong in four separate ways. It's worth doing wrong on purpose.

The instinct: a button starts a timer that decrements `remaining` every second.

``` elisp
;; DON'T DO THIS - shown to be dismantled below.
(vui-defcomponent pomodoro-timer ()
  :state ((remaining (* 25 60)))
  :render
  (vui-vstack
   (vui-text (pomodoro--format remaining) :face 'bold)
   (vui-button "Start"
     :on-click
     (lambda ()
       (run-with-timer 1 1
         (lambda ()
           (vui-set-state :remaining (1- remaining))))))))
```

Click Start and it falls apart. Let's count the ways, because each one teaches something.

**1. The callback runs without a component context.** `vui-set-state` has to know *which* component instance and *which* buffer it's updating. It learns that from a dynamic variable that vui binds only while it's calling your handlers and effects. `run-with-timer` fires its callback a second later, from Emacs's idle loop, with that context long gone. You get:

``` example
vui-set-state called outside of component context
```

**2. The closure captures a stale `remaining`.** Even if the context existed, `(1- remaining)` closes over the value of `remaining` *at the moment you clicked* - say `1500`. The timer fires every second forever computing `(1- 1500)` = `1499`. The clock would jump to `24:59` and stick there. Reading a state variable inside an async callback gives you a snapshot, not a live value.

**3. Clicking Start twice spawns two timers.** The handler has no memory of the timer it created. Click again and you have two timers fighting over `remaining`, counting down twice as fast. There is no handle to cancel either one.

**4. Nothing ever stops.** There's no pause, no cleanup, no off switch. When you eventually kill the buffer (more on that later), the timer keeps firing into the void.

Every one of these comes from the same mistake: treating "start a timer" as a one-off action triggered by a click, when it's really a piece of **ongoing behaviour tied to a piece of state** - "a timer should be running while we are in the running state." That reframing is the whole solution.

# Step 3: The Timer Belongs in an Effect

**Goal:** Run a timer **while** the component is in a running state, and stop it the instant it isn't. We do this with `vui-use-effect`.

An effect is a side effect that vui runs for you after render, keyed on a list of dependencies. It runs after the first render, and again whenever one of its dependencies changes. If the effect returns a function, vui calls that function as **cleanup** - before the effect re-runs, and on unmount. That cleanup is the missing off switch from Step 2.

Add a `running` flag, and let an effect own the timer:

<div class="tabs">

<div class="tab">

**Example**

``` elisp
(vui-defcomponent pomodoro-timer ()
  :state ((remaining (* 25 60))
          (running nil))
  :render
  (progn
    ;; While running, tick once per second. The effect re-runs only when
    ;; `running' flips; its cleanup cancels the timer when we pause or
    ;; when the component goes away.
    (vui-use-effect (running)
      (when running
        (let ((timer (run-with-timer
                      1 1
                      (vui-with-async-context
                       (vui-set-state :remaining
                                      (lambda (n) (max 0 (1- n))))))))
          (lambda () (cancel-timer timer)))))

    (vui-vstack
     (vui-text (pomodoro--format remaining) :face 'bold)
     (vui-button (if running "Pause" "Start")
       :on-click (lambda () (vui-set-state :running (not running)))))))
```

</div>

<div class="tab">

**Screenshot**

![](/images/2026-06-14-vui-pomodoro/pomodoro-step3-timer-effect.webp)

</div>

</div>

This small block fixes all four problems at once. Walk through it slowly, because the shape is the lesson.

**The effect is keyed on `(running)`.** It runs once after the first render (`running` is `nil`, so the body does nothing) and then again *only* when `running` changes. Flip `running` to `t` and the effect runs, starting a timer. Flip it back to `nil` and vui runs the cleanup from the previous run - cancelling the timer - before running the body again (which now does nothing). Start and stop, exactly once each, driven entirely by one boolean. No double timers.

**The body returns its cleanup.** `(lambda () (cancel-timer timer))` is the function vui will call to tear this effect down. Pausing triggers it. Re-rendering for any other reason does not, because `running` didn't change. This is the off switch we lacked.

**`vui-with-async-context` restores the context.** It captures the current buffer and component instance at effect time and returns a function that re-establishes them before running its body. That's what makes `vui-set-state` legal from inside `run-with-timer`. (It also quietly checks the buffer is still alive and the tree still mounted, and skips the body otherwise - so a stray late tick can't error.)

**The functional update kills the stale closure.** Instead of `(1- remaining)` we pass a **function** to `vui-set-state`:

``` elisp
(vui-set-state :remaining (lambda (n) (max 0 (1- n))))
```

When `vui-set-state` gets a function, it calls it with the *current* value and stores the result. `n` is always the live `remaining`, never a snapshot. The `(max 0 ...)` keeps us from counting into negative seconds. This is the same lexical-capture trap from the very first wall, wearing a different hat: a plain closure over `remaining` would tick off a value frozen in time. (Hold one thought, though: decrementing once per tick is itself the wrong model for a clock - the next step is entirely about why - but when you *do* keep a counter in state, this is how you update it from async code.)

There's a placement detail worth stating outright, because it's the part that isn't obvious: **the effect goes at the top of the render body, but its dependency is what controls it.** You're not putting the timer "where the button is" or "where the clock is" - you declare it once, up front, and let the `(running)` dependency decide when it actually starts and stops. Once you've seen it, it's the natural home for anything async. Until you've seen it, it's the single most confusing part of the whole library.

**What you learned:** Effects model ongoing behaviour tied to state. Key them on the flag that gates them, return cleanup, and reach for `vui-with-async-context` plus functional updates whenever the callback is asynchronous.

# Step 4: Counting Ticks Isn't Measuring Time

Let the clock from Step 3 run for a few minutes and check it against a real one. It's behind. Not by much at first, but it never catches up, and the gap only grows.

Here's why. `(1- n)` subtracts one **per tick** - so the clock is really counting how many times the timer fired, not how much time passed. Those aren't the same number. `run-with-timer` doesn't fire on a strict schedule; it only runs when Emacs is free, so a long command, garbage collection, or a busy redisplay pushes the tick late. Worse, when Emacs falls far enough behind, it *drops* the missed repeats instead of firing a catch-up burst - that's `timer-event-handler` protecting you from a storm of callbacks. A dropped tick is a second the counter never subtracts, and never gets back. So the clock runs slow, by an amount that depends on how busy Emacs happened to be. Fine for a toy; wrong for a timer you want to trust.

The fix is to stop counting and start **measuring**. Store when the session should end - a deadline - and on each tick compute the time left by subtracting *now* from it. The timer's job shrinks to "wake up often enough to refresh the display"; the number it shows always comes from the wall clock, so a late or dropped tick changes how smoothly the clock updates, not how much time it reports.

``` elisp
(defun pomodoro--seconds-until (deadline)
  "Whole seconds left until DEADLINE (a `float-time'), clamped at 0."
  (max 0 (ceiling (- deadline (float-time)))))

(vui-defcomponent pomodoro-timer ()
  :state ((remaining (* 25 60))
          ;; Absolute end time while running; nil when paused.
          (deadline nil))
  :render
  ;; "Running" is just "there's a deadline to count down to".
  (let ((running (and deadline t)))
    (progn
      ;; Tick: refresh `remaining' from the clock. The timer sets the
      ;; refresh rate; `deadline' sets the value.
      (vui-use-effect (deadline)
        (when deadline
          (let ((timer (run-with-timer
                        1 1
                        (vui-with-async-context
                         (vui-set-state :remaining
                                        (pomodoro--seconds-until deadline))))))
            (lambda () (cancel-timer timer)))))

      (vui-vstack
       (vui-text (pomodoro--format remaining) :face 'bold)
       (vui-button (if running "Pause" "Start")
         :on-click
         (lambda ()
           (if running
               ;; Pause: freeze what's left, then drop the deadline.
               (vui-batch
                (vui-set-state :remaining (pomodoro--seconds-until deadline))
                (vui-set-state :deadline nil))
             ;; Start: aim at a deadline `remaining' seconds from now.
             (vui-set-state :deadline (+ (float-time) remaining)))))))))
```

Three changes worth naming:

- **The stored state is the deadline, not a flag.** `remaining` is still what we display, but while the clock runs the source of truth is `deadline`. "Running" is now *derived* - `(and deadline t)` - so the boolean can't drift out of sync with whether there's actually a clock to run. Store the most fundamental fact; compute the rest.
- **The effect is keyed on `deadline`.** It starts a timer when a deadline appears and cancels it when one goes away. The once-a-second `vui-set-state :remaining` updates don't restart it, because `deadline` itself doesn't change while the clock runs.
- **Start and Pause just move the deadline.** Start anchors one `remaining` seconds out; Pause freezes the computed remaining and clears the deadline so `remaining` carries the value across the pause. Resuming re-anchors from there.

The `ceiling` is a small nicety: it keeps a freshly started clock showing its full duration for a whole second before the first tick down, rather than dropping to `24:59` the instant you hit Start.

**What you learned:** For anything time-based, store a timestamp and measure against the clock. Counting events - ticks, frames, callbacks - drifts, because what you actually care about is elapsed time, not how many times something fired.

# Step 5: Work and Break, Derived Not Driven

**Goal:** When the clock hits zero, switch between a 25-minute work session and a 5-minute break, and count completed work sessions.

Here's the next tempting wrong turn: do the switch inside the tick. You're already updating `remaining` in the timer callback - why not check for zero there and flip the mode?

Because then the transition logic lives wherever the countdown happens to be driven from. Add a "Skip" button later and you'd have to duplicate the switch there too. Mix it into the tick and a single second's callback is suddenly responsible for arithmetic, session policy, and a counter.

The cleaner model: the transition is a **consequence of `remaining` reaching zero**, no matter what drove it there. So make it a second effect, keyed on `remaining`:

<div class="tabs">

<div class="tab">

**Example**

``` elisp
(defun pomodoro--duration (mode)
  "Session length in seconds for MODE (`work' or `break')."
  (pcase mode
    ('work  (* 25 60))
    ('break (* 5 60))))

(vui-defcomponent pomodoro-timer ()
  :state ((mode 'work)
          (remaining (pomodoro--duration 'work))
          (deadline nil)
          (completed 0))
  :render
  (let ((running (and deadline t)))
    (progn
      ;; Tick (the timestamp version from Step 4).
      (vui-use-effect (deadline)
        (when deadline
          (let ((timer (run-with-timer
                        1 1
                        (vui-with-async-context
                         (vui-set-state :remaining
                                        (pomodoro--seconds-until deadline))))))
            (lambda () (cancel-timer timer)))))

      ;; Transition: when the countdown hits zero, switch sessions.
      ;; Derived from `remaining', so it's correct no matter what drove
      ;; the clock to zero - the tick, or a Skip button. It re-anchors
      ;; `deadline' too, so the next session starts measuring from now.
      (vui-use-effect (remaining)
        (when (and running (zerop remaining))
          (let* ((next (if (eq mode 'work) 'break 'work))
                 (secs (pomodoro--duration next)))
            (vui-batch
             (vui-set-state :mode next)
             (vui-set-state :remaining secs)
             (vui-set-state :deadline (+ (float-time) secs))
             (when (eq mode 'work)
               (vui-set-state :completed #'1+))))))

      (vui-vstack
       (vui-text (if (eq mode 'work) "Work" "Break")
         :face (if (eq mode 'work) 'success 'warning))
       (vui-text (pomodoro--format remaining) :face 'bold)
       (vui-button (if running "Pause" "Start")
         :on-click
         (lambda ()
           (if running
               (vui-batch
                (vui-set-state :remaining (pomodoro--seconds-until deadline))
                (vui-set-state :deadline nil))
             (vui-set-state :deadline (+ (float-time) remaining)))))))))
```

</div>

<div class="tab">

**Screenshot**

![](/images/2026-06-14-vui-pomodoro/pomodoro-step4-work-break.webp)

</div>

</div>

The transition effect watches `remaining`. The moment it hits zero (and we're running), it computes the next mode, re-anchors the clock - both `remaining` and a fresh `deadline` for that mode's duration - and, if a work session just finished, bumps the counter. The updates are wrapped in `vui-batch` so they collapse into a single re-render instead of four.

Note `(vui-set-state :completed #'1+)` - another functional update, the tidy way to say "increment whatever's there now."

The payoff for deriving rather than driving shows up in the next step: "Skip" becomes a one-liner.

**What you learned:** Derive transitions from the state that triggers them, not from the code that happened to change that state. Batch related updates.

# Step 6: Controls and a Progress Bar

**Goal:** Round it out - Pause/Reset/Skip, a progress gauge, and the completed count.

First a small child component for the gauge. It owns no state; it's a pure function of `mode` and `remaining`:

``` elisp
(vui-defcomponent pomodoro-progress (mode remaining)
  :render
  (let* ((total (pomodoro--duration mode))
         (width 40)
         (done (if (> total 0)
                   (round (* width (/ (float (- total remaining)) total)))
                 0))
         (done (max 0 (min width done))))
    (vui-text (concat "[" (make-string done ?#)
                      (make-string (- width done) ?-) "]")
      :face (if (eq mode 'work) 'success 'warning))))
```

Now the controls. This is where deriving the transition pays off - watch how trivial Skip is:

``` elisp
(vui-hstack
 :spacing 2
 (vui-button (if running "Pause" "Start")
   :on-click
   (lambda ()
     (if running
         (vui-batch
          (vui-set-state :remaining (pomodoro--seconds-until deadline))
          (vui-set-state :deadline nil))
       (vui-set-state :deadline (+ (float-time) remaining)))))
 (vui-button "Reset"
   :on-click (lambda ()
               (vui-batch
                (vui-set-state :deadline nil)
                (vui-set-state :remaining (pomodoro--duration mode)))))
 (vui-button "Skip"
   :face 'font-lock-comment-face
   :on-click (lambda ()
               ;; Jump to the end; the transition effect does the rest.
               (vui-set-state :remaining 0))))
```

Skip doesn't know anything about work, break, or counters. It sets `remaining` to `0` and the transition effect from Step 5 takes over - because that effect is keyed on `remaining`, not on "the tick fired." One source of truth for what happens at zero.

Drop the gauge and the stats into the layout and you have the whole app:

<div class="tabs">

<div class="tab">

**Example**

``` elisp
(vui-vstack
 (vui-text "Pomodoro Timer" :face 'outline-1)
 (vui-newline)
 (vui-text (if (eq mode 'work) "Work" "Break")
   :face (if (eq mode 'work) 'success 'warning))
 (vui-text (pomodoro--format remaining) :face 'bold)
 (vui-component 'pomodoro-progress :mode mode :remaining remaining)
 (vui-newline)
 ;; ...the vui-hstack of controls from above...
 (vui-newline)
 (vui-text (format "Completed pomodoros: %d" completed)
   :face 'font-lock-comment-face))
```

</div>

<div class="tab">

**Screenshot**

![](/images/2026-06-14-vui-pomodoro/pomodoro-step5-full-app.webp)

</div>

</div>

The full file - with the demo command and the bits I've elided here - is [`docs/examples/10-pomodoro.el`](https://github.com/d12frosted/vui.el/blob/master/docs/examples/10-pomodoro.el). Run it with `M-x vui-example-pomodoro`.

**What you learned:** Pure child components for presentation, and how a well-placed derived effect makes new controls cheap.

# Step 7: The Bug I Found by Killing the Buffer

This is the part that turned "write an example" into "fix the library."

I had the timer running, switched buffers, and killed the Pomodoro buffer with `C-x k` like you'd kill any buffer. Then, every second, my echo area kept blinking. The timer was still alive. Its UI was gone - but the `run-with-timer` I'd started in the effect was firing into a buffer that no longer existed.

The effect *had* a cleanup. `(lambda () (cancel-timer timer))` was right there. The problem was that nothing called it. vui ran effect cleanups when a component was removed during a **re-render**, and on explicit unmount - but killing the buffer out from under a mounted tree went through neither path. No cleanup, no `on-unmount` hooks, no `on-mount` teardown. Timers and processes held by components simply leaked. The exact timer pattern I'd just written up as the recommended approach was the thing that broke.

The fix: at mount time, vui now installs a buffer-local `kill-buffer-hook` that runs the same full teardown as [`vui-unmount`](https://github.com/d12frosted/vui.el/blob/master/vui.el) - every effect cleanup, every unmount hook, depth-first. Kill the buffer and the timer dies with it. (If you've already called `vui-unmount` explicitly, it doesn't run twice.)

``` elisp
;; Explicit teardown, when you want it:
(vui-unmount)            ; current buffer's tree

;; Implicit teardown, now automatic:
(kill-buffer "*vui-pomodoro*")   ; runs the same cleanup
```

The lesson is an old one: dogfooding finds what tests don't. I had lifecycle tests. None of them killed a buffer with a live timer in it, because I'd never thought of the buffer's death as an unmount. Watching the echo area flash after the UI was gone made it obvious in a way no test had.

# A Smaller Snag: Faces vs. Face Specs

One more snag worth a mention, smaller than the rest - mostly because the answer surprised me too. Say you want a piece of text to be a bolder version of the `error` face, and you'd rather not `defface` a whole new face just for that, so you try something inline. The confusion is fair: in Emacs a *face* (a symbol like `'error`) and the anonymous *face spec* you might write inline look similar but aren't interchangeable.

Here's the part I had to rediscover myself: `:face` already accepts an anonymous spec. vui passes the value straight to Emacs's `face` text property, and Emacs is happy to take a plist of attributes there. So you can derive from an existing face without `defface` at all:

``` elisp
;; a face symbol
(vui-text "time's up" :face 'error)

;; inherit error, override one attribute - works today
(vui-text "time's up" :face '(:inherit error :weight ultra-bold))

;; plain attributes, no inheritance
(vui-text "time's up" :face '(:foreground "red" :weight bold))
```

Two traps to remember, both of which cost me a few minutes:

- **Colours are strings.** `:foreground "red"`, not `:foreground red`. The symbol form just silently does nothing - this was the actual snag, not the spec idea itself.
- **Use `:inherit` for the bare-symbol case.** `(error :weight ultra-bold)` does *not* work - Emacs reads that as a list of three faces (`error`, `:weight`, `ultra-bold`). Spell it `(:inherit error :weight ultra-bold)`.

So there's nothing to build here, just something to document - tracked as [issue \#77](https://github.com/d12frosted/vui.el/issues/77). A good reminder that "the library should support this" is sometimes really "the library already does, and I never wrote it down."

# What This Example Taught Me

The genuinely hard part of vui.el isn't components or layout - it's the seam where **async meets state**. Timers, processes, network calls: anything that fires later, outside the render pass. Three moves carry almost all of it:

1.  **Model behaviour as an effect keyed on the state that gates it.** "Run a timer while there's a deadline" becomes `(vui-use-effect (deadline) ...)` with cleanup. The dependency, not the click, decides when it starts and stops.
2.  **In async callbacks, restore context and update functionally.** `vui-with-async-context` so `vui-set-state` is legal, and `(lambda (n) ...)` so you act on live state instead of a stale snapshot.
3.  **For anything time-based, measure against the clock - don't count events.** Store a deadline and recompute the time left; a counter that ticks once per timer firing drifts, because timers fire late and Emacs drops the misses.

All three are easy once you've seen them and genuinely confusing before. That gap is the whole reason this post exists - and the reason watching someone hit it on stream was so valuable.

If you build something with vui.el and hit a wall, I'd love to hear about it - that's how the buffer-kill bug got fixed, and how [\#77](https://github.com/d12frosted/vui.el/issues/77) got written. Open an issue. I built this for myself, but it's better when it's not just for me.

And again - David, thank you for the stream. It made the library better.
