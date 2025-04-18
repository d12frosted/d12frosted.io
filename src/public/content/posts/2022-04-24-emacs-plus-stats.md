Over time [Emacs Plus](https://github.com/d12frosted/homebrew-emacs-plus) has grown into quite heavy [homebrew](https://brew.sh) formula supporting 4 Emacs versions (starting with Emacs 26), 13 compilation options and 53 icons. For a long time I was accepting new options, patches and icons. Now in the process of rethinking maintenance approach I decided to take a look at how people are using this formula. Luckily, `brew` provides public [analytics](https://formulae.brew.sh/docs/api/#analytics) even for taps, which is being sent by users unless they decide to disable it.

Of course, it's not covering 100% of `emacs-plus` user base, but I still found it interesting and since I might change few things in `emacs-plus` based on this, I thought that other might be interested in it as well. Enjoy.

<!--more-->

# Versions

| version       | 30 days  | 90 days  | 365 days |
|---------------|----------|----------|----------|
| emacs-plus@29 | 559      | 1381     | 2256     |
| emacs-plus@28 | **2344** | **2703** | **5764** |
| emacs-plus@27 | 523      | 602      | 3737     |
| emacs-plus@26 | 29       | 77       | 496      |

Since Emacs 28 was released recently™, there is little wonder that it's being the most popular version among `emacs-plus` users. What interests me is that people are still using Emacs 26. Since it has zero footprint on maintenance, I am not going to remove older version, though I urge you to switch to latest version.

# Options

| option (365 days)      | emacs-plus@27 | emacs-plus@28 | emacs-plus@29 |
|------------------------|---------------|---------------|---------------|
| –HEAD                  | 919           | 0             | 0             |
| –with-ctags            | 0             | 125           | 0             |
| –with-dbus             | 0             | 72            | 0             |
| –with-imagemagick      | 0             | 308           | 279           |
| –with-mailutils        | 0             | 147           | 84            |
| –with-native-comp      | 0             | 4753          | 1213          |
| –with-no-frame-refocus | 0             | 364           | 93            |
| –with-no-titlebar      | 317           | 811           | 77            |
| –with-x11              | 63            | 0             | 0             |
| –with-xwidgets         | 170           | 1816          | 481           |
| –without-cocoa         | 32            | 41            | 0             |
| –without-imagemagick   | 40            | 157           | 0             |

Interestingly, all available options are used, even if some are used rarely. And all of them require some analysis.

For example, `--with-x11` and `--without-cocoa` are not straightforward from the formula point of view and they add some complexity. Since they are being rarely used, I might deprecate them in `emacs-plus@29`.

Other options, like `--with-mailutils`, `--with-ctags` and `--with-dbus` are straightforward and add no complexity, though they are rarely used. So I might provide a different way of enabling such options.

Obviously `--with-native-comp` and `--with-xwidgets` will stay with us, even though the last one is known source of troubles. This is the case where added value is bigger than maintenance burden.

You may notice that Emacs 26 is not mentioned in this table, but this is simply because of issue [\#195](https://github.com/d12frosted/homebrew-emacs-plus/issues/195).

# Icons

| icon                            | installs |
|---------------------------------|----------|
| modern-doom3                    | 1563     |
| nobu417-big-sur                 | 1499     |
| spacemacs                       | 1401     |
| elrumo2                         | 682      |
| modern-sexy-v2                  | 304      |
| modern-papirus                  | 263      |
| modern                          | 179      |
| modern-doom                     | 168      |
| modern-sexy-v1                  | 145      |
| modern-vscode                   | 138      |
| modern-cg433n                   | 130      |
| modern-black-variant            | 123      |
| cacodemon                       | 116      |
| modern-purple-flat              | 109      |
| elrumo1                         | 93       |
| modern-black-dragon             | 87       |
| modern-paper                    | 45       |
| modern-sjrmanning               | 40       |
| modern-nuvola                   | 38       |
| emacs-card-british-racing-green | 38       |
| retro-gnu-meditate-levitate     | 36       |
| modern-alecive-flatwoken        | 36       |
| modern-orange                   | 34       |

Only 23 icons out of 53 are present here, and I am 100% sure some are missing from analytics, but it's nice to know the top 3 icons used by our community.

In any case, I have [simplified](https://github.com/d12frosted/homebrew-emacs-plus/blob/328a0beee56a4f099f9a6eb31290223238dd24bc/iconset) the process of maintaining available icons and I might also provide a way to (easily?) install `emacs-plus` with any icon, even those that are not part of `emacs-plus` repository. If that works, I will cleanup icons a little bit based on usage stats and my personal preferences.

# So what are the plans?

First of all, I am not going to remove any option without informing you in all possible ways (message in the build log and issue/PR in `emacs-plus` repository, as I did with `--with-no-titlebar` option [here](https://github.com/d12frosted/homebrew-emacs-plus/pull/434) and [here](https://github.com/d12frosted/homebrew-emacs-plus/pull/435)). If you find out that I am going to remove an option that you are using - just let me know, either via email or GitHub issues! `emacs-plus` still exists only because of its user base. And I want to support your needs to possible extent.

Secondly, I am thinking about providing a wrapper to install `emacs-plus` with stuff not support by this formula. Nothing fancy, just simple options like `--with-dbus` and local icons. Unfortunately, `brew` seems not powerful enough for this task, so I will have to investigate that a bit.

No ETA, as always, but stay tuned. Safe travels!
