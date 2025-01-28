Since Spacemacs officially supports at least two major versions of Emacs, as a collaborator I have to switch between Emacs versions time to time. And since installed packages don't usually work between different major version of Emacs, I came up with a hack allowing me to seamlessly switch Emacs versions.

**Update on \[2016-10-11 Tue\]:** thanks to [bmag](https://github.com/bmag) Spacemacs [has an option](https://github.com/syl20bnr/spacemacs/pull/5410) to set packages directory based on Emacs version. Just set `dotspacemacs-elpa-subdirectory` to `emacs-version` and be happy about it. This option is available starting with Spacemacs v0.200.

<!--more-->

Usually when I need to switch Emacs version, it means that I have to totally nuke `~/.emacs.d/elpa` directory and wait for several minutes in order to install 300+ packages. This is not critical, but there is room for improvement. Thanks to [bmag](https://github.com/bmag) now it is [possible](https://github.com/syl20bnr/spacemacs/pull/5410) to easily (without any dirty hacks) to change directory where all packages are installed. I use separate directories for every Emacs version. So packages are downloaded only the first time I use specific version. But for the next time everything will be ready for use.

``` commonlisp
;; setup package-user-dir to allow seamless switch between emacs versions
(setq package-user-dir (file-name-as-directory
                        (concat user-emacs-directory "elpa/" emacs-version)))
```

Add this expression to `dotspacemacs/user-init` function in your `.spacemacs` and make it easier to switch between versions.
