Since Spacemacs officially supports at least two major versions of Emacs, as a collaborator I have to switch between Emacs versions from time to time. Since installed packages don't usually work between different major versions of Emacs, I came up with a hack that allows me to seamlessly switch Emacs versions.

## Change Log

- **\[2016-10-11\]:** Thanks to [bmag](https://github.com/bmag), Spacemacs [now has an option](https://github.com/syl20bnr/spacemacs/pull/5410) to set the packages directory based on Emacs version. Simply set `dotspacemacs-elpa-subdirectory` to `emacs-version` and enjoy seamless version switching. This option is available starting with Spacemacs v0.200.

<!--more-->

Usually when I need to switch Emacs versions, it means I have to completely nuke the `~/.emacs.d/elpa` directory and wait several minutes to install 300+ packages. This isn't critical, but there's room for improvement. Thanks to [bmag](https://github.com/bmag), it's now [possible](https://github.com/syl20bnr/spacemacs/pull/5410) to easily change the directory where all packages are installed (without any dirty hacks). I use separate directories for each Emacs version, so packages are only downloaded the first time I use a specific version. On subsequent switches, everything is ready to use.

``` commonlisp
;; setup package-user-dir to allow seamless switching between Emacs versions
(setq package-user-dir (file-name-as-directory
                        (concat user-emacs-directory "elpa/" emacs-version)))
```

Add this expression to the `dotspacemacs/user-init` function in your `.spacemacs` to make switching between versions easier.
