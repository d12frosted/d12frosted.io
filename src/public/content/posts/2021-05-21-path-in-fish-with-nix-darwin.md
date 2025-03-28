During migration to `nix` for package and system management in [environment#11](https://github.com/d12frosted/environment/pull/11), I've encountered an issue with `PATH` variable containing seemingly correct entries, but in incorrect order when using `fish`. Basically, `$HOME/.nix-profile/bin` is put in the end. Since I am very new to `nix` ecosystem (using it for few days), it was not clear what is causing this issue (my configuration, `nix-home-manager`, `nix-darwin` or `fish` itself), so I decided to investigate. While it turned out to be a [known issue](https://github.com/LnL7/nix-darwin/issues/122), I learned a little bit in the process and found a local fix, which I am sharing in the end of the post.

<!--more-->

When `bash` is set as user shell, the value of `PATH` is good expect for repeating values.

``` bash
$ echo $PATH
/Users/d12frosted/.nix-profile/bin: \
  /nix/var/nix/profiles/default/bin: \
  /usr/local/bin: \
  /usr/bin: \
  /bin: \
  /usr/sbin: \
  /sbin: \
  /Library/TeX/texbin: \
  /usr/local/MacGPG2/bin: \
  /opt/X11/bin: \
  /Library/Apple/usr/bin: \
  /Users/d12frosted/.nix-profile/bin: \
  /run/current-system/sw/bin: \
  /nix/var/nix/profiles/default/bin
```

The important part here is `$HOME/.nix-profile/bin` being the first item in this list, which is expected and desired, because we want binaries installed via package manager (in this case `nix`) to shadow any built-in binaries (common with `coreutils` package).

But when `fish` is used as user shell, the list doesn't contain any duplicates, but `$HOME/.nix-profile/bin` and `/nix/var/nix/profiles/default/bin` are placed in the end.

``` fish
$ echo $PATH
/usr/local/bin \
  /usr/bin \
  /bin \
  /usr/sbin \
  /sbin \
  /opt/X11/bin \
  /Library/Apple/usr/bin \
  /usr/local/MacGPG2/bin \
  /Library/TeX/texbin \
  /Users/d12frosted/.nix-profile/bin \
  /run/current-system/sw/bin \
  /nix/var/nix/profiles/default/bin
```

And I become curious about reasons behind difference of these values and possible solution. Since I am very new to `nix` ecosystem, I decided to start with something more familiar - `fish`. It turns out, that it's possible to debug variable modifications with `fish` by using [event handlers](https://fishshell.com/docs/current/index.html#event), which we can put somewhere in the very beginning of [initialization](https://fishshell.com/docs/current/index.html#initialization) process, which is `$__fish_data_dir/config.fish` - configuration file shipped with `fish` itself that is loaded first. In general no one should ever modify this file, but we are debugging, so it's fine. I also use [status](https://fishshell.com/docs/current/cmds/status.html#cmd-status) function to display extra information (mostly interested in stack trace).

``` fish
# Add these lines to the very beginning of $__fish_data_dir/config.fish
# /nix/store/r5brs3gn4amxbl1mrl4433inlghwl1r0-fish-3.2.2/share/fish/config.fish
echo "PATH before initialisation > $PATH"

function __notice_path_change -d "Notice PATH changes" --on-variable PATH
  echo "PATH has changed to $PATH"
  status
end
```

After firing a new `fish` session, I see the following output in terminal emulator.

``` example
PATH before initialisation > /Users/d12frosted/.nix-profile/bin:/run/current-system/sw/bin:/nix/var/nix/profiles/default/bin:/usr/local/bin:/usr/bin:/usr/sbin:/bin:/sbin:/Users/d12frosted/.config/bin:/Users/d12frosted/.local/bin
PATH has changed to /usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin:/opt/X11/bin:/Library/Apple/usr/bin:/usr/local/MacGPG2/bin:/Library/TeX/texbin:/Users/d12frosted/.nix-profile/bin:/run/current-system/sw/bin:/nix/var/nix/profiles/default/bin:/Users/d12frosted/.config/bin:/Users/d12frosted/.local/bin
This is a login shell
Job control: Only on interactive jobs
in function '__notice_path_change' with arguments 'VARIABLE SET PATH'
  called on line 1 of file /nix/store/r5brs3gn4amxbl1mrl4433inlghwl1r0-fish-3.2.2/share/fish/config.fish
in event handler: handler for variable “PATH”
  called on line 198 of file /nix/store/r5brs3gn4amxbl1mrl4433inlghwl1r0-fish-3.2.2/share/fish/config.fish
PATH has changed to /usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin:/opt/X11/bin:/Library/Apple/usr/bin:/usr/local/MacGPG2/bin:/Library/TeX/texbin:/Users/d12frosted/.nix-profile/bin:/run/current-system/sw/bin:/nix/var/nix/profiles/default/bin:/Users/d12frosted/.config/bin:/Users/d12frosted/.local/bin
This is a login shell
Job control: Only on interactive jobs
in function '__notice_path_change' with arguments 'VARIABLE SET PATH'
  called on line 1 of file /nix/store/r5brs3gn4amxbl1mrl4433inlghwl1r0-fish-3.2.2/share/fish/config.fish
in event handler: handler for variable “PATH”
  called on line 129 of file /nix/store/r5brs3gn4amxbl1mrl4433inlghwl1r0-fish-3.2.2/share/fish/config.fish
Welcome to fish, the friendly interactive shell
Type help for instructions on how to use fish

[17:37:38] @MacBook-Pro ~
λ
```

So as you can see, the value of `PATH` is almost correct before `fish` is loaded. The value is in correct order, but lacks few entries. And then it gets the missing entries, but also gets reordered on line 198 of `$__fish_data_dir/config.fish`.

Turns out, `fish` mimics behaviour of `path_helper` macOS (BSD) utility, which makes sure that entries from `/etc/paths` file and all files under `/etc/paths.d/` are present in the `PATH`.

``` example
path_helper(8)                       Nixpkgs System Manager's Manual                       path_helper(8)

NAME
     path_helper — helper for constructing PATH environment variable

SYNOPSIS
     path_helper [-c | -s]

DESCRIPTION
     The path_helper utility reads the contents of the files in the directories /etc/paths.d and
     /etc/manpaths.d and appends their contents to the PATH and MANPATH environment variables respec‐
     tively.  (The MANPATH environment variable will not be modified unless it is already set in the en‐
     vironment.)

     Files in these directories should contain one path element per line.

     Prior to reading these directories, default PATH and MANPATH values are obtained from the files
     /etc/paths and /etc/manpaths respectively.

     Options:

     -c      Generate C-shell commands on stdout.  This is the default if SHELL ends with "csh".

     -s      Generate Bourne shell commands on stdout.  This is the default if SHELL does not end with
             "csh".

NOTE
     The path_helper utility should not be invoked directly.  It is intended only for use by the shell
     profile.

Mac OS X                                      March 15, 2007                                     Mac OS X
```

And this is how it's implemented in `fish` (inside `$__fish_data_dir/config.fish`):

``` fish
#
# Some things should only be done for login terminals
# This used to be in etc/config.fish - keep it here to keep the semantics
#
if status --is-login
    if command -sq /usr/libexec/path_helper
        # Adapt construct_path from the macOS /usr/libexec/path_helper
        # executable for fish; see
        # https://opensource.apple.com/source/shell_cmds/shell_cmds-203/path_helper/path_helper.c.auto.html .
        function __fish_macos_set_env -d "set an environment variable like path_helper does (macOS only)"
            set -l result

            # Populate path according to config files
            for path_file in $argv[2] $argv[3]/*
                if [ -f $path_file ]
                    while read -l entry
                        if not contains -- $entry $result
                            test -n "$entry"
                            and set -a result $entry
                        end
                    end <$path_file
                end
            end

            # Merge in any existing path elements
            for existing_entry in $$argv[1]
                if not contains -- $existing_entry $result
                    set -a result $existing_entry
                end
            end

            set -xg $argv[1] $result
        end

        __fish_macos_set_env PATH /etc/paths '/etc/paths.d'
        if [ -n "$MANPATH" ]
            __fish_macos_set_env MANPATH /etc/manpaths '/etc/manpaths.d'
        end
        functions -e __fish_macos_set_env
    end

    # ...
end
```

In short, it constructs a list of entries from `/etc/paths` file plus files from `/etc/paths.d` and appends to the result all missing entries from `PATH` variable. Since `$HOME/.nix-profile/bin` is not in `/etc/paths`, it is added to the end of the result.

I am not sure why this mechanism exists in the first place, I suspect that it's needed for building proper `PATH` during system loading and for operation of macOS applications (which is a constant source of confusion). If anyone knows more, please share your knowledge via comments or email, I will include better explanations instead of my speculations.

While we learned the reason this value is incorrect, it's still unclear how and by whom `PATH` is fixed when using `bash` and how to fix it in `fish`.

By quick inspection of contents of `/run/current-system` and `/run/current-system/etc`, I find an interesting file `/run/current-system/etc/bashrc`.

``` example
λ la /run/current-system/
total 68K
dr-xr-xr-x    15 root wheel   480 Jan  1  1970 .
drwxrwxr-t 11741 root nixbld 367K May 20 09:14 ..
lrwxr-xr-x     1 root wheel    76 Jan  1  1970 Applications -> /nix/store/4w1af25hb32hqd31sh7pwm4vd00dpzw2-system-applications/Applications
dr-xr-xr-x     5 root wheel   160 Jan  1  1970 Library
-r-xr-xr-x     1 root wheel   40K Jan  1  1970 activate
-r-xr-xr-x     1 root wheel  6.9K Jan  1  1970 activate-user
dr-xr-xr-x     2 root wheel    64 Jan  1  1970 darwin
-r--r--r--     1 root wheel  4.2K Jan  1  1970 darwin-changes
-r--r--r--     1 root wheel    38 Jan  1  1970 darwin-version
lrwxr-xr-x     1 root wheel    51 Jan  1  1970 etc -> /nix/store/5069ikh9adm1m98fjxisgp6m7bn5jzwa-etc/etc
lrwxr-xr-x     1 root wheel    59 Jan  1  1970 patches -> /nix/store/l4dwcgs0zqh5z6b2b4z1wax4fwamg5fg-patches/patches
lrwxr-xr-x     1 root wheel    55 Jan  1  1970 sw -> /nix/store/jj97rcxh8z2fnn45bcd9xwm08xi3vdcy-system-path
-r--r--r--     1 root wheel    13 Jan  1  1970 system
-r--r--r--     1 root wheel    96 Jan  1  1970 systemConfig
dr-xr-xr-x     3 root wheel    96 Jan  1  1970 user
```

``` example
λ la /run/current-system/etc/
total 0
dr-xr-xr-x 9 root wheel 288 Jan  1  1970 .
dr-xr-xr-x 3 root wheel  96 Jan  1  1970 ..
lrwxr-xr-x 1 root wheel  54 Jan  1  1970 bashrc -> /nix/store/b17sn0hfampy7fl1y0lf7nbckv2gfyvb-etc-bashrc
dr-xr-xr-x 5 root wheel 160 Jan  1  1970 fish
dr-xr-xr-x 4 root wheel 128 Jan  1  1970 nix
lrwxr-xr-x 1 root wheel  54 Jan  1  1970 shells -> /nix/store/dyprd01kgm00asrnd7dv0rdmg1fk8855-etc-shells
lrwxr-xr-x 1 root wheel  54 Jan  1  1970 skhdrc -> /nix/store/dd9hd30wlgbv4f2qfp1v863wm2wi8pkk-etc-skhdrc
dr-xr-xr-x 3 root wheel  96 Jan  1  1970 ssh
dr-xr-xr-x 3 root wheel  96 Jan  1  1970 ssl
```

``` bash
# content of /run/current-system/etc/bashrc
# /etc/bashrc: DO NOT EDIT -- this file has been generated automatically.
# This file is read for interactive shells.

[ -r "/etc/bashrc_$TERM_PROGRAM" ] && . "/etc/bashrc_$TERM_PROGRAM"

# Only execute this file once per shell.
if [ -n "$__ETC_BASHRC_SOURCED" -o -n "$NOSYSBASHRC" ]; then return; fi
__ETC_BASHRC_SOURCED=1

# Don't execute this file when running in a pure nix-shell.
if test -n "$IN_NIX_SHELL"; then return; fi

if [ -z "$__NIX_DARWIN_SET_ENVIRONMENT_DONE" ]; then
  . /nix/store/arcg1b2dbhmhj31xnm2f4xxgfsrzpnph-set-environment
fi

# Return early if not running interactively, but after basic nix setup.
[[ $- != *i* ]] && return

# Make bash check its window size after a process completes
shopt -s checkwinsize

# Read system-wide modifications.
if test -f /etc/bash.local; then
  source /etc/bash.local
fi
```

As you can see, it sources `/nix/store/arcg1b2dbhmhj31xnm2f4xxgfsrzpnph-set-environment` file, which basically makes sure that `$HOME/.nix-profile/bin` is at the beginning of `PATH`:

``` bash
# content of /nix/store/arcg1b2dbhmhj31xnm2f4xxgfsrzpnph-set-environment
# Prevent this file from being sourced by child shells.
export __NIX_DARWIN_SET_ENVIRONMENT_DONE=1

export PATH=$HOME/.nix-profile/bin:/run/current-system/sw/bin:/nix/var/nix/profiles/default/bin:/usr/local/bin:/usr/bin:/usr/sbin:/bin:/sbin
export EDITOR="nano"
export NIX_PATH="ssh-auth-sock=/Users/d12frosted/.config/gnupg/S.gpg-agent.ssh:ssh-config-file=/Users/d12frosted/.config/.ssh/config"
export NIX_SSL_CERT_FILE="/etc/ssl/certs/ca-certificates.crt"
export PAGER="less -R"
export XDG_CONFIG_DIRS="$HOME/.nix-profile/etc/xdg:/run/current-system/sw/etc/xdg:/nix/var/nix/profiles/default/etc/xdg"
export XDG_DATA_DIRS="$HOME/.nix-profile/share:/run/current-system/sw/share:/nix/var/nix/profiles/default/share"

# Extra initialisation
# reset TERM with new TERMINFO available (if any)
export TERM=$TERM

export NIX_USER_PROFILE_DIR="/nix/var/nix/profiles/per-user/$USER"
export NIX_PROFILES="/nix/var/nix/profiles/default /run/current-system/sw $HOME/.nix-profile"

# Set up secure multi-user builds: non-root users build through the
# Nix daemon.
if [ ! -w /nix/var/nix/db ]; then
    export NIX_REMOTE=daemon
fi

~
```

So it seems that `nix-darwin` is fixing `PATH` for `bash`, but it doesn't fix `PATH` for `fish`. While the issue is not fixed in the upstream, it's easy to fix it locally by adding required values in `programs.fish.shellInit`.

Since I didn't want to mess too much with specific values, instead, I simply remember the original value of `PATH` before `fish` reconstructed its path and then in my user `init` code I fix the order like this:

``` nix
programs = {
  fish.enable = true;
  fish.shellInit = ''
__nixos_path_fix
  '';
};

# see https://github.com/LnL7/nix-darwin/issues/122
environment.etc."fish/nixos-env-preinit.fish".text = lib.mkMerge [
  (lib.mkBefore ''
set -g __nixos_path_original $PATH
    '')
  (lib.mkAfter ''
function __nixos_path_fix -d "fix PATH value"
set -l result (string replace '$HOME' "$HOME" $__nixos_path_original)
for elt in $PATH
  if not contains -- $elt $result
    set -a result $elt
  end
end
set -g PATH $result
end
 '')
];
```

Rebuild and enjoy `coreutils` and alike!

Safe travels.
