**Important note:** In the years since I wrote this post, the Haskell community has made tremendous progress in fighting **Cabal Hell**. Thanks to [Stack](https://haskell.fpcomplete.com/get-started) and [Nix-style Local Builds](https://cabal.readthedocs.io/en/latest/nix-local-build-overview.html), the solution I describe below is no longer necessary. I'm keeping this post for historical reference (and to keep my blog from looking abandoned).

—

Perhaps I did something terribly wrong in a past life, because I constantly run into **Cabal Hell**. That feeling of powerlessness is simultaneously refreshing and depressing. I hate it most when I simply need to install an executable from Hackage, like `pandoc`.

But we're software engineers, after all. So I decided to write a helper script to avoid world destruction and get the desired executable into my `$PATH`.

**What you'll learn:**

- Understanding Cabal Hell and its causes
- Using Cabal sandboxes to isolate dependencies
- Installing Haskell executables without polluting your global package database
- Automating the process with Fish shell functions

<!--more-->

> **Cabal Hell**
>
> The feeling of powerlessness one has when Cabal does not do what one wanted and one does not know how to fix it.
>
> Well Typed

> **What is the difficulty caused by Cabal-install?**
>
> The main difficulty with Cabal is otherwise known as 'dependency hell', in which cabal-install fails to install a desired package for one reason or another, leading to extensive manual work. As an example of this difficulty, consider a case where the user wishes to install packages A and B. Both work with package C, but not with the same version of C.
>
> [Haskell Wiki](https://wiki.haskell.org/Cabal/Survival)

<img src="/images/2015-04-05-cabal-and-executables/2022-07-19-17-49-57-1428233775.webp" class="d12-image-1/2" />

I need to confess: sometimes I solve **Cabal Hell** by using the `rm -rf` method. **Cabal Hell** is like a disease - it's very hard to cure without ruining your environment (in this case, your package database). However, there is some good news: you can use tools to prevent this bizarre situation from happening. For these purposes, you can use Cabal sandboxes, Stackage, or NixOS. There are probably other handy solutions, but these are the ones I'm familiar with.

`Stackage` is excellent, but it doesn't work very well for me because sometimes I need to install 'heavy' packages that aren't on `Stackage`. Also, I work on a reliably fast computer, so I don't mind spending an extra thirty seconds on compilation - safety is more important. As for `NixOS`, I haven't tried it yet, but I know it's very good at finding compilation problems. Many thanks to the people who created `Stackage` and `NixOS`.

# Sandboxes

I think sandboxes are brilliant. Usually, I only install commonly used packages globally. Everything else goes through sandboxes. Sometimes the project I'm working on has dependencies that can't be installed from Hackage. In such cases, I use:

``` bash
$ cabal sandbox add-source path/to/non-hackage/dependency
```

This means I don't need to install such dependencies globally. If a dependency is particularly heavy and problematic, this approach can save my global package database.

However, you don't use Haskell only for writing libraries (amusing, isn't it?). Sometimes you need to install executables. This is where the 'executables' part comes in.

Usually, I install executables using the following sequence of commands:

``` fish
$ cd path/to/cabal/project
$ cabal sandbox init
$ cabal install --only-dependencies
$ cabal install
$ cp .cabal-sandbox/bin/executable ~/.bin/executable
```

This works because executables are usually completely standalone, so you can build them in a sandbox and then move them to any location of your choice. This approach helps keep the system-wide (or user-wide) package database clean and free from conflicts. I move the executable to `~/.bin` (ensuring `~/.bin` is in `$PATH`) because when something breaks in my package database, I want to keep these executables - they've done nothing wrong!

However, it's tedious to run these commands every time I want to install an executable, so I wrote a simple `fish` function that installs the executable from the `.cabal` file in the current directory for you.

``` fish
function cabal-install-bin -d "Install executables from .cabal file in current directory"
  # set some color settings
  set -l error_color red
  set -l msg_color blue

  # get cabal file in current directory
  set -l cb *.cabal

  set -l c (count *.cabal)
  # we expect only 1 cabal file to be existing
  if test c -ne 1
    set_color $error_color
    if test c -eq 0
      echo "Couldn't find cabal file in (pwd)"
    else
      echo "Found $c cabal files. Think about it!"
    end
    set_color normal
    return 1
  end

  set_color $msg_color
  echo "Using $cb"

  # check if sandbox is not created yet
  if test ! \( -e .cabal-sandbox \) -o ! \( -e cabal.sandbox.config \)
    echo "It looks like there is no sandbox, so creating one"
    set_color normal
    # create sandbox
    cabal sandbox init
  end

  # todo add support of multiple executables
  set -l name (cabal info *.cabal | sed -ne "s/ *Executables: *\(.*\)/\1/p")

  # check that the name is not empty
  if test ! \( -n $name \)
    set_color $error_color
    echo "Couldn't find any executable in cabal file"
    set_color normal
    return 1
  end

  set_color $msg_color
  echo "Found executables: $name"
  echo "Installing dependencies"
  set_color normal

  # first we want to install dependencies
  # we could just ~cabal install~
  # but I find separate installation
  # more satisfying
  cabal install --only-dependencies

  if test $status -ne 0
    return 1
  end

  set_color $msg_color
  echo "Building application"
  set_color normal

  # install package
  cabal install

  if test $status -ne 0
    return 1
  end

  set_color $msg_color
  echo "Copying $name to ~/.bin"
  set_color normal
  # now copy executable to ~/.bin
  cp ".cabal-sandbox/bin/$name" "$HOME/.bin/$name"
end
```

For situations when I don't care about the package sources and it's available on Hackage, I wrote another function (that reuses `cabal-install-bin`).

``` fish
function cabal-unpack-and-install-bin -a package -d "Unpack and install specified executable package from cabal."
  set -l current_dir (pwd)
  cd $TMPDIR
  set -l dir $package*

  if test (count $dir) -ne 0
    echo "Found $TMPDIR$dir"
    echo "Looks like the package already unpacked in \$TMPDIR"
    cd $current_dir
    return 1
  end

  cabal unpack $package

  if test $status -ne 0
    cd $current_dir
    return 1
  end

  set -l dir $package*

  cd $TMPDIR/$dir

  cabal-install-bin

  cd $TMPDIR
  rm -rf $dir

  cd $current_dir
end
```

This function downloads the sources of a single package to `$TMPDIR` (you might want to change this depending on your system), installs the executable (using the `cabal-install-bin` function), and removes the source directory. Useful, isn't it?

You can grab the latest version of these functions on [GitHub](https://github.com/d12frosted/environment/tree/master/fish/functions).

**Happy Haskell coding!**
