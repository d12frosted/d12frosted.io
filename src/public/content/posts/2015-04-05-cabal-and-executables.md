Maybe I did something really wrong in my life, because I constantly run into **Cabal Hell**. That feeling of powerlessness is refreshing, though depressing. And I hate it the most when I just need to install an executable from Hackage, like `pandoc`.

But hey, we are software engineers after all. So I decided to write a little helper script to avoid world destruction and get desired executable in my `$PATH`.

**Important note.** In the last few years Haskell community did a great job in order to fight the **Cabal Hell**. And thanks to [Stack](https://haskell.fpcomplete.com/get-started) and [Nix-style Local Builds](https://cabal.readthedocs.io/en/latest/nix-local-build-overview.html) my solution is no longer required. I keep it just for historical reference (and to keep my blog relatively busy).

<!--more-->

> **Cabal Hell**
>
> The feeling of powerlessness one has when Cabal does not do what one wanted and one does not know how to fix it.
>
> Well Typed

> **What is the difficulty caused by Cabal-install?**
>
> The main difficulty with Cabal is otherwise known as 'dependency hell', in which the cabal-install does not manage to install a desired package for a reason or another, leading to large amount of manual work. As an example of this difficulty, consider a case where the user wishes to install packages A and B. Both of these work with package C, but not with the same version of C.
>
> [Haskell Wiki](https://wiki.haskell.org/Cabal/Survival)

<img src="/images/2015-04-05-cabal-and-executables/2022-07-19-17-49-57-1428233775.webp" class="d12-image-1/2" />

I need to confess. Sometimes I solve ****Cabal Hell**** by using this method with `rm -rf`. ****Cabal Hell**** is like cancer - it's very hard to cure this disease without ruining your environment (in our case - packages database). But with **Cabal Hell** comes one good thing - you can use some tools in order to prevent this bizarre to happen with you. For such purposes, you can use cabal sandboxes, Stackage or nixos. Probably there are some other handy solutions or tools, but this is all I know.

`Stackage` is great, but it doesn't work for me very well because sometimes I need to install 'heavy' packages that are not on `Stackage`. Also, I work on a reliably fast computer, so I don't mind to waste thirty seconds more on the compilation. Safety is more preferable. As for `nixos` – I haven't tried it yet. But I know that it helps to find compilation problems very good. So actually, many thanks to the people that made `Stackage` and `nixos`.

# Sandboxes

I think that sandboxes are really great. Usually, I install globally only commonly used packages. Everything else comes via sandboxes. Sometimes the project I am working on has dependencies that can't be installed from Hackage. In such cases I use

``` bash
$ cabal sandbox add-source path/to/non-hackage/dependency
```

So I don't need to install such dependencies globally. And if this dependency is very heavy and problem-bringing, then it can save my global packages database.

But you use Haskell not only for writing libraries (funny, isn't it?). Sometimes you need to install some executables. So here comes the 'executables' part.

Usually, I install executables by using the following sequence of commands:

``` fish
$ cd path/to/cabal/project
$ cabal sandbox init
$ cabal install --only-dependencies
$ cabal install
$ cp .cabal-sandbox/bin/executable ~/.bin/executable
```

This works because, executables are usually completely stand-alone, so you can build them in a sandbox and then move them to any location of your choice. This approach helps to keep the system (or user) wide packages database clean and free from conflicts. I move executable into `~/.bin` (but make sure that `~/.bin` is in `$PATH`), because when something breaks in my packages database I want to keep these executables (they made nothing bad!).

But it's very boring to call this commands every time I want to install any executables, so I wrote a simple `fish` function that installs executable from `.cabal` file in the current directory for you.

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
      echo "Couldn' find cabal file in (pwd)"
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
  # now copy executable to ~/.bing
  cp ".cabal-sandbox/bin/$name" "$HOME/.bin/$name"
end
```

But for situations when I don't care about package sources and it's available on hackage, I wrote another function (that reuses `cabal-install-bin`).

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

It just downloads sources of a single package to the `$TMPDIR` (you might want to change this to something different, depending on your system), then installs executable (using `cabal-install-bin` function) and removes sources dir. Useful, isn't it?

You can grab the latest version of these function on [GitHub](https://github.com/d12frosted/environment/tree/master/fish/functions).

**Happy Haskell coding!**
