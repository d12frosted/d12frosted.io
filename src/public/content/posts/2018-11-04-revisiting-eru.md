As you might know, Eru is the supreme deity of Arda. The first things that Eru created were the Ainur. He then bade the Ainur to sing to him. Each Ainu had a particular theme given by Eru. Sure enough, Eru makes the 'World and All That Is'.

So when I get a new, clean system, there is nothing yet. And so I call upon the wisdom and power of `Eru.sh` - the one who creates Ainur and the 'World and All That Is'.

``` bash
$ curl https://raw.githubusercontent.com/d12frosted/environment/master/bootstrap/eru.sh | bash
```

I just have to wait patiently, while everything is being downloaded and installed, while all configuration cogs are being placed on the right spot.

<img src="/images/2018-11-04-revisiting-eru/2022-07-19-20-39-33-eru-example-1.webp" class="d12-image-1/2" />

<!--more-->

The good thing about `Eru.sh` is that after the 'World and All That Is' creation you can still call upon his help and you can even ask him to help with specific theme or themes only.

``` bash
$ ./bootstrap/eru.sh                   # to help with all themes
$ ./bootstrap/eru.sh repositories      # to help with repositories
$ ./bootstrap/eru.sh repositories brew # to help with repositories and brew
```

`Eru.sh` reveals all available themes. For example,

``` text
=> Guardian Theme :: Checking operating system
=> Supporting Theme :: Defining helpers
=> Supporting Theme :: Defining variables
=> Guardian Theme :: Ensure all directories exist
-> SSH Theme :: Checking SSH keys
-> Repositories Theme :: Sync environment repository
-> Repositories Theme :: Sync repositories from Repofile
-> Linking Theme :: Link all files as defined in Linkfile
```

Every theme that begins with `=>` is a mandatory theme - the one you can't skip. Every theme prefixed with `->` is an optional one. If you don't specify any themes, all optional themes are sung. If you do specify at least one theme, only the specified ones are sung.

<img src="/images/2018-11-04-revisiting-eru/2022-07-19-20-39-54-eru-example-2.webp" class="d12-image-1/2" />

You can find the latest version of `Eru.sh` on [GitHub](https://github.com/d12frosted/environment/blob/master/bootstrap/eru.sh).

# Interesting stuff

Mythology is great, but let's look at what's cool about `Eru.sh`.

1.  It installs and updates many applications and utilities that I use daily.
2.  It installs my configurations for these applications. The most important ones are `Emacs`, `fish`, `skhd`, and `chunkwm`. Plus, it tries to set up different parts of macOS to my liking.
3.  It works in an incremental fashion. In general, it only installs what's missing. So when my environment is up to date, `Eru.sh` is almost instant.
4.  It allows you to set up only specific parts. For example, you can install only dependencies or sync mandatory repositories, or do both.

This means that on a fresh system, I can easily get my environment just by running one command and waiting for a while. However, in a working environment, I can easily get the latest version of it. This comes in handy since I use more than one computer.

## Bash magic

`Eru.sh` is written in Bash because it must work on systems without Haskell preinstalled. Bash 4 features aren't used for the same reason - it's not available on a clean macOS.

`Eru.sh` is mostly a boring Bash script. However, there are several points that I find interesting.

### Mapping files

There are themes that perform the same action over a set of arguments. To avoid noise in `Eru.sh`, these arguments are stored in external files (rule files). For example, the `repositories` theme synchronises repositories according to rules specified in the `Repofile`, which looks like this:

``` text
$HOME/.spacemacs syl20bnr/spacemacs develop
$DEVELOPER/fancy-yank d12frosted/fancy-yank
$DEVELOPER/orgability d12frosted/orgability
$DEVELOPER/org-drawer-list d12frosted/org-drawer-list
$DEVELOPER/flyspell-correct d12frosted/flyspell-correct
```

Every line specifies a rule: the target location of the repository, remote repository URL (for GitHub, one can specify only `owner/repo` instead of a full HTTPS/SSH URL), and the branch to use.

There's also a function that gets these values as arguments and does all the work. In the case of the `repositories` theme, the function is called `sync_repo`.

One can easily write a script reading a given file line by line and passing them to a function. For example,

``` bash
while IFS='' read -r line || [[ -n "$line" ]]; do
  sync_repo $line
done < "Repofile"
```

Please note that `$line` shouldn't be put in quotation marks; otherwise, the whole line will be passed as the first argument, but we want to split it into multiple arguments by spaces.

Whilst this works, it doesn't scale, requires repetition, and decreases code readability. Let's abstract it a bit by extracting the logic of reading lines and passing them to a function.

``` bash
function map_lines() {
  while IFS='' read -r line || [[ -n "$line" ]]; do
    $1 $line
  done < "$2"
}

map_lines sync_repo "Repofile"
```

Now this resembles a functor for a file. There's no repetition, it scales well, and the purpose of `map_lines sync_repo "Repofile"` is immediately clear.

### Dealing with multiple optional themes

So in `Eru.sh`, we have multiple optional themes. The easiest way to deal with them is to create a separate variable for every theme and then use it to check if we should run an action for a given theme.

``` bash
ALL="true"
REPOSITORIES="false"
LINKING="false"
BREW="false"

POSITIONAL=()

while [[ $# -gt 0 ]]
do
  key="$1"
  case $key in
    repo)
      ALL="false"
      REPOSITORIES="true"
      shift # past argument
      ;;
    link)
      ALL="false"
      LINKING="true"
      shift # past argument
      ;;
    brew)
      ALL="false"
      BREW="true"
      shift # past argument
      ;;
    *)    # unknown option
      POSITIONAL+=("$1") # save it in an array for later
      shift # past argument
      ;;
  esac
done
set -- "${POSITIONAL[@]}" # restore positional parameters

if [[ "$ALL" = "true" || "$REPOSITORIES" = "true" ]]; then
  optional_theme "Repositories" # logging
  map_lines sync_repo "$target/bootstrap/Repofile"
else
  inactive_theme "Repositories" # logging
fi

if [[ "$ALL" = "true" || "$LINKING" = "true" ]]; then
  optional_theme "Linking" # logging
  map_lines safe_link  "$target/bootstrap/Linkfile"
else
  inactive_theme "Linking" # logging
fi

if [[ "$ALL" = "true" || "$BREW" = "true" ]]; then
  optional_theme "Brew" # logging
  cd "$target/bootstrap" && brew bundle
else
  inactive_theme "Brew" # logging
fi
```

But this gets nasty every time a new theme is added (though it happens rarely). And most importantly, it looks so redundant, so repetitive that I almost fall asleep while writing it.

We can improve it a little bit. Let's focus on the part where we check for a variable value.

``` bash
function theme_guard() {
  if [[ "$ALL" = "true" || "$1" = "true" ]]; then
    optional_theme "$2" # logging
    return 0
  else
    inactive_theme "$2" # logging
    return 1
  fi
}

theme_guard "$REPOSITORIES" "Repositories" && {
  map_lines sync_repo "$target/bootstrap/Repofile"
}

theme_guard "$LINKING" "Linking" && {
  map_lines safe_link  "$target/bootstrap/Linkfile"
}

theme_guard "$BREW" "Brew" && {
  cd "$target/bootstrap" && brew bundle
}
```

So we moved out all the checks and logging to a helper function. It is already good enough as an improvement, but we can do even better.

Bash provides an ability to evaluate arbitrary code by using `eval`. Please note though, that `eval` is a bane of shell programming and should be avoided like leprosy. In general, it's okayish to use `eval` when you fully control what is passed to it. If you pass user input to `eval` then you have a problem, because now you need to deal with code injection.

We can use `eval` to get the value of a variable by a string.

``` bash
$ CUSTOM_VAR=42

$ echo '$CUSTOM_VAR'
$CUSTOM_VAR

$ eval echo '$CUSTOM_VAR'
42
```

While this is good enough, it's possible to avoid `eval` in this particular case, when we just want to get the value of variable by name.

``` bash
$ CUSTOM_VAR=42

$ echo '$CUSTOM_VAR'
$CUSTOM_VAR

$ CUSTOM_VAR_REF=CUSTOM_VAR

$ echo ${!CUSTOM_VAR_REF}
42

```

Though if you want to expand variables in an arbitrary string (like `$HOME/config.json`), you'll have to use `eval`.

Right now we're passing two arguments to `theme_guard`: a guarding variable value and the name of a theme. But the code looks similar - the first one is prefixed with the `$` sign and is in uppercase, whilst the second one is in capital case. In order to deal with case conversion, we can use `awk`.

``` bash
function theme_guard() {
  key=$(echo "$1" | awk '{print toupper($0)}')
  local guard_ref="guard_$key"
  guard="${!guard_ref}"
  if [[ "$ALL" = "true" || "$guard" = "true" ]]; then
    optional_theme "$1" # logging
    return 0
  else
    inactive_theme "$1" # logging
    return 1
  fi
}

theme_guard "Repositories" && {
  map_lines sync_repo "$target/bootstrap/Repofile"
}

theme_guard "Linking" && {
  map_lines safe_link  "$target/bootstrap/Linkfile"
}

theme_guard "Brew" && {
  cd "$target/bootstrap" && brew bundle
}
```

We've achieved a small improvement - we no longer need to pass the theme name to `theme_guard` twice. I find this satisfying, and it will come in handy later.

Now let's go back to the variable declaration. It turns out that we can use `eval` to declare variables as well.

``` bash
$ eval 'MEANING=42'

$ eval echo '$MEANING'
42
```

But defining variables based on the user input is dangerous. Fortunately, there is a `declare` program designed just for this task. Moreover, it allows declaring read-only variables.

``` bash
$ declare "CUSTOM_VAR=42"

$ echo $CUSTOM_VAR
42

$ CUSTOM_VAR=12

$ echo $CUSTOM_VAR
12

$ declare -r "CUSTOM_VAL=42"

$ echo $CUSTOM_VAL
42

$ CUSTOM_VAL=12
bash: CUSTOM_VAL: readonly variable
```

So let's use `declare` to our advantage.

``` bash
ALL="true"

POSITIONAL=()
while [[ $# -gt 0 ]]
do
  if [[ "$1" != "" ]]; then
    key=$(echo "$1" | awk '{print toupper($0)}')
    declare -r "$key=true"
    ALL="false"
  fi
  shift
done
set -- "${POSITIONAL[@]}" # restore positional parameters
```

Whilst this becomes a little harder to understand, it saves us a lot of repetition. The only thing I would improve here immediately is to add a unique prefix to variable names, so the user doesn't interfere with other variables. The final version looks like this:

``` bash
ALL="true"

POSITIONAL=()
while [[ $# -gt 0 ]]
do
  if [[ "$1" != "" ]]; then
    key=$(echo "$1" | awk '{print tolower($0)}')
    declare -r "guard_$key=true"
    ALL="false"
  fi
  shift
done
set -- "${POSITIONAL[@]}" # restore positional parameters

function theme_guard() {
  key=$(echo "$1" | awk '{print tolower($0)}')
  local guard_ref="guard_$key"
  guard="${!guard_ref}"
  if [[ "$ALL" = "true" || "$guard" = "true" ]]; then
    optional_theme "$1" # logging
    return 0
  else
    inactive_theme "$1" # logging
    return 1
  fi
}

theme_guard "Repositories" && {
  map_lines sync_repo "$target/bootstrap/Repofile"
}

theme_guard "Linking" && {
  map_lines safe_link  "$target/bootstrap/Linkfile"
}

theme_guard "Brew" && {
  cd "$target/bootstrap" && brew bundle
}
```

Just by extracting checks into a separate function, using `declare` to define variables, and using variable indirection to read the value of variables, we've significantly improved the initial code. Less redundancy, clearer intention, and the ability to scale in terms of themes.

# Epilogue

Fortunately, our actions didn't lead to Saruman taking control over the Shire, so we are good. This post is already long enough, so I am going to finish here. If you have any questions, just [email](mailto:boris@d12frosted.io) me.
