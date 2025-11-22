Yet another tale about Bash script refactoring. This time, the configuration script of [mirror-elpa](https://github.com/d12frosted/mirror-elpa) has undergone some changes, and I like the result so much that I decided to share it.

Imagine you want your users to provide some input - for example, repository owner and name. You need this value, so if the user doesn't provide one, you ask again. However, you also want to support default values, so that if the user doesn't provide any value but there's a default available, you use that instead.

And you can only use Bash.

<!--more-->

Text descriptions are nice, but let's see what we want to achieve in action.

``` bash
λ ./config
Repository owner: ↵
Repository owner: d12frosted↵
Repository name: (mirror-elpa) ↵

owner = d12frosted
repo = mirror-elpa

λ ./config
Repository owner: d12frosted↵
Repository name: (mirror-elpa) some-other-repo↵

owner = d12frosted
repo = some-other-repo
```

As you can see, there's no default value for the repository owner, so if the user doesn't provide a value, we ask again. The repository name, however, has a default value, so when the user doesn't provide any other value, the default is used. The user also has the option to override the default value.

It's relatively straightforward to implement this in Bash.

``` bash
# defaults
owner=
repo=mirror-elpa

# get the owner
while :; do
  printf "Repository owner: "
  [ -n "$owner" ] && printf "(%s) " "$owner"
  read -r i_owner
  [ -z "$i_owner" ] && i_owner="$owner"
  [ -n "$i_owner" ] && break
done
owner="$i_owner"

# get the repo
while :; do
  printf "Repository name: "
  [ -n "$repo" ] && printf "(%s) " "$repo"
  read -r i_repo
  [ -z "$i_repo" ] && i_repo="$repo"
  [ -n "$i_repo" ] && break
done
repo="$i_repo"

# print result
echo
echo "owner = $owner"
echo "repo = $repo"
```

However, we've all heard about the DRY principle, right? We can use variable indirection and `declare` to do all the work for us.

``` bash
# defaults
owner=
repo=mirror-elpa

ask() {
  local def
  local res
  def="${!1}"
  res=
  while :; do
    printf "%s: " "$2"
    [ -n "$def" ] && printf "(%s) " "$def"
    read -r res
    [ -z "$res" ] && res="$def"
    [ -n "$res" ] && break
  done
  declare -rg "$1=$res"
}

ask "owner" "Repository owner"
ask "repo" "Repository name"

echo
echo "owner = $owner"
echo "repo = $repo"
```

Indirection allows us to get the value of a variable by name. From the Bash manual:

> If the first character of parameter is an exclamation point (`!`), a level of variable indirection is introduced. Bash uses the value of the variable formed from the rest of parameter as the name of the variable; this variable is then expanded and that value is used in the rest of the substitution, rather than the value of parameter itself. This is known as indirect expansion.

We could avoid using indirection by using `eval`, but `eval` can lead to problems as it allows execution of arbitrary code. Why risk it when we can use something designed for this job?

The same goes for `declare`. Whilst we could use `eval`, there's a built-in specifically for this problem. We use `-r` since we're not planning to modify this value in the future, and the `-g` flag to make the variable accessible outside the function body via regular variable expansion. Please note that the `-g` option isn't available prior to Bash version 4.2. If you need to support older versions (for example, on macOS the preinstalled Bash is version 3.2), you can use `readonly` instead.

``` bash
readonly "$1=$res"
```

If you're interested in using `eval` instead of indirection and `declare`, it would look like this.

``` bash
#!/usr/bin/env bash

# defaults
owner=
repo=mirror-elpa

ask() {
  local def
  local res
  def=$(eval echo "\$$1")
  res=
  while :; do
    printf "%s: " "$2"
    [ -n "$def" ] && printf "(%s) " "$def"
    read -r res
    [ -z "$res" ] && res="$def"
    [ -n "$res" ] && break
  done
  eval "$1=$res"
}

ask "owner" "Repository owner"
ask "repo" "Repository name"

echo
echo "owner = $owner"
echo "repo = $repo"
```

However, I personally wouldn't use `eval` unless absolutely necessary.

You can find another example of this technique in my [Revisiting Eru](/posts/2018-11-04-revisiting-eru) post.
