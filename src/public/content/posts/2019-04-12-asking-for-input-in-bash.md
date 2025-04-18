Yet another tale about bash script refactoring. This time the configuration script of [mirror-elpa](https://github.com/d12frosted/mirror-elpa) had undergone some changes and I like the result so much that I decided to share it.

Imagine that you want your users to give you some input, for example, repository owner and name. And you want the value so desperately, that if the user doesn't provide one, you ask the user again. On the other hand, you also want to support default values. So for example, if the user doesn't provide any value, but there is a default value, you could use that.

And you can't use anything except for bash.

<!--more-->

Text descriptions are nice, but let's see in action, what we want to achieve.

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

As you can see, there is no default value of the repository owner, so if the user doesn't provide a value, we ask again. While the repository name has a default value so when the user doesn't provide any other value, the default one is used. And the user has an option to override the default value.

It's really easy to implement this in bash.

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

But common, we've heard about the DRY thing, right? We can use variable indirection and `declare` to do all the dirty job for us.

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

Indirection allows us to get the value of the variable by name. From the bash man:

> If the first character of parameter is an exclamation point (`!`), a level of variable indirection is introduced. Bash uses the value of the variable formed from the rest of parameter as the name of the variable; this variable is then expanded and that value is used in the rest of the substitution, rather than the value of parameter itself. This is known as indirect expansion.

We could avoid using indirection here, by using `eval`, but `eval` can lead to problems as it allows execution of arbitrary code. So why risking, when we can use something suited for this job?

The same goes for `declare`. While we could use `eval`, there is a built-in just for this particular problem. We use `-r` since we are not planning to modify this value in the future and `-g` flag to make the variable accessible outside the function body via regular variable expansion. Please note that `-g` option is not available prior to 4.2 version of `bash`. If for some reason you plan to support older versions (for example, on macOS the preinstalled `bash` has version 3.2), then you can use `readonly`.

``` bash
readonly "$1=$res"
```

If you are interested in using `eval` instead of indirection and `decalre`, it would look like this.

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

But personally, I would not use `eval` until I really have to.

You can find another example of this technique in [Revisiting Eru](/posts/2018-11-04-revisiting-eru) post.
