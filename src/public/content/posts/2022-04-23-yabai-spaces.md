![](/images/2022-04-23-yabai-spaces/2022-07-19_22-18-40_yabai-banner.svg)

The idea behind tiling window managers is brilliant - frames on the screen are organised in a non-overlapping fashion. In practice that means productivity boost because (1) all non-hidden frames are always visible and (2) all resizing and movement is done automatically by window manager whenever a frame becomes visible or hidden. It might take some time to get used to this approach, but in the end it's love or hate relationship without position in-between.

My story with tiling window managers started in macOS, where window manager can't be changed, but… you have applications that imitate them either in a non-intrusive manual manner (à la [Spectacle](https://www.spectacleapp.com)) or in an automatic manner (à la [Amethyst](https://github.com/ianyh/Amethyst) and [yabai](https://github.com/koekeishiya/yabai)). So around 4 years ago I started using chunkwm (former yabai) and I've been happy user since then.

Since I am too lazy to setup macOS spaces manually, I want yabai to enforce specific configuration on startup - constant amount of spaces (with labels), meaning that missing are created and extra are removed. In addition, I want some applications to start on specific spaces (e.g. I love my browser to always be in the 3rd space) and to start some application silently (without stealing focus).

In this article we are going to learn how to achieve these goals.

<!--more-->

# Prerequisites

In order to achieve defined goals, you need to complete the following steps.

1.  [Disable System Integrity Protection](https://github.com/koekeishiya/yabai/wiki/Disabling-System-Integrity-Protection).
2.  [Install yabai](https://github.com/koekeishiya/yabai/wiki/Installing-yabai-(latest-release)) (unfortunately, preferably from [brew](https://brew.sh)) and enable scripting additions.
3.  Create [configuration file](https://github.com/koekeishiya/yabai/wiki/Configuration#configuration-file).
4.  Make sure that yabai is running and operational.
5.  Install [jq](https://stedolan.github.io/jq/) (JSON processor).

# Setting up preferred spaces

In my setup I have 6 spaces, one is for Emacs, one for terminal emulators, one for web, one for social stuff, one for media stuff and one for various trash. Yabai [configuration file](https://github.com/koekeishiya/yabai/wiki/Configuration#configuration-file) must be an executable and in order to make things easy, let it be shell executable.

``` bash
#!/usr/bin/env sh

# load scripting additions (optional)
sudo yabai --load-sa
yabai -m signal --add event=dock_did_restart action="sudo yabai --load-sa"

# config (tweak it to your liking)
yabai -m config layout bsp
yabai -m config top_padding    8
yabai -m config bottom_padding 8
yabai -m config left_padding   8
yabai -m config right_padding  8
yabai -m config window_gap     8
yabai -m config auto_balance off
yabai -m config split_ratio 0.5
yabai -m config window_shadow off
```

So let's define a function called `setup_space` that takes two arguments - space index and it's label (unless you are using widgets software like [Übersicht](https://github.com/felixhageloh/uebersicht), labels have little meaning). We are going to use it like this:

``` bash
setup_space 1 emacs
setup_space 2 code
setup_space 3 web
setup_space 4 social
setup_space 5 media
setup_space 6 other
```

So what should it do in order to make sure that these 6 spaces exist and properly set?

Yabai comes with [query command](https://github.com/koekeishiya/yabai/wiki/Commands#querying-information) that allows to query information about all spaces or specific spaces. For example, `yabai -m query --spaces` returns information about all spaces, and `yabai -m query --spaces --space 1` returns information about space with index `1` if it exists.

There are [space](https://github.com/koekeishiya/yabai/wiki/Commands#space-commands) commands allowing to create spaces and label them.

This should be enough for us to define `setup_space`.

``` bash
function setup_space {
  local idx="$1"
  local name="$2"
  local space=
  echo "setup space $idx : $name"

  space=$(yabai -m query --spaces --space "$idx")
  if [ -z "$space" ]; then
    yabai -m space --create
  fi

  yabai -m space "$idx" --label "$name"
}

setup_space 1 emacs
setup_space 2 code
setup_space 3 web
setup_space 4 social
setup_space 5 media
setup_space 6 other
```

You can remove all spaces except for 1 via Mission Control and run `yabairc`:

``` bash
$ ~/.config/yabai/yabairc
```

All extra spaces should be created.

In the internet you may find solution that uses space focus command - `yabai -m space --focus "$idx" || yabai -m space --create`, but keep in mind that this doesn't work as intended and leads to creation of extra spaces on each run of `yabairc`. Aside for producing undesired result, this approach also has two extra drawbacks. Firstly, it's slower because focus switching is not free. Secondly, focus switching leads to screen flickering. That's why you should avoid this command in setup script as much as possible.

# Cleaning up extra spaces

This is completely optional, but I like to remove extra spaces on startup. First of all, I don't have key bindings to quickly switch to spaces with index bigger than 6. Secondly, if for some reason I am using macOS full screen application, it places the application too far away.

In order to achieve this goal, we have two approaches.

1.  Check if space with index `MAX_SPACES + 1` (for me it's `7`) exists, and if it does - remove it. Repeat the procedure until you run out of spaces with index `MAX_SPACES + 1`. This works because removing a space in the middle, changes index of all that comes after.
2.  Just query all spaces with index `> MAX_SPACES` and remove them.

Since I want to reduce invocations of `yabai` client, I am going with the second approach. In order to query spaces with index bigger than `MAX_SPACES`, we are going to use `jq` select capabilities. If you want to learn more about that, just take a break and use `jq` manual.

``` bash
for idx in $(yabai -m query --spaces | jq '.[].index | select(. > 6)' | sort -nr); do
  yabai -m space --destroy "$idx"
done
```

Keep an eye for `sort -nr`. We want to remove spaces in reversed order, otherwise indices are reassigned. Another approach would be to ignore `idx` and always remove space with index `7`:

``` bash
for _ in $(yabai -m query --spaces | jq '.[].index | select(. > 6)'); do
  yabai -m space --destroy 7
done
```

In that case you don't need to rely on `sort` and `jq` selector just acts as a way to repeat action `SPACES_COUNT - MAX_SPACES` times.

# Moving applications on start

As I said, I love my browser to start on specific space. What makes yabai so wonderful is system of events and rules. You can read more about rules and signals by running `man yabai` or by checking official wiki on [rules](https://github.com/koekeishiya/yabai/blob/master/doc/yabai.asciidoc#66-rule) and [signals](https://github.com/koekeishiya/yabai/blob/master/doc/yabai.asciidoc#67-signal).

``` bash
yabai -m rule --add app="^Safari$" space=^3
yabai -m rule --add app="^FireFox$" space=^3
yabai -m rule --add app="^Telegram$" space=4
yabai -m rule --add app="^Music$" space=5
yabai -m rule --add app="^Spotify$" space=5
```

If you put "^" before space number, the space will be focused after application is started. Personally, I don't use that as because (a) I often start applications without intention to use them right now and (b) I am forced to reload configuration on Emacs initialisation, so it's being picked up by yabai.

In general, rules and signal are quite powerful, so I urge you to play around with them. I am yet to discover more possibilities. Would love to hear from you if you have ideas to share.

# Full configuration

``` bash
#!/usr/bin/env bash

# load scripting additions
sudo yabai --load-sa
yabai -m signal --add event=dock_did_restart action="sudo yabai --load-sa"

# config
yabai -m config layout bsp
yabai -m config top_padding    8
yabai -m config bottom_padding 8
yabai -m config left_padding   8
yabai -m config right_padding  8
yabai -m config window_gap     8
yabai -m config auto_balance off
yabai -m config split_ratio 0.5
yabai -m config window_shadow off

#
# setup spaces
#
for _ in $(yabai -m query --spaces | jq '.[].index | select(. > 6)'); do
  yabai -m space --destroy 7
done

function setup_space {
  local idx="$1"
  local name="$2"
  local space=
  echo "setup space $idx : $name"

  space=$(yabai -m query --spaces --space "$idx")
  if [ -z "$space" ]; then
    yabai -m space --create
  fi

  yabai -m space "$idx" --label "$name"
}

setup_space 1 emacs
setup_space 2 code
setup_space 3 web
setup_space 4 social
setup_space 5 media
setup_space 6 other

# move some apps automatically to specific spaces
yabai -m rule --add app="^Safari$" space=^3
yabai -m rule --add app="^Firefox$" space=^3
yabai -m rule --add app="^Telegram$" space=4
yabai -m rule --add app="^Music$" space=5
yabai -m rule --add app="^Spotify$" space=5
```

Thank you for bearing with me till the end! Safe travels!
