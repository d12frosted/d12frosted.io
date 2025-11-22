I've always admired people who can do virtually everything programming-related from the terminal. Whilst every task has its own specialised tools, there are some instruments you can rely on in almost any situation. That's why I spend so much time in the terminal - it's the one constant across different projects and environments.

For about a year and a half, I was using `zsh` beefed up with `oh-my-zsh`. Whilst it provided crucial functionality, I wasn't entirely happy with the `oh-my-zsh` ecosystem. When someone mentioned `fish` in the comments of a Use Haskell for shell scripting thread on Reddit, I decided to give it a try after reading a few minutes about how excellent it is.

In this post, I'll share my experience using `fish` after making the switch. This isn't a tutorial (the shell already has an excellent one, and you might also want to read the full documentation), but rather a review of what makes `fish` special and whether it's worth switching.

**What you'll learn:**

- How Fish compares to traditional shells like Zsh
- Fish's powerful out-of-the-box autocompletion features
- Configuration options and customisation approaches
- Trade-offs between POSIX compatibility and user experience

Exploring new tools is fun. So even if you're totally happy with your current setup, I'd encourage you to consider what `fish` has to offer.

<!--more-->

# Fish: catch it before you cook

For installation guide, check official site or readme file on [GitHub](https://github.com/fish-shell/fish-shell).

After installation, you might want to make `fish` your default shell. To do this, use `chsh`:

``` bash
$ chsh -s $(which fish)
```

On macOS (and some other systems), you might encounter an error saying `chsh: /path/to/fish: non-standard shell`. In this case, you need to modify the `/etc/shells` file by adding the path to Fish:

``` bash
$ sudo sh -c 'echo $(which fish) >> /etc/shells'
```

For any other problems, may the force be with you. Otherwise, you're ready to dive in.

# Fish: first impression

The first thing you'll notice in `fish` is blazingly fast completion and suggestions based on history, man pages, and custom rules. This amazed me when I first saw it in action, because I remember how tricky it is to configure such things in `bash` and `zsh` - but here you get everything and more out of the box.

<img src="/images/2015-02-07-make-the-fish-fly/2022-07-19_17-11-16_fish-suggestions.gif" class="d12-image-3/4" />

What I particularly like about Fish's suggestions is how they narrow based on what you've typed. For example, when the prompt is empty, typing `↑` navigates through your entire history. If you type `stack`, the history narrows to show only `stack` commands. Type `stack build` and it narrows even further. It's like a tree where every word you type selects a more specific branch. Of course, it's actually a bit smarter than this simple metaphor suggests.

<img src="/images/2015-02-07-make-the-fish-fly/2022-07-19-17-35-48-fish-suggestions.webp" class="d12-image-1/2" />

The second impressive feature is how `fish` generates completions from `man` pages. In `bash`, you rely on `bash-completion`; in `oh-my-zsh`, you install plugins (and when you have many of them, everything becomes laggy). Fish, however, automatically parses `man` pages for completions, and it does so incredibly fast. I no longer need to clear what I've typed just to check `man` when I forget whether I need `-N` or `-n`. All completions are paginated, so you can scroll through them to find what you need. It's superb for productivity.

<figure class="d12-image-3/4">
<img src="/images/2015-02-07-make-the-fish-fly/2022-07-19-17-36-39-1423317617.webp" />
<figcaption>Fish generates them automatically by parsing your installed man pages</figcaption>
</figure>

So you'd better keep pressing `<tab>` in different situations to discover what else it can complete for you.

``` bash
λ git checkout <tab>
features/fish  (Branch)  master  (Branch)  origin/master  (Branch)
```

But yeah, sometimes command you are executing doesn't have its own `man` page. For such cases, you also have an option to write your own completions. For more information, check out this guide.

# Fish: configurations

`Fish` comes with excellent out-of-the-box configurations. For most users, it's enough to configure only the prompt and `PATH` variable. You should understand that `Fish` as a product [believes](https://fishshell.com/docs/current/design.html#design-configurability) that "configurability is the root of all evil". Their philosophy is that "every configuration option in a programme is a place where the programme is too stupid to figure out for itself what the user really wants, and should be considered a failure of both the programme and the programmer who implemented it".

Some might find this argument a bit extreme. In my opinion, though, the `Fish` developers have done an excellent job of providing sensible defaults (the irony isn't lost on me). That said, there are things that are intentionally configurable, such as visuals and environment variables.

Since `Fish` is a 'friendly fish', it allows you to configure many things from your browser. Just run the `fish_config` function in your terminal and explore the options.

On startup, `fish` evaluates files in this order: `/path/to/fish/config.fish`, `/etc/fish/config.fish`, and `$HOME/.config/fish/config.fish`. If you want to change `fish` configurations, modify `/etc/fish/config.fish` for system-wide configurations or `$HOME/.config/fish/config.fish` for user-only configurations.

Actually, `fish` supports `$XDG_CONFIG_HOME` variable for specifying the path to user-only configurations directory. You need to set this variable to the path of the directory that contains `fish` directory. I do use it because I like to have all dot files in one repository. You can change its value by using the `set` function:

``` bash
$ set -U XDG_CONFIG_HOME ~/.environment
```

We use `-U` flag here because we need to give `XDG_CONFIG_HOME` universal scope (it will be shared between all the current users fish instances on the current computer, and will be preserved across restarts of the shell).

If you want to configure your prompt, you need to define the `fish_prompt` function in file `$XDG_CONFIG_HOME/fish/fish_prompt.fish`. A simple example from the documentation:

``` fish
function fish_prompt -d "Write out the prompt"
  printf '%s@%s%s%s%s> ' (whoami) (hostname|cut -d . -f 1) (set_color $fish_color_cwd) (prompt_pwd) (set_color normal)
end
```

Before rushing into creating your own prompt, I recommend checking some of the prompts available out of the box. Just run `fish_config` and go to the `Prompt` tab. Alternatively, use [mine](https://github.com/d12frosted/environment/blob/master/fish/functions/fish_prompt.fish), which has a cool feature - it notifies you when long-running commands finish. I describe this briefly in a [separate article](/posts/2017-06-13-Fish-notify-me-when-you-finish).

# Fish: one language to script them all

**Update:** `Fish` has recently started moving towards more POSIX-compatible syntax, so this argument isn't as valid as it once was.

As you might already know, `fish` has its own scripting language that doesn't prioritise POSIX compatibility. In my opinion, this is the most controversial aspect of discussions like 'Zsh vs Fish'. Whilst it's less verbose and more intuitive, I see the `fish` language as an unnecessary complication. When I need to write any script that can't be described in one or two lines, I prefer to use `Haskell`. Additionally, there are countless great `sh` one-liners that don't work in the `fish` shell without modification. For example, `git cat-file -p branch^{tree}` becomes `git cat-file -p branch^\{tree\}`.

# Conclusion

`Fish` doesn't try to be everything, but it does aim to provide an excellent out-of-the-box experience. You should definitely give it a try and form your own conclusion - everyone's needs are different.

## Advantages

- Syntax highlighting.
- Good performance. I haven't measured it, but unlike `zsh` with huge amount of configurations, it doesn't lag.
- Suggestions and completion based on history, custom rules and man pages.
- Paginated completion, which is useful if you have dozens or hundreds items to complete from.
- Inline auto-suggestions.
- History de-duplication. So if you called something more than one time, it will appear in suggestions only once.

## Disadvantages

- Not POSIX compatible. Whilst I appreciate some of Fish's improvements over `sh`, I prefer using `sh` for small scripts. For anything non-trivial, I'd rather use `Haskell`. In my opinion, Fish's custom language is an unnecessary complexity. However, this is a minor drawback that only matters when you want to use classic `sh` one-liners.

I don't list Fish as a dependency drawback because I don't work in environments I can't modify. Besides, `Haskell` with all the packages I need is a far heavier dependency than `fish`, so it would be hypocritical to complain.

# Epilogue

I've grown tired of `oh-my-zsh`. It's a great project, but it's too large for my needs - it includes so much I don't use. The project suffers from its popularity. The last time I checked its repository, I found 417 unresolved pull requests, some of them years old, most receiving no attention. I understand the reasons for this situation, but understanding doesn't make it less frustrating.

I know you can use `zsh` without `oh-my-zsh` (or any similar project). You can keep your configurations [minimal](https://github.com/jleclanche/dotfiles/blob/master/.zshrc), or copy-paste huge amounts of code to maintain all the fancy features.

Whilst I usually enjoy tinkering with configurations, in this case I genuinely appreciate `fish`'s approach. Most things just work, allowing me to focus on specific functionality I actually need.

So go ahead and try what `fish` offers. Don't be afraid of making the switch. If you have any questions, feel free to email me - I'll be happy to help.

**Have a nice shell-time!**

P.S. As most of the stuff, my settings can be found on [GitHub](https://github.com/d12frosted/environment/tree/master/fish).
