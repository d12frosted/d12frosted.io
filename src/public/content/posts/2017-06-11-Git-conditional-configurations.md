More than five years ago [Jeff King](https://github.com/peff) has [added](https://github.com/git/git/commit/9b25a0b52e09400719366f0a33d0d0da98bbf7b0) [include directive](https://git-scm.com/docs/git-config#_includes) to [git config](https://git-scm.com/docs/git-config) subprogram. This directive allows splitting configuration file across multiple files. A nice feature by itself was [boosted](https://github.com/git/git/compare/45cbc37c5f84fadd78cccf6b2ea82a9ef251cdc6...3efd0bedc6625a6b194c1f6e5f1b7aa7d8b7e6bb) in [Git 2.13.0](https://github.com/git/git/releases/tag/v2.13.0) by adding conditional includes - one of my favourite features of this release. And something that makes me sad.

<!--more-->

Consider a simple example of `~/.gitconfig` file.

``` conf-unix
# content of ~/.gitconfig

[core]
    editor = emacsclient

[user]
    useconfigonly = true

[include]
    path = ~/.gitconfig_machine_specific
    path = ~/.gitconfig_sensitive

# include ~/.gitconfig_personal only when active repository is under
# ~/Projects/personal/
[includeIf "gitdir:~/Projects/personal/"]
    path = ~/.gitconfig_personal

# include ~/.gitconfig_work only when active repository is under
# ~/Projects/work/
[includeIf "gitdir:~/Projects/work/"]
    path = ~/.gitconfig_work
```

As you can see, using `include` and `includeIf` directives allows to achieve following things.

1.  File `~/.gitconfig` can be shared among different computers as machine specific configurations are included as a separate file (`~/.gitconfig_machine_specific`).
2.  File `~/.gitconfig` can be made public as all sensitive configurations are included as a separate file (`~/.gitconfig_sensitive`).
3.  You can set some configurations based on git repository location (e. g. personal or work).

Please note that these directives can be used in any git configuration level (system level, user level, repository level or individual command invocation). Which makes it even more powerful.

Also, note that the only condition implemented now is `gitdir` that matches against repository path.

# The same but different

Almost a year ago I have created a tool named [git-config-manager](https://github.com/d12frosted/git-config-manager). The idea is simple[^1], you have several configuration schemes which you may apply on a repository level. For example, you may use one name, email address and commit sign key when working on open source projects and totally different identity when committing to your day job repository. You just ask the tool to set one or several schemes in current repository and it loops through key-values and sets them using `git config`. Nothing fancy.

With `git-config-manager` you define schemes in a single `json` file.

``` json
{
  "personal": {
    "user": {
      "name": "Drunk Monkey",
      "email": "drunk.monkey@protonmail.ch",
      "signingkey": "A1B2C3D4"
    },
    "pull": {
      "rebase": true
    }
  },
  "work": {
    "user": {
      "name": "Mr. Tie",
      "email": "tie@corp.me",
      "signingkey": "E5F6G7H8"
    },
    "pull": {
      "rebase": null
    }
  }
}
```

When you ask `git-config-manager` to set configurations from the `personal` scheme (`$ git-config-manager set personal`), it takes the definition and calls `git config` for every configuration key-value pair. This is really straightforward.

But the simple approach has several drawbacks. First of all, you have to manually call `git-config-manager set scheme` (from the terminal or using the `magit` extension). While with `includeIf` directory everything is handled automatically. Secondly, if you wish to change configuration piece in all affected repositories (e. g. update sign key in all work projects), git configuration system allows you to make the change in one place (in the `~/.gitconfig_work` file) and it will be automatically propagated to all affected repositories. With `git-config-manager` it's not that simple because it just sets values when it's asked to set.

# What's the point?

When I learned about `includeIf` directory I thought that the time has come to deprecate `git-config-manager`. But then I realised that while they overlap in use cases, I might implement several new features I've been thinking about lately.

So again, stay tuned. Meanwhile, please read about [other changes in Git 2.13.0](https://github.com/blog/2360-git-2-13-has-been-released).

[^1]: The idea is simple, but solution is slightly over-engineered. Mostly because I didn't know about include directive in the first place. And partly because I wanted to write it in Haskell.
