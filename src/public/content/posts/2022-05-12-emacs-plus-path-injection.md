> Ever find that a command works in your shell, but not in Emacs?
>
> – [@purcell](https://github.com/purcell)

There are two known billion-dollar problems in software engineering: null pointer exceptions and caching. Over time of maintaining various projects I realised that there is a third billion-dollar problem - `PATH` value. Whenever something doesn't build, whenever something doesn't work as expected, the blood trail often leads to… `PATH` value. Sometimes it's some other environment variable, but in most of the cases, it's just `PATH`.

Time to time I help Emacs+ users with `PATH` related issues, so I figured that Emacs+ should provide some solution out of box. 10 messages from users in an esoteric project is like Maidan in Kyiv (yup, now I am bringing politics in here). So there is a huge problem needs solving. From thought to solution the path is quick.

TL;DR, your `PATH` value is now being injected into `Emacs.app` during build, so it is picked up by Emacs whenever you run it from Finder, Docker, Spotlight or `launchd` system. Works for `emacs-plus@28` and `emacs-plus@29`.

<!--more-->

Even though I talk about problems related to `PATH` value, it's value is actually correct, just not something we expect when it comes to using external binaries from within Emacs.

Disclaimer. In the next few paragraphs I am explaining how macOS runs applications, but it might be not fully accurate as I am not an expert in these low level details. At least, speculation stands, so bear with me. And if you know better then me, please do tell me, so we can improve this information.

User applications in macOS are running in the login shell environment. From `PATH` perspective, it means that it's value is inherited from login shell. So what's the value? If we take a look at `/etc/profile` and `/etc/zshenv` we see that they all run `path_helper`, which already appeared on my blog in [Fixing PATH in fish with nix-darwin](/posts/2021-05-21-path-in-fish-with-nix-darwin) post. In short, it's a helper for constructing `PATH` environment variable by taking entries from `/etc/paths` file and all files under `/etc/paths.d/`. It actually also constructs `MANPATH` entries, but it's not important right now.

So by default, applications have `PATH` value defined by result of `path_helper` invocation.

``` bash
$ cat /etc/paths
/usr/local/bin
/usr/bin
/bin
/usr/sbin
/sbin

$ ls -l /etc/paths.d
-rwxr-xr-x 1 root wheel 23 Apr 22 20:47 100-rvictl*

$ cat /etc/paths.d/100-rvictl
/Library/Apple/usr/bin

$ /usr/libexec/path_helper -s
PATH="/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin:/Library/Apple/usr/bin"; export PATH;
```

And this is pretty much the same `PATH` most users see unless they tweak things. As it turns out there are several ways to do it. I am not going to dig into each of them, but roughly, you have the following options:

1.  Add a file to `/etc/paths.d/` with values you want to append to `PATH` by default. This method affects everything, so use cautiously.
2.  Create a `my.startup.plist` in `~/Library/LaunchAgents` that calls `launchctl setenv` to modify `PATH`. This method affects all applications run by user.
3.  Set [LSEnvironment](https://developer.apple.com/documentation/bundleresources/information_property_list/lsenvironment) property in applications `Info.plist` file. This method affects only specific application.
4.  [purcell/exec-path-from-shell](https://github.com/purcell/exec-path-from-shell), which is a great alternative of previous method, but for Emacs specifically. The biggest added value is that it works dynamically and is based on your current shell settings.

At this point it might become obvious, which method Emacs+ takes to solve `PATH` problem for its users. No, it's not the last option, as it's up to user to install external packages and setup them. Less intrusive, but more error-prone solution is to modify `Emacs.apo/Info.plist` file to include the value of the `PATH` during installation.

Turns out, `brew` uses a sanitized environment, which they call `superenv`. If you think about it, such environment makes a lot of sense as you want to avoid any environment dependent stuff to mess up installation process. But that also means that Emacs+ formula can't access user `PATH` value.

The good part is that it's possible to switch to standard environment in formula definition:

``` ruby
class EmacsPlusAt29 < EmacsBase # which is actually just Formula
  # ...
  env :std

  # ...
end
```

Now it's possible to access `PATH` value and inject it to the `Info.plist`.

``` ruby
prefix.install "nextstep/Emacs.app"

# inject PATH to Info.plist
path = ENV['PATH'].split("#{HOMEBREW_PREFIX}/Library/Homebrew/shims/shared:").last
system "/usr/libexec/PlistBuddy -c 'Add :LSEnvironment dict' '#{prefix}/Emacs.app/Contents/Info.plist'"
system "/usr/libexec/PlistBuddy -c 'Add :LSEnvironment:PATH string' '#{prefix}/Emacs.app/Contents/Info.plist'"
system "/usr/libexec/PlistBuddy -c 'Set :LSEnvironment:PATH #{path}' '#{prefix}/Emacs.app/Contents/Info.plist'"
system "/usr/libexec/PlistBuddy -c 'Print :LSEnvironment' '#{prefix}/Emacs.app/Contents/Info.plist'"
system "touch '#{prefix}/Emacs.app'"
```

Notice that I remove all entries from `PATH` that come before (inclusive) `#{HOMEBREW_PREFIX}/Library/Homebrew/shims/shared`. This is because `brew` still appends some stuff to the `PATH` during build process and we definitely don't want that stuff to be part of `PATH`, right?

That's basically it. With these small changes you can enjoy expected `PATH` value without the need to use [purcell/exec-path-from-shell](https://github.com/purcell/exec-path-from-shell). Though this package should be used if you want to get other stuff from your interactive shell.

While it all might sounds like an effort not worth result, this change actually provides two extra points.

I could at last remove flags manipulations related to `native-comp` feature:

``` ruby
if build.with? "native-comp"
  gcc_ver = Formula["gcc"].any_installed_version
  gcc_ver_major = gcc_ver.major
  gcc_lib="#{HOMEBREW_PREFIX}/lib/gcc/#{gcc_ver_major}"

  ENV.append "CFLAGS", "-I#{Formula["gcc"].include}"
  ENV.append "CFLAGS", "-I#{Formula["libgccjit"].include}"
  ENV.append "CFLAGS", "-I#{Formula["gmp"].include}"
  ENV.append "CFLAGS", "-I#{Formula["libjpeg"].include}"

  ENV.append "LDFLAGS", "-L#{gcc_lib}"
  ENV.append "LDFLAGS", "-I#{Formula["gcc"].include}"
  ENV.append "LDFLAGS", "-I#{Formula["libgccjit"].include}"
  ENV.append "LDFLAGS", "-I#{Formula["gmp"].include}"
  ENV.append "LDFLAGS", "-I#{Formula["libjpeg"].include}"
end
```

It's not that bad to have them, but since Emacs own `configuration.ac` supports `brew` during `libgccjit` check, I'd rather let Emacs developers do the work that they know how to do (in contrast with my doings).

Another perk is also related to `native-comp` feature, but now it affects users in a more direct fashion. Native compilation normally starts **before** any custom user code in `init.el` and people [often run into problems](https://github.com/d12frosted/homebrew-emacs-plus/issues?q=native-comp) related to [environment troubles](https://github.com/d12frosted/homebrew-emacs-plus/issues/378).

So all that is nice. Hopefully I will not need to revert this injection. Because at this point of time, injection happens in `emacs-plus@28` and `emacs-plus@29`. But most importantly, there is a blog post about `PATH` injection. So business here is serious, you can't simply step back.

Safe travels folks! And use Emacs responsibly.
