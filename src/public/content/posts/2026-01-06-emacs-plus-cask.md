For years, the number one feature request for `emacs-plus` has been pre-built binaries. Every time someone opened an issue, I'd explain why it wasn't feasible: native compilation dependencies, icon customisation, patches, the sheer complexity of bundling everything correctly.

Well, I finally ran out of excuses.

``` bash
brew install --cask emacs-plus-app
```

That's it. ~60 seconds instead of ~30 minutes. Native compilation included. No dependency hell.

# The Problem with Compiling Emacs

If you've installed `emacs-plus` before, you know the drill:

1.  Run `brew install emacs-plus@31`
2.  Wait 20-40 minutes while your laptop turns into a space heater
3.  Pray nothing goes wrong with dependencies
4.  Eventually, maybe, get a working Emacs

And then the fun begins. A week later, you run `brew upgrade` and suddenly:

``` example
dyld[12345]: Library not loaded: /opt/homebrew/opt/libgccjit/lib/gcc/current/libgccjit.so.0
```

Your Emacs is broken because Homebrew upgraded `libgccjit` and the paths changed. The fix? `brew reinstall emacs-plus@31` and another 30-minute wait.

Native compilation made this worse. It's an incredible feature - Emacs Lisp compiled to native code, massive performance improvements - but it comes with a fragile dependency chain. `libgccjit` needs `gcc`, both need to be the exact versions Emacs was compiled against, and if anything changes… broken.

# The Cask Solution

The new cask approach solves all of this:

``` bash
# Stable (currently Emacs 30)
brew install --cask emacs-plus-app

# Development (Emacs master branch)
brew install --cask emacs-plus-app@master
```

The key wins:

- **Self-contained** - All dependencies bundled. No more "library not loaded" after upgrades.
- **Native compilation works** - Out of the box. No configuration. No tricks.
- **Custom icons** - Yes, even with pre-built binaries.

The cask includes native compilation (with AOT), xwidgets, tree-sitter, imagemagick, mailutils, and [Emacs Client.app](/posts/2025-11-24-emacs-plus-client-app). The `@master` variant updates nightly.

# Native Compilation: The Hard Part

Getting native compilation to work in a redistributable binary was the main technical challenge. Here's why it's tricky.

When Emacs compiles Lisp to native code, it shells out to `libgccjit`, which in turn needs `gcc` and various support libraries. On a normal Homebrew installation, these live in `/opt/homebrew/opt/libgccjit/` and `/opt/homebrew/opt/gcc/`.

But in a cask? The user might not have these installed. Or they might have different versions. We needed the app to be completely self-contained.

## Bundling the Libraries

The first step was bundling all required libraries inside `Emacs.app`:

    Emacs.app/Contents/Frameworks/
    ├── libgccjit.so.0
    ├── libgcc_s.1.1.dylib
    ├── libemutls_w.a
    └── ... (other gcc runtime libs)

We use `install_name_tool` to rewrite the library paths so they're relative to the app bundle:

``` bash
install_name_tool -change \
  "/opt/homebrew/opt/libgccjit/lib/gcc/current/libgccjit.so.0" \
  "@executable_path/../Frameworks/libgccjit.so.0" \
  Emacs.app/Contents/MacOS/Emacs
```

## The Environment Problem

But bundling libraries isn't enough. When Emacs invokes `libgccjit` to compile Lisp code, `libgccjit` needs to find `gcc`, header files, and linker scripts. These paths are baked in at compile time.

The solution is `CaskEnv` - a small system that injects the correct environment variables when Emacs launches:

``` ruby
# Simplified from Library/CaskEnv.rb
def self.inject_native_comp_env(app_path)
  frameworks = "#{app_path}/Contents/Frameworks"

  env = {
    "LIBRARY_PATH" => frameworks,
    "LD_LIBRARY_PATH" => frameworks,
    "C_INCLUDE_PATH" => "#{frameworks}/include",
    "LDFLAGS" => "-L#{frameworks}"
  }

  # Write to Info.plist LSEnvironment
end
```

When you launch Emacs from Finder or Spotlight, macOS reads `LSEnvironment` from `Info.plist` and sets up the environment before the app starts. Native compilation finds everything it needs inside the app bundle.

## The CLI Wrapper

There's one more wrinkle. When you run `emacs` from the terminal, `LSEnvironment` isn't applied - that only works for GUI launches.

So we create a wrapper script at `/opt/homebrew/bin/emacs` that sets up the environment and then launches the real binary:

``` bash
#!/bin/bash
export LIBRARY_PATH="/Applications/Emacs.app/Contents/Frameworks"
export LD_LIBRARY_PATH="/Applications/Emacs.app/Contents/Frameworks"
# ... more env vars ...
exec "/Applications/Emacs.app/Contents/MacOS/Emacs" "$@"
```

This wrapper also handles finding `Emacs.app` whether it's in `/Applications`, `~/Applications`, or still in the Homebrew Caskroom.

# Custom Icons Still Work

One concern people had: "If it's pre-built, can I still use custom icons?"

Yes! The cask respects your `~/.config/emacs-plus/build.yml`:

``` yaml
icon: modern-doom
```

During `brew install --cask emacs-plus-app`, the postflight script checks your config and applies the icon. This works because icons are just resources - we can swap `Emacs.icns` without recompiling anything.

You can browse all [76 available icons](https://github.com/d12frosted/homebrew-emacs-plus/blob/master/community/icons/README.md) in the gallery.

To change icons after installation:

``` bash
# Update build.yml
echo "icon: spacemacs" > ~/.config/emacs-plus/build.yml

# Reinstall to apply
brew reinstall --cask emacs-plus-app
```

# Formula vs Cask: When to Use Which

| Feature            | Formula          | Cask                  |
|--------------------|------------------|-----------------------|
| Installation time  | 20-40 minutes    | ~1 minute             |
| Native compilation | Yes (needs deps) | Yes (self-contained)  |
| Custom icons       | Yes              | Yes                   |
| Custom patches     | Yes              | No                    |
| Build options      | Full control     | Sensible defaults     |
| Dependencies       | Homebrew-managed | Bundled               |
| Updates            | Manual reinstall | Nightly (`@master`)   |
| Disk space         | Shared libs      | ~500MB self-contained |

**Use the cask** if you want:

- Fast installation
- Native compilation without dependency hassles
- The default feature set (which is quite comprehensive)
- Automatic nightly updates for development builds

**Use the formula** if you need:

- Custom patches (community or your own)
- Specific build options (`--with-poll`, `--without-cocoa`, etc.)
- Minimal disk usage (shared Homebrew libraries)
- A specific git revision

# Platform Support

The cask builds for:

- **macOS 14 (Sonoma)** - ARM64 and Intel
- **macOS 15 (Sequoia)** - ARM64 and Intel
- **macOS 26 (Tahoe)** - ARM64 and Intel

Each build is created in CI on the matching platform to ensure compatibility. The SHA256 checksums in the cask file are updated automatically after each successful build.

# The Naming Scheme

You might have noticed the cask is called `emacs-plus-app`, not `emacs-plus`. This follows the homebrew-core convention where `emacs` is the formula and [`emacs` (cask)](https://github.com/Homebrew/homebrew-cask/blob/master/Casks/e/emacs.rb) is the pre-built app.

The versioning is semantic rather than numeric:

- `emacs-plus-app` - Always the current stable release (Emacs 30 today, 31 when it releases)
- `emacs-plus-app@master` - Always the development branch

This means you don't need to change your install command when a new stable version comes out.

# Finally

I've been maintaining [emacs-plus](https://github.com/d12frosted/homebrew-emacs-plus) for nearly a decade now. Pre-built binaries have been requested for most of that time, and I kept saying no - too complex, too many edge cases, native compilation makes it impossible. Turns out it wasn't impossible, just hard. The cask has been running for a few days now with nightly builds, and the "library not loaded" issues have disappeared for cask users (thanks for reporting, btw).

If you've been compiling Emacs for years and it's worked fine - keep doing that. The formula isn't going anywhere. But if you've been bitten by dependency issues, or you just want Emacs installed in under a minute, this is for you.

``` bash
brew tap d12frosted/emacs-plus
brew install --cask emacs-plus-app
```

Issues and feedback welcome [on GitHub](https://github.com/d12frosted/homebrew-emacs-plus/issues).

------------------------------------------------------------------------

*Next up: [the community patches and icons system](/posts/2026-01-07-emacs-plus-community-system) that makes all those custom icons possible.*
