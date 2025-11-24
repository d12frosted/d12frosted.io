Despite my rather sporadic involvement lately, `emacs-plus` is alive and well. We recently fixed [a nasty PATH injection bug on macOS 15](https://github.com/d12frosted/homebrew-emacs-plus/pull/841) that was causing native compilation to fail for many users. And now I'm finally dusting off a feature that's been sitting on my shelf for almost a year: proper Finder integration for `emacsclient`.

If you run Emacs as a daemon, you've probably noticed that `emacs-plus` only shipped `Emacs.app` - a regular macOS application with no awareness of the server/client setup. There was simply no way to use `emacsclient` from Finder.

After several iterations and a bit of AppleScript wrestling, I'm happy to announce that `emacs-plus` now ships with `Emacs Client.app` - a proper macOS application that makes `emacsclient` a first-class Finder citizen. And it works exactly how you'd expect.

# The Problem

Running Emacs as a daemon is great for performance - instant frame creation, persistent state, no startup time. But macOS integration has always been rough:

- Right-click → "Open With" doesn't work with command-line tools
- Drag-and-drop onto a shell script icon? Not happening
- Setting `emacsclient` as your default text editor? Forget about it

The core issue is that macOS communicates file-opening requests through **AppleEvents**, not command-line arguments. When you double-click a file or use "Open With", macOS sends an `application:openFiles:` event to the target application. Shell scripts can't receive these events - they only see arguments passed via the command line.

# The Journey: Shell Script → AppleScript

My first attempt used a simple shell script wrapper bundled as an app. It looked promising in testing, but the moment you tried "Open With" from Finder… nothing. The script would launch, but it had no idea which file to open.

After digging through Apple's documentation and finding some prior art (shoutout to [Nicholas Kirchner's implementation](https://github.com/NicholasKirchner/Emacs_Client_For_OSX)), the solution became clear: **AppleScript**.

## Why AppleScript?

I evaluated four approaches:

| Approach     | Handles Finder Events | Complexity | Build Requirements      |
|--------------|-----------------------|------------|-------------------------|
| Shell Script | ❌ No                 | Very Low   | None                    |
| Swift Binary | ✅ Yes                | Very High  | Xcode, Swift compiler   |
| Automator    | ✅ Yes                | High       | AppleScript + Automator |
| AppleScript  | ✅ Yes                | Low        | Built-in `osacompile`   |

AppleScript won because it:

- Natively handles the `on open` event for files from Finder
- Compiles during installation using the built-in `osacompile` command
- Requires zero external dependencies
- Keeps the formula simple and maintainable

# How It Works

`Emacs Client.app` is built using two AppleScript handlers that cover all the ways you might launch it.

## Handler 1: Opening Files (`on open`)

Triggered when you:

- Right-click a file → "Open With → Emacs Client"
- Drag files onto the Emacs Client.app icon
- Set Emacs Client as default and double-click a file

``` applescript
on open theDropped
  repeat with oneDrop in theDropped
    set dropPath to quoted form of POSIX path of oneDrop
    set pathEnv to "PATH='/opt/homebrew/bin:...' "
    do shell script pathEnv & "/opt/homebrew/bin/emacsclient -c -a '' -n " & dropPath
  end repeat
  tell application "Emacs" to activate
end open
```

**Note:** This example shows paths for Apple Silicon Macs (`/opt/homebrew`). Intel Macs use `/usr/local` instead. The actual implementation uses the correct prefix for your system.

**Key implementation details:**

- `POSIX path of oneDrop` - Converts macOS file aliases to Unix paths
- `quoted form of` - Handles spaces and special characters in filenames
- `emacsclient -c` - Creates a new frame
- **`-a ''`** - Auto-starts the daemon if not running (more on this below!)
- `-n` - Returns immediately without blocking
- `tell application "Emacs" to activate` - Brings the frame to the foreground

## Handler 2: Launching Empty Frame (`on run`)

Triggered when you:

- Launch Emacs Client from Spotlight
- Click it in the Dock
- Double-click it in Finder (without files)

``` applescript
on run
  set pathEnv to "PATH='/opt/homebrew/bin:...' "
  do shell script pathEnv & "/opt/homebrew/bin/emacsclient -c -a '' -n"
  tell application "Emacs" to activate
end run
```

Same idea, just without file arguments - creates a fresh frame.

## PATH Injection

Just like [the PATH injection I added to Emacs.app](/posts/2022-05-12-emacs-plus-path-injection), `Emacs Client.app` respects the `EMACS_PLUS_NO_PATH_INJECTION` environment variable (or the new `build.yml` setting once that ships).

During the build, your `PATH` is captured and injected into the AppleScript source:

``` ruby
path = ENV['PATH'].split("#{HOMEBREW_PREFIX}/Library/Homebrew/shims/shared:").last
escaped_path = path.gsub("'", "\\\\'")
```

This ensures Homebrew-installed binaries are available when launching from Finder or Spotlight, where the environment is minimal.

## The Magic of `-a ''` (Alternate Editor)

Here's the best part: **you don't need to manually manage the daemon anymore**.

The `-a ''` flag tells `emacsclient`:

1.  Try to connect to an existing daemon
2.  If no daemon is running, start one automatically
3.  Then open the file/frame

This is way more reliable than checking daemon status manually. It handles edge cases like:

- Daemon crashed or was killed
- Socket file exists but daemon isn't running
- Multiple Emacs versions installed
- First launch after a reboot

No more `ps aux | grep daemon`, no more "is the server running?" confusion. It just works.

# Installation and Usage

Starting with `emacs-plus@30` and `emacs-plus@31`, the app is built automatically during installation. After installing, create an alias in `/Applications`:

``` bash
osascript -e 'tell application "Finder" to make alias file to posix file "$(brew --prefix)/Emacs Client.app" at posix file "/Applications" with properties {name:"Emacs Client.app"}'
```

Then you can:

1.  **Set as default application**: Right-click any text file → Get Info → Open with → Emacs Client → "Change All…"
2.  **Use "Open With"**: Right-click any file → Open With → Emacs Client
3.  **Drag and drop**: Drag files onto the Emacs Client.app icon in your Dock
4.  **Launch empty frame**: Open from Spotlight (`Cmd+Space`, type "Emacs Client")

# Known Issue: Generic App Icon

There's one cosmetic issue I haven't cracked yet: **the icon shows a generic app icon instead of the Emacs icon**.

The build process tries to replace the default AppleScript droplet icon by:

1.  Copying `Emacs.icns` to `applet.icns` in the app's Resources folder
2.  Updating `CFBundleIconFile` in `Info.plist` to reference `applet`
3.  Removing the default `droplet.icns` and `droplet.rsrc` files

This works on some macOS versions (including macOS 15 Sequoia), but fails on macOS 26 Tahoe for reasons I haven't fully investigated yet. I've tried the usual suspects:

- Touching the app bundle to update modification time

- Running the full Launch Services reset:

  ``` bash
  /System/Library/Frameworks/CoreServices.framework/Frameworks/LaunchServices.framework/Support/lsregister \
    -kill -r -domain local -domain system -domain user
  ```

- Various `PlistBuddy` incantations

None of these helped, so the root cause remains unclear.

If you have experience with macOS icon caching or Launch Services, **I'd love your help**! The relevant code is in `Library/EmacsBase.rb` in the `create_emacs_client_app` method. Check out [the implementation on GitHub](https://github.com/d12frosted/homebrew-emacs-plus).

# What's Next: Community Patches and Icons

This big feature is done (minus the icon glitch), and I'm already planning the next major improvement: **a community patches and icons system** (see [issue \#851](https://github.com/d12frosted/homebrew-emacs-plus/issues/851)).

The idea is simple: instead of committing to maintain dozens of patches and icons indefinitely, I'll provide infrastructure for community contributions through a `build.yml` configuration file:

``` yaml
patches:
  - smooth-cursor        # From community registry
  - my-custom-patch:     # External URL
      url: https://example.com/my.patch
      sha256: abc123...

icon: modern-flat

settings:
  no_path_injection: true
```

This will enable:

- **Three-tier system**: Built-in patches (maintained by me), community patches (maintained by contributors), and wild-west URLs (maintained by users)
- **No long-term commitment**: Community features can be added/removed without my involvement
- **Easy discovery**: A registry of available patches and icons
- **Helper scripts**: Tools to create and test community patches automatically

The goal is to make `emacs-plus` more sustainable whilst still providing the customisation users want. I'll write a detailed post once this lands.

# Help Wanted: Testing and Feedback

This feature just shipped in `emacs-plus@30` and `emacs-plus@31`. If you're using Emacs daemon, please give it a try!

I'm especially interested in:

1.  **Icon rendering**: Does the Emacs Client.app show the correct icon on your system? (Check `/Applications/Emacs Client.app` after creating the alias)
2.  **File type associations**: Can you set Emacs Client as the default app for your preferred file types?
3.  **Daemon auto-start**: Does the `-a ''` magic work reliably for you?
4.  **Edge cases**: Unusual workflows, multiple Emacs versions, custom socket locations, etc.

Report issues or share feedback on [GitHub](https://github.com/d12frosted/homebrew-emacs-plus/issues) or [in the PR discussion](https://github.com/d12frosted/homebrew-emacs-plus/pull/783).

And if you know the secret to making macOS respect custom icons in AppleScript-compiled apps, please, for the love of Emacs, share your wisdom.

------------------------------------------------------------------------

*Thanks to [Nicholas Kirchner](https://github.com/NicholasKirchner) for the prior art on using AppleScript for `emacsclient` integration, and to [@elken](https://github.com/elken) for early testing and feedback.*
