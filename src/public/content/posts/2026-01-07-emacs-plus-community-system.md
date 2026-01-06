One of the recurring tensions in maintaining `emacs-plus` has been customisation. Users want patches, icons, build options. I want to not spend my weekends debugging why someone's obscure patch broke on the latest Emacs commit.

For years, I handled this by saying yes to everything and then regretting it. The formula accumulated dozens of `--with-X-icon` options, each one a potential support burden. Patches would break, users would open issues, and I'd feel obligated to fix things I never wrote.

The community system is my attempt to break this cycle: provide infrastructure for customisation, but make responsibility explicit.

# The Three-Tier Model

``` example
┌─────────────────────────────────────────────────────────────┐
│  Built-in Patches                                           │
│  Maintained by me. Applied unconditionally. Must fix ASAP.  │
├─────────────────────────────────────────────────────────────┤
│  Community Patches & Icons                                  │
│  Maintained by contributors. Opt-in via build.yml.          │
│  No SLA. Can break.                                         │
├─────────────────────────────────────────────────────────────┤
│  Wild-West                                                  │
│  Any external URL. Requires SHA256. Your problem entirely.  │
└─────────────────────────────────────────────────────────────┘
```

## Built-in Patches

These are patches I consider essential for a good macOS Emacs experience. They're applied to every build, and if they break, I fix them immediately.

Current built-in patches include:

- `fix-window-role` - Proper window management behaviour
- `system-appearance` - Respond to macOS light/dark mode changes
- `round-undecorated-frame` - Rounded corners for undecorated frames

If a built-in patch causes issues, that's a bug in `emacs-plus` and I'll address it.

## Community Patches & Icons

This is where things get interesting. The `community/` directory contains patches and icons contributed by users, with clear ownership:

``` json
{
  "name": "window-blur",
  "description": "Window blurring and alpha transparency effects",
  "maintainer": "someuser",
  "versions": ["30", "31"]
}
```

Each contribution has a maintainer. If it breaks, that's who you contact - not me. I provide the infrastructure; the community provides the content.

Currently available community patches:

- `window-blur` - Window blurring and configurable alpha transparency
- `async-process-read` - Improved async process read performance

And 76 icons, ranging from classic designs to modern interpretations to the inevitable Doom and Spacemacs variants.

## Wild-West

For patches and icons that aren't in the registry, you can specify any URL:

``` yaml
patches:
  - my-experimental-patch:
      url: https://example.com/my.patch
      sha256: abc123...

icon:
  url: https://example.com/custom.icns
  sha256: def456...
```

The SHA256 requirement isn't just security theatre - it ensures reproducible builds and catches when upstream URLs change unexpectedly.

# Configuration via build.yml

All customisation happens through `~/.config/emacs-plus/build.yml`:

``` yaml
# Apply community patches
patches:
  - window-blur
  - async-process-read

# Use a community icon
icon: modern-doom

# Pin to a specific revision (optional)
revision:
  "30": abc123def456
  "31": 789xyz
```

This file is read by both the formula and the cask. When you run `brew install emacs-plus@31` or `brew install --cask emacs-plus-app`, the same configuration applies.

For formula builds, patches are applied during compilation. For cask builds, only icons are applied (since the binary is pre-built), but that's usually what people want anyway.

# The Icons Gallery

The most visible part of the community system is the [icons gallery](https://github.com/d12frosted/homebrew-emacs-plus/blob/master/community/icons/README.md) - 76 icons with previews, author attribution, and source links. We also imported icons from [homebrew-emacs-head](https://github.com/daviderestivo/homebrew-emacs-head) to consolidate the community's work in one place.

Two of these icons are designed specifically for macOS 26 Tahoe's liquid glass aesthetic: `liquid-glass` contributed by [leaferiksen](https://github.com/leaferiksen), and `dragon-plus` which I made myself. The original `dragon` icon - an E styled as a dragon, similar to the official Emacs icon - has always been my favourite, so I attempted a Tahoe version. I'm no artist, but it works. More on what "Tahoe-compatible" means [in the next post](/posts/2026-01-08-emacs-plus-liquid-glass-icons).

To use an icon:

``` yaml
icon: spacemacs
```

Then install or reinstall:

``` bash
brew install emacs-plus@31
# or
brew reinstall --cask emacs-plus-app
```

# The Old Way is Gone

The legacy `--with-X-icon` options have been removed:

``` bash
# Old (no longer works)
brew install emacs-plus@31 --with-spacemacs-icon

# New
echo "icon: spacemacs" > ~/.config/emacs-plus/build.yml
brew install emacs-plus@31
```

The new approach is better:

1.  **Persistent configuration** - Your icon choice survives reinstalls
2.  **Works with cask** - The same config applies to pre-built binaries
3.  **Discoverable** - Browse the gallery instead of guessing option names
4.  **Maintainable** - Adding new icons doesn't require formula changes

The `--with-icon=name` option still works as a shorthand, but `build.yml` is the recommended approach.

# Contributing

Adding a new icon or patch is straightforward:

``` bash
# For icons
./scripts/create-community-icon.rb

# For patches
./scripts/create-community-patch.rb
```

The scripts guide you through creating the proper directory structure, metadata, and testing. For patches, they'll verify the patch applies cleanly against each supported Emacs version.

Requirements:

**Icons:**

- `icon.icns` file
- `metadata.json` with name, maintainer, homepage
- `preview.png` (128x128)

**Patches:**

- Version-specific patch files (`emacs-30.patch`, `emacs-31.patch`)
- `metadata.json` with compatible versions
- `README.md` explaining what it does

Once submitted, I'll review for proper structure and obvious issues, but I won't maintain your contribution long-term. That's on you.

# The Maintenance Contract

Let me be explicit about what this system means:

**I will:**

- Maintain the infrastructure (registry, helper scripts, config parsing)
- Review PRs for proper format and no obvious malicious code
- Keep built-in patches working
- Remove abandoned contributions after 3+ months of unresponsiveness

**I will not:**

- Fix your patch when Emacs updates break it
- Debug issues with community contributions
- Guarantee any community feature works

This might sound harsh, but it's the only sustainable model. The alternative is what I was doing before: implicitly promising to maintain everything forever, then burning out.

# The Transformation

This system represents a shift in how I think about `emacs-plus`.

For years, the formula grew by accumulation. Every reasonable request got a `--with-something` flag. Icons, patches, build options - if someone asked nicely and it wasn't obviously broken, it went in. The result was a formula with dozens of options, each one a maintenance liability. Homebrew's own guidelines [discourage excessive options](https://docs.brew.sh/Acceptable-Formulae#we-dont-like-install-options), and for good reason.

The community system inverts this. Instead of me gatekeeping what goes into the formula, the infrastructure is open. Want an experimental patch? Add it to the registry or point to a URL. Want a custom icon? Same thing. The formula stays lean; the customisation lives in `build.yml` and the community directory.

This is better for everyone. Users get more freedom to experiment with patches and icons from around the ecosystem. Contributors get a clear path to share their work. And I get to focus on the core formula without feeling responsible for every edge case.

Browse the [icons gallery](https://github.com/d12frosted/homebrew-emacs-plus/blob/master/community/icons/README.md), check out the [community documentation](https://github.com/d12frosted/homebrew-emacs-plus/blob/master/community/README.md), and if you have something to contribute, [open a PR](https://github.com/d12frosted/homebrew-emacs-plus).

------------------------------------------------------------------------

*Next up: [macOS Tahoe and liquid glass icons](/posts/2026-01-08-emacs-plus-liquid-glass-icons) - why automatic conversion doesn't work and what it takes to make icons look right on macOS 26.*
