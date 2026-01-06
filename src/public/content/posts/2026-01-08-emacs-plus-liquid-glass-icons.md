macOS 26 (Tahoe) shipped with a new visual language Apple calls "liquid glass" - translucent surfaces, layered depth, light and shadow that respond to the environment. The Dock icons are part of this, and they require a completely new format.

If you've upgraded to Tahoe and noticed your Emacs icon looks… off, this post explains why, and what we're doing about it in `emacs-plus`.

# The Icon Jail

On Tahoe, apps with traditional `.icns` icons don't get the liquid glass treatment. Instead, macOS displays them in what I've started calling "icon jail" - the icon is rendered smaller, centred inside a rounded square container with a subtle border.

It's not broken, exactly. The icon still works. But it looks out of place next to native apps with their translucent, glowing icons. Your Emacs icon sits there like a tourist who didn't get the dress code memo.

The technical reason: Tahoe prioritises `Assets.car` (compiled asset catalogs) over `.icns` files. If an app only has `.icns`, the system falls back to the jail treatment.

# Why Not Just Convert Everything?

The obvious solution: write a script that converts all 76 community icons to the new format. Problem solved, right?

I tried this. The results were bad.

The liquid glass aesthetic isn't just a file format change. It's a design language with specific requirements:

- **Layered depth** - Icons should have distinct foreground, midground, and background layers that the system can composite with translucency effects
- **Light response** - Elements should react to the system's lighting model, appearing to glow or shadow based on context
- **Translucency** - Parts of the icon should be semi-transparent, letting the Dock's blur show through
- **Specific geometry** - Apple's guidelines specify padding, safe zones, and shape constraints

When you take an icon designed as a flat 2D image and mechanically convert it to `Assets.car`, you get… a flat 2D image in a fancy container. The system applies its lighting effects to a shape that wasn't designed for them. Edges look wrong. Colours shift unexpectedly. Transparency appears where it shouldn't.

The `dragon` icon, for example, has fine details in the dragon's scales and the letter E. Automatic conversion turned it into a muddy blob with weird halos. Not acceptable.

# What Proper Conversion Requires

Making a Tahoe-native icon requires manual work with Apple's tools:

## Icon Composer

Apple provides Icon Composer (part of Xcode's developer tools) for creating `.icon` bundles - the source format that compiles to `Assets.car`. This isn't a batch converter; it's a design tool.

An `.icon` bundle contains:

- `icon.json` - Configuration specifying layers, effects, and metadata
- Multiple PNG files at different resolutions
- Layer definitions for foreground, background, and optional midground elements

## The Layer Model

A well-designed Tahoe icon typically has:

1.  **Background layer** - A shape (often a rounded rectangle or circle) that receives the liquid glass blur effect
2.  **Foreground layer** - The main icon artwork, which can have its own transparency and glow properties
3.  **Optional midground** - Additional elements that sit between background and foreground

Each layer can specify:

- Opacity and blend modes
- Whether it should receive system lighting effects
- Edge treatments and shadows

## Compilation

Once you have an `.icon` bundle, you compile it with `actool`:

``` bash
actool icon.icon --compile . --app-icon Emacs \
  --enable-on-demand-resources NO \
  --minimum-deployment-target 26.0 \
  --platform macosx \
  --output-partial-info-plist /dev/null
```

This produces `Assets.car`, which goes into `Emacs.app/Contents/Resources/`.

# The Two Tahoe Icons

Given the manual effort required, we currently have two Tahoe-compatible icons in the gallery:

## liquid-glass

Contributed by [leaferiksen](https://github.com/leaferiksen), this icon was designed from scratch for the liquid glass aesthetic. It embraces the translucency and depth effects rather than trying to adapt an existing design.

## dragon-plus

This one's mine. The original `dragon` icon - an E styled as a dragon, reminiscent of the official Emacs artwork - has been my favourite since I first added it to the gallery. When Tahoe shipped and it ended up in icon jail, I decided to attempt a proper conversion.

I'm not a designer. My process was mostly trial and error in Icon Composer, adjusting layers until it stopped looking obviously wrong. The result won't win any design awards, but it integrates properly with the Dock - the dragon breathes with the same liquid glass glow as Safari and Finder.

# Using Tahoe Icons

If you're on macOS 26 and want proper icon integration:

``` yaml
# ~/.config/emacs-plus/build.yml
icon: dragon-plus
# or
icon: liquid-glass
```

Then install or reinstall:

``` bash
brew install emacs-plus@31
# or
brew reinstall --cask emacs-plus-app
```

The installation process detects if the icon includes `Assets.car` and copies it to the right place. On older macOS versions, the system ignores `Assets.car` and falls back to the bundled `.icns`, so these icons work everywhere.

# Why We Don't Auto-Convert

Some users have asked why we don't just run all icons through a conversion script, even if the results aren't perfect. "Surely something is better than icon jail?"

I disagree. A bad liquid glass icon is worse than icon jail because it sets wrong expectations. Icon jail is clearly "this app hasn't been updated for Tahoe." A poorly-converted liquid glass icon is "this app tried and failed." The former is honest; the latter is jarring.

If you want your icon to integrate with Tahoe, use one of the two properly-designed options. If you prefer a different icon that hasn't been converted yet, accept the jail for now - it's not that bad, and it's better than a mangled conversion.

# Contributing Tahoe Icons

If you're a designer (or willing to learn Icon Composer), we'd welcome more Tahoe-compatible icons. The requirements:

1.  Create an `.icon` bundle using Icon Composer
2.  Include proper layer separation (background + foreground at minimum)
3.  Test on actual Tahoe hardware - the simulator doesn't fully replicate the lighting model
4.  Provide both the `.icon` source and compiled `Assets.car`
5.  Include light and dark mode previews

The [icons gallery README](https://github.com/d12frosted/homebrew-emacs-plus/blob/master/community/icons/README.md) has technical details on the metadata format.

This is genuinely skilled work. If you have design experience and want to convert your favourite icon properly, the community would benefit. But please don't submit mechanical conversions - they'll be rejected.

# Looking Forward

Two icons isn't many, but it's a start. As more people upgrade to Tahoe and feel the icon jail pain, I expect we'll see more contributions. The infrastructure is ready; we just need artists.

In the meantime, if you're on Tahoe: `dragon-plus` and `liquid-glass` are there for you. If you're on an older macOS: everything works as before, and you can safely ignore all of this until you upgrade.

``` yaml
icon: dragon-plus
```

------------------------------------------------------------------------

*This post is part of a series on recent `emacs-plus` changes. See also: [pre-built binaries via cask](/posts/emacs-plus-cask) and [the community patches and icons system](/posts/emacs-plus-community-system).*
