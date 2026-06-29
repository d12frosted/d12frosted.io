It started, as these things do, with a quiet failure. My `elpa-mirror` cron had stopped pushing snapshots, and when I went looking I found red X's spreading across the CI of almost every Emacs project I maintain. None of them had changed. The world around them had.

This is the story of a single HTTP 429, what it actually meant, and how chasing it turned into a small published Action, a fleet-wide spring cleaning, and a couple of bugs fixed in tooling I had not touched in years.

<!--more-->

# The symptom

Every failing job died in the same place, the step that installs Emacs:

``` text
warning: error: unable to download
  'https://api.github.com/repos/purcell/nix-emacs-ci/commits/HEAD':
  HTTP error 429

{
  "message": "This endpoint is temporarily being throttled. Please try
  again later. For more on scraping GitHub ...",
  "status": "429"
}
```

I lean on the excellent [purcell/setup-emacs](https://github.com/purcell/setup-emacs) for CI, like a lot of the Emacs world does. Under the hood it installs Emacs with Nix:

``` bash
nix profile install --accept-flake-config \
  "github:purcell/nix-emacs-ci#emacs-snapshot"
```

To resolve that `github:` reference, Nix first asks `api.github.com` for the latest commit of `nix-emacs-ci`. That one tiny metadata call is what was getting throttled. The actual Emacs binary download was never the problem.

# What it was not

My first instinct was wrong, and it is worth saying so. I assumed I had blown through my own API rate limit. I had not. From my laptop the exact endpoint returned `200` happily, and my authenticated quota sat at `4829/5000`. The endpoint was healthy, my account was healthy. The 429 only ever happened from inside a GitHub-hosted runner.

So the obvious fix was to authenticate the call. A bigger quota should make the throttle go away, right? I built a throwaway private repo to test it in isolation, and that is where the story turned. Here is what one run printed, hitting the same endpoint twice:

``` text
UNAUTHENTICATED  -> HTTP 200   limit 60,   remaining 56
AUTHENTICATED    -> HTTP 429   limit 5000,  remaining 5000, used 0
```

Read that again. The **authenticated** request was the one that got throttled, with its entire 5000/hour quota completely unused. That is not a quota limit at all. It is GitHub's **secondary**, anti-abuse rate limit, the one whose error text literally mentions scraping. And the secondary limit does not care whether you are authenticated. Feeding Nix a token, in the system `nix.conf`, on the command line, any way I tried, changed nothing.

# Why it hits, and why it hits some of us harder

GitHub-hosted runners share a pool of egress IPs across, well, everyone. Every project on Earth that uses `setup-emacs` resolves the **same** `nix-emacs-ci` endpoint, unauthenticated, from that **same** shared pool. From GitHub's side that looks exactly like a swarm scraping one URL, so the secondary limit kicks in. It is intermittent, because whether your job lands on an already-tripped IP is luck of the draw.

I checked whether this was just me by sampling other public repos that use `setup-emacs`. It was not just me. Independent projects flipped from green to red on the very same day. And in my own matrix builds I could see two jobs at the **identical timestamp**, one passing and one failing, because they landed on different runner IPs. A coin flip, repeated across a fleet.

Which explains why it felt personal. I maintain a lot of small Emacs packages, most of them testing across several Emacs versions. A run goes red if any single job in the matrix loses the coin flip, so with enough repos and enough matrix entries, almost every run was red, even though most individual jobs still passed.

# The fix: stop calling the API

If authentication cannot save you, the answer is to not make the rate-limited call in the first place. `nix-emacs-ci` can be installed from a plain archive tarball instead of the `github:` flake reference:

``` bash
nix-env -iA emacs-snapshot \
  -f https://github.com/purcell/nix-emacs-ci/archive/master.tar.gz
```

That URL is served by `codeload.github.com`, not `api.github.com`, so it never touches the throttled endpoint. Pair it with the project's [emacs-ci Cachix cache](https://emacs-ci.cachix.org) so Emacs is downloaded rather than built, and a clean run installs `emacs-snapshot` in about twenty seconds.

I wrapped exactly that into a tiny composite Action, [d12frosted/setup-emacs](https://github.com/d12frosted/setup-emacs), a throttle-proof drop-in for `purcell/setup-emacs`:

``` yaml
- uses: d12frosted/setup-emacs@v1
  with:
    version: snapshot   # or 29.4, 30.1, 30.2, ...
- run: emacs --version
```

It still stands entirely on Steve Purcell's `nix-emacs-ci` and `setup-emacs`, the only change is swapping the API-based fetch for the tarball. It works on Linux and macOS, and `version` maps to the matching `nix-emacs-ci` attribute. If you are hitting these 429s today, dropping this in is a two line change.

# Spring cleaning the fleet

Once I had a fix, migrating the rest of my Emacs repos was mechanical, one line each. But touching every workflow at once is a good excuse to notice what else had quietly rotted:

- A few projects had genuinely **broken** tests and lint that had been red for a while, hiding behind the noise. The migration surfaced them, so I fixed them.
- Several workflows triggered on both `push` and `pull_request` without scoping `push` to a branch, so every pull request ran its whole matrix **twice**. Easy to miss, easy to fix.
- The version matrices had drifted all over the place, from ancient `27.1` to random combinations. I standardized everything on the latest stable plus `snapshot`, which is the pair I actually care about.

This is the part I did not expect to enjoy. A single forced change across many repositories is a surprisingly effective audit. You see the whole portfolio at once, and the small rough edges you would never open a dedicated PR for suddenly get fixed because you are already there.

# Bonus saga: the mirror itself

The 429 was only the first layer of why `elpa-mirror` had stopped. With Emacs installing again, two more issues surfaced, both predating this whole adventure:

1.  `elpa-clone` crashes on the bleeding-edge Emacs snapshot (it tripped on a `wrong-type-argument` deep in its archive handling). The mirror does not need the newest Emacs, it just needs a working one, so pinning it to a recent stable was the right call.
2.  A regression hid in the mirror script's own logic. When the working path was unset it fell back to `mktemp -d`, which **creates** an empty directory, and a later `[ ! -d "$path" ]` guard then happily skipped cloning the mirror repository. The result was a sync into an empty non-repo: every package looked new (a slow full re-download) and nothing could be committed or pushed. Guarding on `$path/.git` instead fixed it, and incremental runs dropped from tens of minutes back to about three.

I also took the chance to revisit **what** gets mirrored. Org ELPA turned out to be dead (its `archive-contents` is now a 185 byte stub, Org ships via GNU ELPA), so I dropped it. And I finally added the `-devel` archives, the VCS nightly builds of GNU and NonGNU ELPA, which had simply never been there.

One small detour worth recording: the `-devel` archives **do** offer rsync, and I briefly wondered if I should switch to it for speed. I should not. Their rsync modules serve the raw build layout, `.tar.lz` compressed, with every historical nightly retained and no readme files, which is a different thing entirely from the `packages/` presentation layout a mirror is supposed to publish. So plain `https` it is, on purpose.

# Takeaways

- Unauthenticated calls to `api.github.com` from GitHub-hosted runners are a trap. The IP pool is shared, and the secondary rate limit does not care about your token. If a tool you depend on makes such a call per CI run, it will eventually bite you, and authenticating will not save you.
- Reproduce in isolation before you trust your theory. A two step throwaway repo told me, unambiguously, that authentication was a dead end. Without it I would have spent the day adding tokens that could never have worked.
- One thin reusable Action beats N copy-pasted snippets. The fix lives in one place now, and the whole fleet points at it.
- A forced sweep is a free audit. Migrating every workflow at once paid for itself in broken tests found and duplicate runs killed.

If you maintain Emacs packages and your CI has been flaky lately, it is probably this. [d12frosted/setup-emacs](https://github.com/d12frosted/setup-emacs) is there if it helps. And as always, it rests entirely on the work of [Steve Purcell](https://github.com/purcell) and [Desmond O. Chang](https://github.com/dochang), who built the pieces that make any of this possible.
