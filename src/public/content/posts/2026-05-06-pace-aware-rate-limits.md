Claude Code exposes rate-limit data in the statusline JSON: a used percentage and a reset timestamp for each window - a 5-hour rolling one and a weekly one. The obvious thing to do is print the number, color it by threshold, done.

I did that first. Then I noticed how misleading it was.

5% of a weekly limit looks fine. But if you're an hour into the week, you're on track for 840% by reset. The raw number tells you what you've already spent. It does not tell you whether you're about to blow through the budget.

So I made the statusline project this value.

``` example
ctx 19% · 5h 27% → 0% (now) · wk 5% → 3% (now)
```

Each rate-limit segment shows the raw value and the linear extrapolation to the end of the window. The arrow says "if you keep going at this pace." The color follows the projection, not the raw.

# The math

``` example
projected = used% × (window / elapsed)
```

That's the whole idea. If you've used 10% in a quarter of the window, you'll use 40% by the end. Thresholds then apply to the projection: ≥100% red, ≥80% yellow, otherwise green.

In bash:

``` bash
elapsed=$(( window - (reset - now) ))
projected=$(( p * window / elapsed ))
```

`window` is a constant - 18000 seconds for the 5-hour window, 604800 for the weekly one. `reset` is the unix timestamp the host reports.

# Just-started windows

The first few minutes of any window break the math. If 1% of the window has elapsed and you've used 1%, the projection says you'll hit 100% - which is meaningless, because the rounding noise on the elapsed fraction is wider than the signal.

So when elapsed is less than 5% of the window, I skip the projection entirely and fall back to coloring on the raw value:

``` bash
if (( elapsed >= window / 20 )); then
  projected=$(( p * window / elapsed ))
  ...
fi
```

For the 5-hour window that's 15 minutes. For the weekly, ~8 hours. Until then, the projection is too noisy to be useful.

# Bogus values right after launch

Occasionally - usually right after Claude Code starts - the host feeds a 10-digit unix timestamp where the percentage should be. Probably a stale field from another payload getting misread.

The fix is a regex sanity check before doing anything:

``` bash
[[ "$p" =~ ^[0-9]+$ ]] || return
(( p > 100 )) && return
```

If the value isn't a sensible 0..100 integer, skip the segment. The next refresh fills it in.

# Capping the projection

If you fire off something heavy in the first 30 seconds of a fresh weekly window, the projection genuinely is 5000%. It's correct, but unhelpful and would wrap the statusline. So:

``` bash
(( projected > 999 )) && projected=999
```

The `5h 27% → 0% (now)` you see in the screenshot is the other end of the same problem. The reset has already passed, so `reset - now` goes negative and elapsed exceeds the window, dragging the projection below the raw value. At that point the projection is meaningless anyway - the window is over, the statusline just hasn't refreshed yet.

# The whole segment function

Putting it together:

``` bash
seg() {
  local label=$1 pct=$2 reset=$3 window=$4
  [ -z "$pct" ] && return
  local p=${pct%.*}
  [[ "$p" =~ ^[0-9]+$ ]] || return
  (( p > 100 )) && return
  local color="" projected=""

  if [ -n "$reset" ] && [ -n "$window" ]; then
    local elapsed=$(( window - (reset - now) ))
    if (( elapsed >= window / 20 )); then
      projected=$(( p * window / elapsed ))
      if   (( projected >= 100 )); then color="$C_RED"
      elif (( projected >=  80 )); then color="$C_YELLOW"
      else                              color="$C_GREEN"
      fi
    fi
  fi

  [ -z "$color" ] && color=$(pct_color "$p")

  local out=" ${C_DIM}·${C_RESET} ${C_DIM}${label}${C_RESET} ${color}${p}%${C_RESET}"
  if [ -n "$projected" ]; then
    (( projected > 999 )) && projected=999
    out+="${C_DIM}→${C_RESET}${color}${projected}%${C_RESET}"
  fi
  if [ -n "$reset" ]; then
    out+=" ${C_DIM}($(fmt_reset "$reset"))${C_RESET}"
  fi
  printf "%s" "$out"
}
```

Called once per window:

``` bash
ctx_seg=$(seg "ctx" "$ctx_pct" "" "")
fh_seg=$(seg  "5h"  "$fh_pct"  "$fh_reset" 18000)
wk_seg=$(seg  "wk"  "$wk_pct"  "$wk_reset" 604800)
```

The context segment passes no window - there is no time component to context, the percentage is the truth. The two rate-limit segments get their respective window lengths and project against them.

# Why bother

Most of the time the projection just confirms what you already know. But every so often it catches something the raw number wouldn't:

- "I'm at 30% an hour in" - raw says green, projection says red. Slow down.
- "I'm at 60% but the window resets in twenty minutes" - raw says yellow, projection says green. Fine.

Same data, different decision.

The statusline should tell you what's about to happen, not what already did.

------------------------------------------------------------------------

*The full statusline script lives in [my dotfiles repository](https://github.com/d12frosted/environment/tree/master/claude) alongside the rest of my Claude Code configuration.*
