<img src="/images/2017-06-13-Fish-notify-me-when-you-finish/2022-07-19-17-44-48-b98438ae-65f5-11e7-975f-0bbc94238d24.webp" class="d12-image-3/4" />

Have you ever run `git fetch`, stared at the screen for a few seconds, then switched to your browser to read something 'useful' whilst `git` fetches updates? Five minutes later you think, 'Oh wait, I was doing something important, wasn't I?' Ring a bell?

<!--more-->

At some point in my life[^1], I decided to fix this problem. The idea is simple: send a notification whenever Fish completes executing a long-running command. This has been part of [my configurations](https://github.com/d12frosted/environment/tree/master/fish) for a long time, and I've only now decided to share it more openly. Shame on me!

Fish already exposes a variable called `CMD_DURATION` that returns the duration of the previous command execution in milliseconds. This makes it straightforward to create a solution we can put into the `fish_prompt` function.

``` fish
function __d12_prompt__check_duration
  if test $CMD_DURATION
    if test $CMD_DURATION -ge $cmd_notification_threshold
      __d12_prompt__on_duration_exceeded $CMD_DURATION
      __d12_prompt__notify_completion $CMD_DURATION
    end
  end
  set CMD_DURATION 0
end

function __d12_prompt__on_duration_exceeded -a duration
  set_color $fish_color_command
  echo -esn '  ~> duration: '
  set_color $fish_color_param
  echo -es $duration ' ms'
  set_color normal
end

function __d12_prompt__notify_completion -a duration
  if command -v terminal-notifier > /dev/null
    echo -es 'Finished in ' $duration ' ms' | terminal-notifier
  end
end
```

Just call `__d12_prompt__check_duration` at the very beginning[^2] of `fish_prompt`. Then set the value of `cmd_notification_threshold` to the minimum number of milliseconds before a notification is sent. A good place for setting that value is `config.fish`.

Good luck, and don't procrastinate too much!

P.S. The current implementation of `__d12_prompt__notify_completion` targets macOS users. Make sure you modify it to work with your system.

[^1]: Just after I realised that I'd failed to master meditation techniques that would help me stare into the terminal window until command execution completes without being distracted by other things.

[^2]: But after you cache the `$status` value.
