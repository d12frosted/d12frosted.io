Have you ever written a shell script that failed halfway through, leaving your system in a corrupted state? Perhaps a temporary file wasn't cleaned up, or a half-finished operation left things in an inconsistent state. It's frustrating, and the typical solution - littering your script with `if` statements to check every command - quickly becomes unwieldy and distracts from solving the actual problem.

Fortunately, the shell provides elegant built-in mechanisms for handling errors. In this post, I'll show you how to write more robust shell scripts using exit-on-error mode and signal traps.

**What you'll learn:**

- How to use `set -e` to exit immediately when commands fail
- Understanding the `&&` and `||` operators for conditional execution
- Using `trap` to handle cleanup operations reliably
- Best practices for error handling in shell scripts

<!--more-->

Before we dive into the techniques, let's define two helper functions - `success` and `failure`. We'll use these in our examples to represent abstract commands that always succeed and always fail, respectively.

``` bash
# just like true, but also echoes
function success {
  echo "success"
  return 0
}

# just like false, but also echoes
function failure {
  echo "failure"
  return 1
}
```

Now let's implement our first script.

``` bash
success
failure
success

# produces following output:
# success
# failure
# success
# result is 0 (success)
```

All commands will be executed, even the last `success`, despite `failure` having failed. In most cases, this isn't what we want. The usual solution is to use the `&&` operator, which conditionally executes the second command only if the first succeeds.

``` bash
success &&
failure &&
success

# produces following output:
# success
# failure
# result is 1 (failure)
```

This works well and behaves like imperative code in most programming languages - whenever one piece of the execution chain fails, the whole chain fails unless we explicitly handle it. However, there's a fundamental problem: you must be explicit about behaviour that should be enabled by default.

Fortunately, the shell provides a better solution. The `sh` command has a special exit-on-error mode which, when enabled, forces the shell to exit whenever a simple command returns a non-zero status code. You can enable this mode by running `sh` with the `-e` option, or by calling `set -e` within your script.

``` bash
set -e
success
failure
success

# produces following output:
# success
# failure
# result is 1 (failure)
```

This is much cleaner! For reference, here's what the [official manual](https://www.gnu.org/software/bash/manual/html_node/The-Set-Builtin.html) says about `set -e`:

> Exit immediately if a pipeline (see [Pipelines](https://www.gnu.org/software/bash/manual/html_node/Pipelines.html#Pipelines)), which may consist of a single simple command (see [Simple Commands](https://www.gnu.org/software/bash/manual/html_node/Simple-Commands.html#Simple-Commands)), a list (see [Lists](https://www.gnu.org/software/bash/manual/html_node/Lists.html#Lists)), or a compound command (see [Compound Commands](https://www.gnu.org/software/bash/manual/html_node/Compound-Commands.html#Compound-Commands)) returns a non-zero status. The shell does not exit if the command that fails is part of the command list immediately following a while or until keyword, part of the test in an if statement, part of any command executed in a && or \|\| list except the command following the final && or \|\|, any command in a pipeline but the last, or if the command’s return status is being inverted with !. If a compound command other than a subshell returns a non-zero status because a command failed while -e was being ignored, the shell does not exit. A trap on ERR, if set, is executed before the shell exits.
>
> This option applies to the shell environment and each subshell environment separately (see [Command Execution Environment](https://www.gnu.org/software/bash/manual/html_node/Command-Execution-Environment.html#Command-Execution-Environment)), and may cause subshells to exit before executing all the commands in the subshell.
>
> If a compound command or shell function executes in a context where -e is being ignored, none of the commands executed within the compound command or function body will be affected by the -e setting, even if -e is set and a command returns a failure status. If a compound command or shell function sets -e while executing in a context where -e is ignored, that setting will not have any effect until the compound command or the command containing the function call completes.

Just as a side note, there are several other useful options, like `-u` (to fail when any undefined variable is referenced) and `-x` (to print each command before it's executed).

Using exit-on-error mode helps mitigate many problems caused by executing commands in a corrupted state. If you ever need to disable it, simply call `set +e` (note the `+` instead of `-`).

However, there's another problem to address. Sometimes we need to handle specific signals (like pressing `C-c` to stop script execution) or ensure cleanup code runs even when the script fails. Let's look at a classic example.

``` bash
set -e

TMPFILE="path/to/tmp_file"

function cleanup () {
  echo "removing $TMPFILE"
  rm -f $TMPFILE
}

echo "hello world" > $TMPFILE
cat $TMPFILE

# gives user a chance to press CTRL-C
sleep 3

# here we are doing something that might fail
failure
# oh, this always fails

cleanup
```

When you run this script, a temporary file is left behind because the script exits before reaching the cleanup call. Fortunately, we can solve this using the `trap` command, which allows us to perform an action when a specific signal is received. You can check the list of supported signals by calling `trap -l`.

``` bash
set -e

TMPFILE="tmp_file"

function cleanup () {
  echo "removing $TMPFILE"
  rm -f $TMPFILE
}

# execute cleanup function whenever INT, TERM or EXIT signal is received
trap cleanup INT TERM EXIT

echo "Hello World!" > $TMPFILE
cat $TMPFILE

# gives user a chance to press CTRL-C
sleep 3

# here we are doing something that might fail
failure
# oh, this always fails

cleanup
```

If you press `C-c`, the programme immediately stops execution, but the `cleanup` function is still called. It's also called when `failure` is executed. This is exactly the behaviour we want!

Note that you can only set one trap per signal. If you set a new trap, you're implicitly disabling the old one. You can also disable a trap by specifying `-` as the argument, like this:

``` bash
trap - INT TERM EXIT
```

# Conclusion

The shell provides elegant mechanisms for dealing with errors and preventing corrupted states. For simple situations, the `&&` and `||` operators are sufficient. However, for more complex scripts, combining exit-on-error mode (`set -e`) with the `trap` command gives you robust error handling and guaranteed cleanup operations.

I recommend starting every non-trivial shell script with `set -e` and using `trap` for cleanup operations. Your future self (and anyone else running your scripts) will thank you!
