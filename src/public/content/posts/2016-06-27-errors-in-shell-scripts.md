Writing shell scripts might be pretty tedious because of all those failing commands leading to a corrupted state. And it gets even worse when you overpopulate your script with `if` statements which distract you from solving your initial problem. And fortunately, there are several neat tricks for handling errors in shell scripts that everyone should be familiar with.

<!--more-->

Before we start investigating these tricks, let's define two helper functions - `success` and `failure`. We will use them in our examples to represent abstract commands that always succeeds and always fails respectively.

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

All commands will be executed, even the last `success` despite the fact that `failure`, well, failed. In most cases, this is not what we want. The usual solution is to use `&&` operator that conditionally executes the second command.

``` bash
success &&
failure &&
success

# produces following output:
# success
# failure
# result is 1 (failure)
```

This was quite easy, wasn't it? Such behaviour looks just like imperative code in pretty much any language. Whenever one piece of execution chain fails - the whole chain fails unless we handle it somehow. And here is a fundamental problem about the previous script - you must be explicit about behaviour that should be on by default.

And you'll be pleased to know that it's possible to enable such behaviour. `Sh` has a special exit-on-error mode, which when enabled forces the shell to exit when the simple command returns non-zero status code. In order to enable this mode run `sh` with `-e` option or just call `set -e`.

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

This looks much better. Just for reference, I would like to quote [manual](https://www.gnu.org/software/bash/manual/html_node/The-Set-Builtin.html) for the `set -e`.

> Exit immediately if a pipeline (see [Pipelines](https://www.gnu.org/software/bash/manual/html_node/Pipelines.html#Pipelines)), which may consist of a single simple command (see [Simple Commands](https://www.gnu.org/software/bash/manual/html_node/Simple-Commands.html#Simple-Commands)), a list (see [Lists](https://www.gnu.org/software/bash/manual/html_node/Lists.html#Lists)), or a compound command (see [Compound Commands](https://www.gnu.org/software/bash/manual/html_node/Compound-Commands.html#Compound-Commands)) returns a non-zero status. The shell does not exit if the command that fails is part of the command list immediately following a while or until keyword, part of the test in an if statement, part of any command executed in a && or \|\| list except the command following the final && or \|\|, any command in a pipeline but the last, or if the command’s return status is being inverted with !. If a compound command other than a subshell returns a non-zero status because a command failed while -e was being ignored, the shell does not exit. A trap on ERR, if set, is executed before the shell exits.
>
> This option applies to the shell environment and each subshell environment separately (see [Command Execution Environment](https://www.gnu.org/software/bash/manual/html_node/Command-Execution-Environment.html#Command-Execution-Environment)), and may cause subshells to exit before executing all the commands in the subshell.
>
> If a compound command or shell function executes in a context where -e is being ignored, none of the commands executed within the compound command or function body will be affected by the -e setting, even if -e is set and a command returns a failure status. If a compound command or shell function sets -e while executing in a context where -e is ignored, that setting will not have any effect until the compound command or the command containing the function call completes.

Just as a side note, there are several other useful options, like `-u` (to fail when any undefined variable is referenced) and `-x` (to print each command before it's executed).

Using exit-on-error mode helps to mitigate lots of problems caused by executing commands in a corrupted state. If you ever need to disable it after enabling it - just call `set +e` (note `+` instead of `-`).

There is also another problem that must be solved. Sometimes we need to handle specific signal (like pressing `C-c` to stop script execution). Let's investigate pretty classical example.

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

As a result of running this script - there will be a temporary file that wasn't removed due to the failure during execution. The good thing is that it's possible to handle even such problem using `trap` command which allows performing an action when a specific signal is received. You can check the list of supported signals by calling `trap -l`.

``` bash
set -e

TMPFILE="tmp_file"

function cleanup () {
  echo "removing $TMPFILE"
  rm -f $TMPFILE
}

# execute cleanup function whenever INT, TERM or EXIT singal is received
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

If you press `C-c`, the program immediately stops the execution, but the `cleanup` function is still called. It is also called when `failure` is executed. And this is really nice.

Note that you can only set one trap per signal. If you set a new trap you're implicitly disabling the old one. You can also disable a trap by specifying - as the argument, like this:

``` bash
trap - INT TERM EXIT
```

# Conclusion

As you can see, the shell provides several good tricks for dealing with errors and corrupted state. In simple situations, it's good enough to just use `&&` and `||` operators, but with more complicated problems exit-on-error mode combined with `trap` command is a very powerful tool.
