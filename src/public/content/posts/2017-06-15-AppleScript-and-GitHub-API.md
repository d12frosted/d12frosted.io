AppleScript is a powerful scripting language that allows you to directly control scriptable applications and some parts of macOS. The language itself can be frustrating for those who aren't familiar with the natural language programming paradigm. For example, with AppleScript, you can `tell application "Preview" to quit` or ask someone to `print pages 1 thru 5 of document 2`.

But my goal isn't to promote or demote AppleScript. Instead, I want to share a little script I wrote whilst [experimenting](/being-an-org-mode-addict) with [OmniFocus 2](https://www.omnigroup.com/omnifocus). I hate manually capturing issues that I care about, so I started working on a script that takes the URL from an active tab in Safari and sends it to OmniFocus - into the right project with a useful title and body.

For example, I want [d12frosted/flyspell-correct#30](https://github.com/d12frosted/flyspell-correct/issues/30) to go into the `flyspell-correct` project with title and description taken from that issue.

<!--more-->

Without going into too many details about the full script, I'd like to share the part that communicates with GitHub. Note that this script isn't part of any repository and probably never will be, mostly because it's unfinished and I don't want to maintain full API support. Any feedback is welcome (I'm very new to AppleScript!).

Here's an example of getting the title of `d12frosted/private-repository#42`. First we set up `GitHubClient` by setting the path to [jq](https://stedolan.github.io/jq/) and our credentials. Then we ask it to get a specific issue and select the title from it. The `select` verb accepts any `jq` command. Finally, the `commit` verb does all the dirty work - it sends a request to GitHub and then pipes the result to `jq`.

``` applescript
property jqPath : "/usr/local/bin/jq"
property githubUser : "d12frosted"
property githubAccessToken : "ACCESS_TOKEN"

tell GitHubClient
  setupJQ(jqPath)
  authorise(githubUser, githubAccessToken)
  getIssue("d12frosted", "private-repository", 42)
  select(".title")
  commit()
end tell
```

Here is the implementation of `GitHubClient`.

``` applescript
property _username : missing value
property _access_token : missing value
property _api_url : missing value
property _method : missing value
property _selector : missing value
property _jq_path : "jq"

on authorise(username, access_token)
  set _username to username
  set _access_token to access_token
end authorise

on setupJQ(path)
  set _jq_path to path
end setupJQ

on select (selector)
  set _selector to selector
end select

on commit()
  local prefix, cmd
  set prefix to "export LANG='" & user locale of (system info) & ".UTF-8'; shopt -s compat31; "
  set cmd to "curl "
  if _username is not missing value and _access_token is not missing value then
    set cmd to cmd & "-u " & quoted form of (_username & ":" & _access_token) & " "
  end if
  set cmd to cmd & quoted form of _api_url
  if _selector is not missing value then
    set cmd to cmd & " | " & _jq_path & " " & quoted form of _selector
  end if
  tell me to do shell script prefix & cmd
  return result
end commit

on apiGET(endpoint)
  set _method to "GET"
  set _api_url to "https://api.github.com/" & endpoint
end apiGET
```

This is enough to implement different methods like `getIssue` or `getRepoContributors`.

``` applescript
# API - Repos
# https://developer.github.com/v3/repos

on getMyRepos()
  apiGET("user/repos")
end getMyRepos

on getRepos(owner)
  apiGET("users/" & owner & "/repos")
end getRepos

on getOrgRepos(org)
  apiGET("orgs/" & org & "/repos")
end getOrgRepos

on getRepo(owner, repo)
  apiGET("repos/" & owner & "/" & repo)
end getRepo

on getRepoContributors(owner, repo)
  apiGET("repos/" & owner & "/" & repo & "/contributors")
end getRepoContributors

on getRepoLanguages(owner, repo)
  apiGET("repos/" & owner & "/" & repo & "/languages")
end getRepoLanguages

on getRepoTags(owner, repo)
  apiGET("repos/" & owner & "/" & repo & "/tags")
end getRepoTags

# apiGET - Issues
# https://developer.github.com/v3/issues

on getIssue(owner, repo, issueNumber)
  apiGET("repos/" & owner & "/" & repo & "/issues/" & issueNumber)
end getIssue
```

And so on.

Whilst this implementation works for me, it has several drawbacks:

- If you wish to get several values from an API call, you have to call `commit` for every `select` verb, which means that the same request will be sent multiple times.
- `GitHubClient` only supports authentication via username and access token (no other authentication methods).
