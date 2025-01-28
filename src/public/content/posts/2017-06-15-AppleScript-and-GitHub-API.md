AppleScript is a powerful scripting language that allows to directly control scriptable applications and some parts of macOS. The language itself can be a little bit frustrating for those who are not familiar with natural language programming paradigm. For example, with AppleScript, you can `tell application "Preview" to quit` or ask someone to `print pages 1 thru 5 of document 2`.

But my goal is not to promote or demote AppleScript. Instead, I wish to share a little script that I've fetched while [experimenting](/being-an-org-mode-addict) with [OmniFocus 2](https://www.omnigroup.com/omnifocus). I hate to manually capture issues that I care about, so I started to work on a script that takes URL from an active tab in Safari and sends it to OmniFocus into the right project with useful title and body.

For example, I want [d12frosted/flyspell-correct#30](https://github.com/d12frosted/flyspell-correct/issues/30) to go into the `flyspell-correct` project with title and description taken from that issue.

<!--more-->

Without going into too many details about irrelevant parts of that script, I just wish to share the part that communicates with GitHub. Note that this script is not part of any repository and I doubt that it ever will. Mostly because it's unfinished and I don't want to maintain full API support. And any feedback is welcome (I am so very new to AppleScript!).

Here is an example of getting the title of `d12frosted/private-repository#42`. First we setup `GitHubClient` by setting the path to [jq](https://stedolan.github.io/jq/) and by setting credentials. Then we ask it to get a specific issue and select from it the title. Select verb accepts any(?) `jq` command. The latest verb `commit` does all the dirty job - sends a request to GitHub and then pipes the result to `jq`.

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

Here is the implementation of `GitHubClient`.

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

This is enough to implement different methods like `getIssue` or `getRepoContributors`.

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

And so on.

While this implementation works for me, it has several drawbacks.

- If you wish to get several values from API call, you have to call `commit` for every `select` verb. It means that the same request will be sent multiple times.
- `GitHubClient` doesn't support any other authentication method except for login-pass(token) pair.
