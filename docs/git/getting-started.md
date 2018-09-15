# Git Getting Started

The following is a getting started guide for using [git] while developing
Cardano SL.

Its probably best to avoid the use of GUIs that try to make the use of [git]
easier. Git is a complex tool with many sub commands and each sub command has
many options. This makes it difficult for a GUI to provide the required
flexibility. Its also probably best to avoid the use of the [gitflow]
extension as it provides zero advantage over a solid intermediate level
understanding of git itself.

Finally git is already relatively customizable and with the addition of custom
shell functions, shell aliases and shell scripts customization is almost
limitless.

In the following, the most standard versions of each command will be used and
when the command uses options, the long option name will be used. It should be
noted however that if the `${HOME}/.gitconfig` below makes the command
"git branch --all" and "git br -a" equivalent.

## Getting Started

You should have a recent version (>= 2.17) of git installed, and done the
required [first time setup](https://git-scm.com/book/en/v2/Getting-Started-First-Time-Git-Setup)
and set up [GPG signing of commits](https://help.github.com/articles/signing-commits-using-gpg/)
in the Cardano repo itself.

You can then clone the Cardano repo using:

```
git clone https://github.com/input-output-hk/cardano-sl
```

After you change directory into that cloned repository, you may want to tell
git to use the SSH protocol for the "git push" command, keeping the HTTPS
protocol for the "git pull" and "git fetch" commands by issuing the following
command:

```
git remote set-url --push origin git@github.com:input-output-hk/cardano-sl
```

This may also be a good time to run the following (replacing XXXXXXXXXXXXXXXX
with your GPG key fingerprint):

```
git config --local commit.gpgsign true
git config --local user.signingkey XXXXXXXXXXXXXXXX
```

to make sure that all commits you make are GPG signed.

If you make a commit and then run:

```
git log --format=raw
```

it should be obvious if your commits are GPG signed.

## Branching from develop

Cardano development happens on the "develop" branch. You can find out what
branch you are one by running:

```
git branch
```

which will list all the branches in your local repository and print an asterisk
to the left of the current branch. You can list all the branches in the
Github repository using:

```
git branch --all
```

Once you are ready to start working on a new feature you should create a branch
to work on that feature. To name space things correctly, its best to name the
branch with a combination of your username and the YouTrack ticket name. Assuming
you are on the "develop" branch, to create the new branch and then change to
it:

```
git branch username/YT-1234
git checkout username/YT-1234
```

You can then work on your changes and commit them using something like:

```
git commit --message="YT-1234: Fixed the wibble"
```

which includes the YouTrack ticket number in the commit message. Its a good
idea not to put too much information in the title of the git commit message.
If you want to add more information you can run:

```
git commit --amend
```

which will bring up you text editor allowing you to add a message body to
the text after the commit message title like for instance
[this commit](https://github.com/input-output-hk/cardano-sl/commit/ddc58940e5ddec357cc94097df2951359cb37ed5)

Once you have committed your change you can use:

```
git show
```

to show the contents of the commit. Similarly, you can use something like

```
git show ddc58940e5
```

to show the contents of the commit whose hash begins with "ddc58940e5"


## Customizing Git

Git has a configuration file at `${HOME}/.gitconfig` which might look like this:

```
[user]
  name = My Name
  email = me@example.com

[core]
  whitespace = trailing-space,space-before-tab
  autocrlf = false

[github]
  user = gh-user-name

[alias]
  # Simple aliases
  br = branch
  co = checkout
  cp = cherry-pick
  st = status

  # More complex aliases
  logv = log --name-status
  logp = "log --graph --pretty=format:\"%h -%d %s (%cr)\" --abbrev-commit --date=relative"
  subup = submodule update

  # Check create a branch off the current branch and then change to it.
  # eg: "git bct my-name/my-topic"
  bct = "checkout -b"

[pull]
  # Make "git pull --rebase" the default behaviour for "git pull"
  rebase = true

[push]
  default = matching

[diff]
  ignoreSubmodules = dirty
  compactionHeuristic = true

[pager]
  status = false
  branch = false
  stash = false
  diff = true
```

Its also possible to define shell functions that string together one of more
git commands. For example, the following shell function when added to the users
`${HOME}/.bashrc` file allows rebase the current branch against the develop
branch on Github:

```
function git-rebase-develop
{
  git fetch origin && git rebase --preserve-merges origin/develop
}
```


[git]: https://git-scm.com/
[gitflow]: https://github.com/nvie/gitflow
