# Git Cheat Sheet

The following is a cheat sheet for using [git] while developing Cardano SL
which should contain all the git operations that someone working on Cardano
will need at some point.

### Cloning the Cardano SL repository
```
git clone https://github.com/input-output-hk/cardano-sl
```
After you change directory into that cloned repository, you may want to edit
the `.git/config` file which has a section:
```
[remote "origin"]
  url = https://github.com/input-output-hk/cardano-sl
  fetch = +refs/heads/*:refs/remotes/origin/*
```
to which you may want to add a "pushurl" line like this:
```
[remote "origin"]
  url = https://github.com/input-output-hk/cardano-sl
  pushurl = git@github.com:input-output-hk/cardano-sl
  fetch = +refs/heads/*:refs/remotes/origin/*
```
which tells gits to use the HTTPS transport for the "git pull" command, but to
use the SSH protocol for "git push" commands.


### Listing the current and local branches
```
git branch
```
To list all the branches both local and at the origin (usually Github):
```
git branch --all
```

### Create and change into a new branch
Development on Cardano SL is usually done by branching off the "develop" branch.
Assuming you are already on the develop branch and your user name is "xyz" and
you are working on YouTrack ticket number "YT-1234":
```
git branch xyz/YT-1234
git checkout xyz/YT-1234
```
A shorter version of the above is:
```
git checkout -b xyz/YT-1234
```

### Rebasing your changes against develop
While you are working on your branch, it is not uncommon for other developers
to merge their changes into the develop branch. To pull those changes into
your branch (named "xyz/YT-1234" from above):
```
git checkout develop
git pull
git checkout xyz/YT-1234
git rebase develop
```
Occasionally when you rebase against "develop", you will get merge conflicts.
Resolving merge conflicts is explained
[here](https://help.github.com/articles/resolving-a-merge-conflict-using-the-command-line/).

### Editing the commit message of the most recent commit
This is useful if you have just made a commit with just a commit message
title and now want to add more text to the commit message body.
```
git commit --amend
```

### Discarding the current changes
Discarding the current changes to the working tree is simply a matter of
resetting to the current HEAD commit:
```
git reset --hard HEAD
```
Obviously, all the current changes will be lost.
