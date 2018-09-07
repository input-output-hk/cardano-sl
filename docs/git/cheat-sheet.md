# Git Cheat Sheet

The following is a cheat sheet for using [git] while developing Cardano SL
which should contain all the git operations that someone working on Cardano
will need at some point.

### Cloning the Cardano SL repository
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
your branch:
```
git fetch origin
git rebase origin/develop
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

### Rewriting Git History
This is something that should only ever be done on feature and/or PR branches. However
during development or bug fixing in your branch, it it a good idea to do regular
work-in-progess commits so you don't accidentally lose your work or if you need to
temporarily change to another branch. If you do make work-in-progress commits, you will
need to clean and rewrite up the git history before you submit your PR.

Rewriting git history is explained [here](https://git-scm.com/book/en/v2/Git-Tools-Rewriting-History).
