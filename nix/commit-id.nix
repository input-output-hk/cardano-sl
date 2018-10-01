# Get the commit id of a git repo
# Example: commitIdFromGitRepo ./.git
#
# This is an improved version of the one from <nixpkgs/lib/sources.nix>
# with support for git worktrees.

{ lib }:
with builtins;

let
  readCommitFromFile = path: file:
    let fileName       = toString path + "/" + file;
        packedRefsName = toString path + "/packed-refs";
    in if lib.pathExists fileName
       then readCommitFromRefFile path fileName
       # Sometimes, the file isn't there at all and has been packed away in the
       # packed-refs file, so we have to grep through it:
       else if lib.pathExists packedRefsName
         then readCommitFromPackedRefsFile path packedRefsName
         else throw ("Not a .git directory: " + path);

  readCommitFromRefFile = path: fileName:
    let fileContent = lib.fileContents fileName;
        # Sometimes git stores the commitId directly in the file but
        # sometimes it stores something like: «ref: refs/heads/branch-name»
        matchRef    = match "^ref: (.*)$" fileContent;
    in if   isNull matchRef
       then fileContent
       else readCommitFromFile path (lib.head matchRef);

  readCommitFromPackedRefsFile = path: packedRefsName:
    let fileContent = readFile packedRefsName;
        matchRef    = match (".*\n([^\n ]*) " + file + "\n.*") fileContent;
    in if   isNull matchRef
       then throw ("Could not find " + file + " in " + packedRefsName)
       else lib.head matchRef;

  # Inside a worktree gitdir there is a pointer to the common gitdir
  # and a symbolic reference to the worktree branch.
  readCommitFromGitDir = path: gitDir:
    let
      commonDir = lib.fileContents (gitDir + "/commondir");
      refFile = gitDir + "/HEAD";
    in
      readCommitFromRefFile (gitDir + "/" + commonDir) refFile;

  # .git may be a file containing a pointer to the worktree
  readCommitFromWorkTreeFile = path: 
    let
      fileContent = readFile path;
      matchGitDir = match ("^gitdir:[ \t]*([^\n]*).*") fileContent;
    in if isNull matchGitDir
      then throw ("Not a .git directory: " + path)
      else readCommitFromGitDir path (lib.head matchGitDir);

in
  path:
    if lib.pathType path == "regular"
      then readCommitFromWorkTreeFile path
      else readCommitFromFile path "HEAD"
