# This utility script searchs prints lines matching a pattern in
# Haskell source code.
# N.B. If you want to find something in all files in the repostiory,
# consider using `git grep'.

# grep --color=auto -r "$@" lib/src lib/test lib/bench core/Pos update/Pos db/Pos lrc/Pos infra/Pos ssc/Pos tools/src txp/Pos auxx/src auxx/*.hs wallet node/*hs explorer/src --exclude-dir='.stack-work'
grep --color=auto -r --exclude-dir={.stack-work,.git} --include \*.hs "$@" *
