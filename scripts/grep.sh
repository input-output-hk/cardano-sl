# This utility script is written by @gromak and is provided "as is".
# The author doesn't guarantee anything about it. It might work.

grep --color=auto -r "$@" lib/src lib/test lib/bench core/Pos update/Pos db/Pos lrc/Pos infra/Pos ssc/Pos tools/src txp/Pos auxx/src auxx/*.hs wallet node/*hs explorer/*.hs --exclude-dir='.stack-work'
# grep -r --exclude-dir={.stack-work,.git} "$@" .
