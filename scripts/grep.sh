# This utility script is written by @gromak and is provided "as is".
# The author doesn't guarantee anything about it. It might work.

grep --color=auto -r "$@" node/src node/test node/bench core/Pos update/Pos db/Pos lrc/Pos infra/Pos ssc/Pos godtossing/Pos tools/src txp/Pos lwallet/Pos lwallet/*.hs wallet explorer/*.hs --exclude-dir='.stack-work'
# grep -r --exclude-dir={.stack-work,.git} "$@" .
