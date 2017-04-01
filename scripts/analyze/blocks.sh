#!/usr/bin/env bash

for f in node*.log; do
     grep -n 'Created a new block' $f \
	     | sed -r 's/^([0-9]+):\S*\s\S*\s.....(\S*)\s(\S*)\s\S*.\s.*$/\2_\3 \1/' \
	| while read l; do
            echo "$l $f"
     done
done | sort | while read -a l; do
   t=${l[0]}
   n=${l[1]}
   f=${l[2]}
   n2=`tail -n +$((n+1)) $f | grep -nE 'ERROR|WARN|DEBUG|INFO|NOTICE'| head -1 | sed -r 's/^([0-9]*):.*$/\1/'`
   echo -n "[$t] $f: "
   tail -n +$((n+1)) $f | head -n $((n2-1))
done
