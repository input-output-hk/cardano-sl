#!/usr/bin/env bash

_ppid=$PPID

echo "My pid $$, PPID=$PPID"

if [[ "$1" == "" ]]; then
  v=$(uxterm -e "$(pwd)/$0 --" || echo bla)
  case $v in
    bla)
      echo "Update started"
      curl -X POST --cert ./scripts/tls-files/client.pem -kv https://127.0.0.1:8090/api/update/apply
      sleep 1s
      exit 20
      ;;
    *)
      echo "Normal exit"
      ;;
  esac
else
  while read l; do
    case "$l" in
      update)
        echo "Starting update.."
        sleep 1s
        kill -15 $_ppid
        ;;
      exit)
        echo "Exiting.."
        sleep 1s
        exit 0
        ;;
      *)
        echo "Wrong command! Available: exit update"
    esac
  done
fi

