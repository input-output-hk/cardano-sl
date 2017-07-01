#!/usr/bin/env bash

while getopts u:b:m: option
do
 case "${option}"
 in
 u) USER=${OPTARG};;
 b) BRANCHNAME=${OPTARG};;
 m) MERGE_COMMIT=$OPTARG;;
 esac
done

# user is optional
if [ -z "$USER" ]
  then
    GIT_PROVIDER="github.com"

    # extract user from repo URL
    USER=`echo $REPO_URL | sed -Ene's#https://'${GIT_PROVIDER}'/([^/]*)/(.*).git#\1#p'`
    if [ -z "$USER" ]; then
      echo "-- ERROR:  Could not identify User."
      exit
    fi
fi
if [ -z "$BRANCHNAME" ]
  then
    echo "-- ERROR:  Could not identify branch name."
    exit
fi
if [ -z "$MERGE_COMMIT" ]
  then
    echo "-- ERROR:  Could not identify merge commit."
    exit
fi



echo "git checkout master
git pull --ff-only # make sure you are using latest master
git checkout -b $USER/merge-$BRANCHNAME-to-master # you can choose whatever name you want actually
git merge $MERGE_COMMIT # where MERGE_COMMIT is the merge commit corresponding to your PR
# resolve conflicts
git commit
git push -u origin $USER/merge-$BRANCHNAME-to-master
# make a PR"
