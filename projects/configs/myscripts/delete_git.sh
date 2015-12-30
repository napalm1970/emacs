#!/bin/sh

FOLDER=$1
printf -v MESSAGE  '(Delete folder %s)' $FOLDER

$ echo $FOLDER >> .gitignore
$ git rm -r --cached $FOLDER
$ git add .gitignore
$ git commit -m $MESSAGE
$ git push
