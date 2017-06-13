#!/bin/bash

for arg; do
	echo removing $arg
	rm -f **/"$arg"
done

IFS=$'\r\n'
gitignore=($(cat .gitignore))
unset IFS
for file in "${gitignore[@]}"; do
	echo rm -f src/$file
	rm -f src/$file
done

