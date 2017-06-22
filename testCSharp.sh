#!/bin/bash

EXIT=0
cd "${0%/*}"
#dir

function synth {
    python src/main.py -d --csharp 'Union.cs' --csharp-class 'Union' $1
}

function fail {
    EXIT=1
    echo "*** FAILED ***"
}

function run-test {
    csc /out:My.exe *.cs
    ./My
}

for entry in "examples"/*
do
  if [ $EXIT == 1 ] ; then
    exit "$EXIT"
  fi
  echo "$entry"
  synth "$entry" || fail
  run-test || fail
done



exit "$EXIT"
