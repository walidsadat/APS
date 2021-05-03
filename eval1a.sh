#!/usr/bin/env bash
cd APS1a
make
cd ..

for f in Samples1a/*
do
    echo $f
    cat $f
    echo
    APS1a/prologTerm $f | swipl -s APS1a/typeChecker.pl -g main_stdin
    if [ $? -eq 0 ]
    then
        APS1a/eval $f
    fi
    echo "\n\n"
done

cd APS1a
make clean
cd ..