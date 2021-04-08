#!/usr/bin/env bash
cd APS0
make
cd ..

for f in Samples0/*
do
    echo $f
    cat $f
    echo
    APS0/prologTerm $f | swipl -s APS0/typeChecker.pl -g main_stdin
    if [ $? -eq 0 ]
    then
        APS0/eval $f
    fi
    echo "\n\n"
done

cd APS0
make clean
cd ..