#!/usr/bin/env bash
cd APS1
make
cd ..

for f in Samples1/*
do
    echo $f
    cat $f
    echo
    APS1/prologTerm $f | swipl -s APS1/typeChecker.pl -g main_stdin
    if [ $? -eq 0 ]
    then
        APS1/eval $f
    fi
    echo "\n\n"
done

cd APS1
make clean
cd ..