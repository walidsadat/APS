#!/usr/bin/env bash
cd APS3
make
cd ..

for f in Samples3/*
do
    echo $f
    cat $f
    echo
    APS3/prologTerm $f | swipl -s APS3/typeChecker.pl -g main_stdin
    if [ $? -eq 0 ]
    then
        APS3/eval $f
    fi
    echo "\n\n"
done

cd APS3
make clean
cd ..