#!/usr/bin/env bash
cd APS2
make
cd ..

for f in Samples2/*
do
    echo $f
    cat $f
    echo
    APS2/prologTerm $f | swipl -s APS2/typeChecker.pl -g main_stdin
    if [ $? -eq 0 ]
    then
        APS2/eval $f
    fi
    echo "\n\n"
done

cd APS2
make clean
cd ..