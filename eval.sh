#!/usr/bin/env bash
cd APS2
make
cd ..
echo Samples2/prog203.aps
cat Samples2/prog203.aps
echo
APS2/prologTerm Samples2/prog203.aps | swipl -s APS2/typeChecker.pl -g main_stdin
if [ $? -eq 0 ]
    then
        #APS1a/prologTerm $f | swipl -s APS1a/eval.pl -g main_stdin
        APS2/eval Samples2/prog203.aps
    fi
echo "\n\n"

cd APS2
make clean
cd ..