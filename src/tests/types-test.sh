#!/bin/bash

# one optional option: v(erbose)
# first argument is (B)eginners, (I)ntermediate or (A)dvanced
# second argument is the executable file (fouine)

# usage: ./autotest.sh [-v] [BIAT] fouine

# This script compares the outputs of ocaml and fouine on the files of the
# relevant folders and print the files for which they differ in failures.txt
# The subfolders ManualOutput contains files that are not compiled by Ocaml. The
# fouine program should then output the content of file.out when executed on
# file.ml

# support verbose mode
_V=0

function log () {
    #if [[ $_V -eq 1 ]]; then
        echo -e "$@"
    #fi
}
function logcat () {
    if [[ $_V -eq 1 ]]; then
        echo -n "'";cat "$@"; echo "'"
    fi
}

shift $((OPTIND-1))


case "$1" in
B) Folders=( "Beginners" );;
I) Folders=("Beginners" "Intermediate");;
A) Folders=("Beginners" "Intermediate" "Advanced" );;
T) Folders=( "TypeInf/OK" "TypeInf/NO" );;
*) echo "ERROR: Wrong first argument: $1 should be [BIAT]";exit 1;;
esac

if [[ -x "$2" ]]
then
    Exe=$2
else
    echo "ERROR: Second argument '$2' is not executable or found"; exit 1
fi



function check_output(){
    log "process file: $i";
    ./$Exe -showtypes  $i > /tmp/out.txt;
    logcat /tmp/out.txt;
}

#    ./$Exe -showtypes  $i > /tmp/out.txt;

    
rm -f failures.txt; 
for fold in ${Folders[*]}; do

    echo "$fold Folder";
    
    for i in `ls $fold/*ml`; do
        check_output;
    done;
    
    
    for i in `ls $fold/ManualOutput/*ml`; do
        check_output;
    done;
    
    
    for i in `ls $fold/ShouldFail/*ml`; do
        log "process file: $i";
        rm -f /tmp/out.txt;
        ./$Exe  $i > /tmp/out.txt 2>&1;
        
    done;
    
done;

