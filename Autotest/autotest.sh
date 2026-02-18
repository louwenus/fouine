#!/bin/bash

# une seule option, -v pour la verbosité de la sortie
# le premier argument est un choix parmi T1 T2 T3 T31 T4 T5 T6 T7 (cf. ci-dessous)
# le second argument est le fichier exécutable (fouine)

# usage: ./autotest.sh [-v] [T1T2T3T31T4T5T6T7] fouine

# This script compares the outputs of ocaml and fouine on the files of the
# relevant folders and print the files for which they differ in failures.txt
# The subfolders ManualOutput contains files that are not compiled by Ocaml. The
# fouine program should then output the content of file.out when executed on
# file.ml

# support verbose mode
while getopts "v" OPTION
do
  case $OPTION in
    v) _V=1
       ;;
  esac
done

function log () {
    if [[ $_V -eq 1 ]]; then
        echo -e "$@"
    fi
}
function logcat () {
    if [[ $_V -eq 1 ]]; then
        echo -n "'";cat "$@"; echo "'"
    fi
}

shift $((OPTIND-1))


case "$1" in
1) Folders=( "T1-Coeur" );;
2) Folders=( "T1-Coeur" "T2-LetIn" );;
3) Folders=( "T1-Coeur" "T2-LetIn" "T3-Fonctions" );;
31) Folders=( "T1-Coeur" "T2-LetIn" "T3-Fonctions" "T31-RecDef" );;
4) Folders=( "T1-Coeur" "T2-LetIn" "T3-Fonctions" "T31-RecDef" "T4-References" );;
5) Folders=( "T1-Coeur" "T2-LetIn" "T3-Fonctions" "T31-RecDef" "T4-References" "T5-Exceptions" );;
6) Folders=( "T1-Coeur" "T2-LetIn" "T3-Fonctions" "T31-RecDef" "T4-References" "T5-Exceptions" "T6-Couples" );;
7) Folders=( "T1-Coeur" "T2-LetIn" "T3-Fonctions" "T31-RecDef" "T4-References" "T5-Exceptions" "T6-Couples" "T7-Listes" );;
*) echo "ERROR: Wrong first argument: $1 should be in [1 2 3 31 4 5 6 7]";exit 1;;
esac

if [[ -x "$2" ]]
then
    Exe=$2
else
    echo "ERROR: Second argument '$2' is not executable or found"; exit 1
fi



function check_output(){
    log "process file: $i";
    
    timeout 20s ./"$Exe"  "$i" > /tmp/out.txt;
    
    if ! (cmp -s /tmp/ref.txt /tmp/out.txt)
    then
        echo -e "\twrong output in file: $i"; echo "$i" >> failures.txt;
        log "\t Ref output:";
        logcat /tmp/ref.txt;
        log "\t Fouine output:";
        logcat /tmp/out.txt;
        log "\t Ocaml source:"
        logcat /tmp/source.ml;
    fi
}


    
rm -f failures.txt; 
for fold in ${Folders[*]}; do

    echo "$fold Folder";
    
    for i in `ls $fold/*ml`; do
    
        cat preludeCaml.ml $i > /tmp/source.ml;
        ocaml -w -A /tmp/source.ml > /tmp/ref.txt
        
        check_output;

        
        timeout 20s ./"$Exe" "-showsrc" "$i" | cat preludeCaml.ml - > /tmp/source.ml;
        ocaml -w -A /tmp/source.ml > /tmp/ref.txt

        check_output;
        
    done;
    
    
    for i in `ls $fold/ManualOutput/*ml 2>/dev/null`; do
        
        cp "${i%.*}.out" /tmp/ref.txt
        
        check_output;
        
    done;
    
    
    for i in `ls $fold/ShouldFail/*ml 2>/dev/null`; do
        
        log "process file: $i";
        rm -f /tmp/out.txt;
        ./"$Exe"  "$i" > /tmp/out.txt 2>&1;
        if !(grep --quiet "rror" /tmp/out.txt)
        then 
            echo -e "\tNo error in bug file: $i"; echo $i >> failures.txt;
            log "\t Fouine output:";
            logcat /tmp/out.txt;
            log "";
        fi
        
        
    done;
    
done;
