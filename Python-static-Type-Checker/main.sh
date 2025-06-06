#!/bin/bash
#!/usr/bin/env python3

cd "$(dirname "${BASH_SOURCE[0]}")"

red="\x1b[31m"
reset_color="\x1b[0m"
green="\x1b[32m"
yellow="\x1b[33m"
magenta="\x1b[35m"
cyan="\x1b[36m"

debug=""
while getopts "dh" opt; do
    case $opt in
        d)
            debug="d"
            ;;
        h)
            echo "Usage: $0 [-d] <file_path>"
            echo "Options:"
            echo "  -d    Enable debug mode"
            echo "  -h    Show this help message"
            exit 0
            ;;
        *)
            echo "Invalid option: -$OPTARG" >&2
            exit 1
            ;;
    esac
done
shift $((OPTIND -1))

if [ $# -ne 1 ]; then
    echo "Usage: $0 [-d/-h] <file_path>"
    exit 1
fi

file_path=$1
printf "$magenta""Running static type checker on "$green"$file_path$reset_color\n"
printf ""$cyan"1.======= Syntax Check Started...======="$reset_color"\n\n"
if python3 -m py_compile $file_path; then
    printf "Syntax check "$green"passed."$reset_color"\n"
else
    printf "\n"$yellow"Syntax check Failed with  "$red"ERRCODE $?$reset_color\n"
    exit 1
fi

printf "\n"$cyan"2.======= Type Check Started...========="$reset_color"\n\n"
dune exec ./bin/main.exe $file_path $debug
printf "\nType Check Completed.\n"