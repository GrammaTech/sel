#!/bin/bash
PROG=$1
SCRIPT_DIR="$(cd "$(dirname "${BASE_SOUCE[0]}" )" && pwd)"

run_it() {
    $PROG $@; }

case $2 in
    1) run_it 5 ;;
    2) run_it 10 ;;
    3) run_it -1 ;;
    4) run_it 20 ;;
esac
