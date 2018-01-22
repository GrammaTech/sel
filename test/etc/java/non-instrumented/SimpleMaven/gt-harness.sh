#!/bin/bash
#
# Usage: bug-injector.sh COMMAND [OPTIONS]
#
# Support script for using SimpleMaven with BugInjector.
# This script should be passed as the TEST_SCRIPT
# parameter to BugInjector.
#
# Commands:
#  clean ----------------- Clean SimpleMaven build
#  build ----------------- Build project
#  test [PATH] [NUM] ----- Execute test NUM (max 9903) on the binary
#                          at PATH (sqlite3 or an instrumented variant)

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

help(){
    cat "$0" \
        |sed '/^[^#]/q' \
        |head -n -1 \
        |tail -n +3 \
        |sed -e :a -e '/^\n*$/{$d;N;ba' -e '}' \
        |cut -c3- >&2
    exit 1;
}

build(){
    cd $DIR 1>/dev/null 2>/dev/null
    mvn package 1>/dev/null 2>/dev/null
    exitcode=$?

    cd - 1>/dev/null 2>/dev/null
    exit $exitcode
}

clean() {
    cd $DIR 1>/dev/null 2>/dev/null
    mvn clean
    cd - 1>/dev/null 2>/dev/null
}

test(){
    $DIR/test.sh $1 $2
}

if [ $# -lt 1 ]; then
    help
fi

COMMAND=$1
shift

case $COMMAND in
    help) help ;;
    build) build ;;
    clean) clean ;;
    # test) test $1 $2;;
esac
