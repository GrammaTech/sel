#!/bin/bash
#
# Usage: fitness.py [OPTIONS] EXE [TEST-NUM]
#   Run test(s) and report the result(s)
#
# Options:
#  -c,--count -------------- print a count of the number of tests

ARGS=(-5 0 3 4 5 6)

EXPS=("x is smaller than 5"
      "x is smaller than 5"
      "x is smaller than 5"
      "x is smaller than 5"
      "x is exactly 5"
      "x is 5 or larger")

## Parse Options
eval set -- $(getopt -o c -l count -- "$@" || exit 1;)
while [ $# -gt 0 ];do
    case $1 in
        -c|--count) echo ${#ARGS[*]}; exit 0;;
        (--) shift; break;;
        (-*) error "unrecognized option $1";;
        (*)  break;;
    esac
    shift
done

if [ -z $1 ];then
    echo "At least one argument required."
    exit 1
else
    EXE=$1
fi
if [ -z $2 ];then
    declare -a TEST_NUMS
    for i in $(seq ${#ARGS[*]});do
        TEST_NUMS+=($((i - 1)))
    done
else
    TEST_NUMS=($2)
fi

do_test(){
    local exe=$1; shift;
    local arg=$1; shift;
    local exp="$@";
    res=$($exe $arg);
    if [ "$res" == "$exp" ];then
        echo 1;
    else
        echo "'$res' != '$exp'" >&2
        echo 0;
    fi;
}

SCORE=0
for TEST_NUM in ${TEST_NUMS[*]};do
    RES=$(do_test $EXE ${ARGS[$TEST_NUM]} ${EXPS[$TEST_NUM]})
    SCORE=$((SCORE + RES))
done

# Stupid faking of non-integer division for bash expressions.  The
# point is to print the percent of the tests that have passed and then
# exit with zero if the score is 100%.
SCORE=$(((SCORE * 100) / ${#TEST_NUMS[*]}))
if [ $SCORE -eq 100 ];then
    echo 1
    exit 0
elif [ $SCORE -ge 10 ];then
    echo "0.${SCORE}"
else
    echo "0.0${SCORE}"
fi
exit 1
