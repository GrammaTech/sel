#!/bin/bash
# $1 = EXE
# $2 = test name
# exit 0 = success
ulimit -t 1
basedir=$(dirname $0)
prog=$1
test_equal(){
    result=$($basedir../../bin/lisp-runner $prog "(euclids-gcd $1 $2)")
    if [ "$result" == "$3" ];then exit 0;else exit 1;fi
}
case $2 in
  p1)  test_equal 1071 1029 21 ;;
  p2)  test_equal 555 666 111 ;;
  p3)  test_equal 678 987 3 ;;
  p4)  test_equal 8767 653 1 ;;
  p5)  test_equal 16777216 512 512 ;;
  p6)  test_equal 16 4 4 ;;
  p7)  test_equal 315 831 3 ;;
  p8)  test_equal 513332 91583315 1 ;;
  p9)  test_equal 112 135 1 ;;
  p10) test_equal 310 55 5 ;;
  n1)  test_equal 0 55 55 ;;
esac
