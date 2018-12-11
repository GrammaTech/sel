#!/bin/bash
# $1 = EXE
# $2 = test case number
# exit 0 = success

# prevent tests from hanging
ulimit -t 1

# prevent generation of core files
ulimit -c 0

test_equal(){ if [ "$1" == "$2" ];then exit 0;else exit 1;fi }

case $2 in
  # positive tests
  0)  test_equal "$($1 1071 1029)" "21" ;;
  1)  test_equal "$($1 555 666)" "111" ;;
  2)  test_equal "$($1 678 987)" "3" ;;
  3)  test_equal "$($1 8767 653)" "1" ;;
  4)  test_equal "$($1 16777216 512)" "512" ;;
  5)  test_equal "$($1 16 4)" "4" ;;
  6)  test_equal "$($1 315 831)" "3" ;;
  7)  test_equal "$($1 513332 91583315)" "1" ;;
  8)  test_equal "$($1 112 135)" "1" ;;
  9)  test_equal "$($1 310 55)" "5" ;;
  10) test_equal "$($1 55 0)" "55"  ;;
  # negative test
  11) test_equal "$($1 0 55)" "55" ;;
esac
