#!/bin/bash
# $1 = EXE 
# $2 = test name  
# exit 0 = success
ulimit -t 1
test_equal(){ if [ "$1" == "$2" ];then exit 0;else exit 1;fi }
case $2 in
  p1)  test_equal "$(./lisp-runner $1 1071 1029)" "21" ;;
  p2)  test_equal "$(./lisp-runner $1 555 666)" "111" ;;
  p3)  test_equal "$(./lisp-runner $1 678 987)" "3" ;;
  p4)  test_equal "$(./lisp-runner $1 8767 653)" "1" ;;
  p5)  test_equal "$(./lisp-runner $1 16777216 512)" "512" ;;
  p6)  test_equal "$(./lisp-runner $1 16 4)" "4" ;;
  p7)  test_equal "$(./lisp-runner $1 315 831)" "3" ;;
  p8)  test_equal "$(./lisp-runner $1 513332 91583315)" "1" ;;
  p9)  test_equal "$(./lisp-runner $1 112 135)" "1" ;;
  p10) test_equal "$(./lisp-runner $1 310 55)" "5" ;;
  n1)  test_equal "$(./lisp-runner $1 0 55)" "55" ;;
esac 
