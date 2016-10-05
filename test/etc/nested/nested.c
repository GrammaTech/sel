#include <stdio.h>

void main(void) {
    int i = 0;
    int j = 1;
    int k = 2;

    /* While loop. */
    while(j){
        puts('WHILE');
    }

    /* Do loop. */
    do{
        puts('DO');
    } while(k);

    /* For loop. */
    for(i=0;i<k;i++){
        puts('FOR');
    }

    /* Single child. */
    if(i < i){
        puts('IF-1');
    }

    /* Empty then. */
    if(j == k) {
    } else {
        puts('IF-2');
    }

    /* Empty else. */
    if(j == k) {
        puts('IF-3');
    } else {
    }

    /* Multiline loop. */
    while(j + k <= i){
        puts('MULTILINE');
        puts('WHILE-1');
        j++;
        k++;
        i++;
    }

}
