#include <stdio.h>
#include <assert.h>

int main(int argc, char **argv){
    assert(argc >= 0);
  
    int t=100;

    while(1){
	if(!(t <= argc)) break;
    }

    return 0;
}
