typedef struct F F2;

struct F {
  int x;
};

typedef struct F F3;

int p(F2* q) {
  return q->x;
}


  
