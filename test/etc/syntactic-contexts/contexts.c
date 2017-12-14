struct fields {
    int f1;
    int f2;
};

void full_stmt()
{
    int x = 0;
}

void braced_body()
{
    if (1) {
        int x = 1;
    }
}

void unbraced_body(int x)
{
    if (2)
        x = 2;
}

void list(int a, int b, int c)
{
}

void initialization_list()
{
    int a[] = { 1, 2, 3 };
}

#define MACRO(x) x
void trailing_semi_with_whitespace(int x)
{
    MACRO(x) ;
}
