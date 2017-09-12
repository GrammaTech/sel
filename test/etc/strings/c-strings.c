int main(int argc, char* argv[])
{
    char test[] = "test";
    char *x = argv[1];
    if (!strcmp(test, x))
        return 1;
    return 0;
}
