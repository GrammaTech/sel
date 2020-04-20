#include <stdio.h>
#include <stdlib.h>
#include <dlfcn.h>

int run_shared_function() {
    void *dlsym_return_value;
    int (*shared_function)();
    void *shared_handle;

    /* Make sure the symbolic link is being used here. */
    shared_handle = dlopen("./shared.so.link", RTLD_LAZY);

    if (!shared_handle) {
        fprintf(stderr, "Failed to Open shared.so.link\n");
        return -1;
    }

    dlsym_return_value  = dlsym(shared_handle, "shared_function");

    if (!dlsym_return_value) {
        fprintf(stderr, "shared_function not found in shared.so.link\n");
        return -1;
    }

    shared_function = (int (*)()) dlsym_return_value;

    return shared_function();
}

int run_nested_binary() {
    int system_return_value;
    system_return_value = system("./nested-dir/helper dummy-value");

    if (system_return_value) {
        fprintf(stderr, "Failed to run ./nested-dir/helper\n");
    }

    return system_return_value;
}

int main() {
    return run_shared_function() || run_nested_binary();
}
