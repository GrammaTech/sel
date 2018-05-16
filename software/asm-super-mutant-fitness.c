#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>
#include <unistd.h>
#include <sys/mman.h>
#include <errno.h>

typedef void (*vfunc)();

extern vfunc variant_table[]; // 0-terminated array of variant
                              // function pointers
#define NUM_INPUT_REGS 15
#define NUM_OUTPUT_REGS 9
#define RSP_INDEX 3  // index into output_regs where RSP is stored

#define PAGE_SIZE 4096
#define PAGE_MASK 0xfffffffffffff000

extern unsigned long input_regs[NUM_INPUT_REGS];
extern unsigned long output_regs[NUM_OUTPUT_REGS];
extern unsigned long input_mem[];
extern unsigned long output_mem[];
extern unsigned long num_tests;   // number of test cases

unsigned long save_rsp;  // save the stack pointer
unsigned long save_rbx;  // save the stack pointer

unsigned long result_regs[NUM_OUTPUT_REGS]; // for storing the
                                            // resulting values

static unsigned long test_offset = 0;

#define timer_start(name) \
    unsigned long name; \
    {\
        unsigned long lo, hi;\
        asm("rdtscp" : "=a" (lo), "=d" (hi));\
        name = lo | (hi << 32);\
    }

#define timer_elapsed(start_name, elapsed_name)          \
    unsigned long elapsed_name; \
    {\
        unsigned long lo, hi;\
        asm("rdtscp" : "=a" (lo), "=d" (hi));\
        elapsed_name = (lo | (hi << 32)) - start_name;\
    }

//
// First push all of the input registers to save them (except rsp, we
// have to store that in static data).
// Then, replace all registers with the input register data.
// We do this in a macro so it will always be inlined in the caller
// function.
//
#define init_regs()                  \
    asm(                             \
        "push %rax\n\t"              \
        "push %rbx\n\t"              \
        "push %rcx\n\t"              \
        "push %rdx\n\t"              \
        "push %rbp\n\t"              \
        "push %rsi\n\t"              \
        "push %rdi\n\t"              \
        "push %r8\n\t"               \
        "push %r9\n\t"               \
        "push %r10\n\t"              \
        "push %r12\n\t"              \
        "push %r13\n\t"              \
        "push %r14\n\t"              \
        "push %r15\n\t"              \
        "movq %rsp, save_rsp\n\t"    \
        "movq test_offset, %rax\n\t" \
        "leaq input_regs, %rbx\n\t"  \
        "add %rax, %rbx\n\t"        \
        "movq 0x00(%rbx), %rax\n\t" \
        "movq 0x10(%rbx), %rcx\n\t" \
        "movq 0x18(%rbx), %rdx\n\t" \
        "movq 0x20(%rbx), %rsp\n\t" \
        "movq 0x28(%rbx), %rbp\n\t" \
        "movq 0x30(%rbx), %rsi\n\t" \
        "movq 0x38(%rbx), %rdi\n\t" \
        "movq 0x40(%rbx), %r8\n\t"  \
        "movq 0x48(%rbx), %r9\n\t"  \
        "movq 0x50(%rbx), %r10\n\t" \
        "movq 0x58(%rbx), %r12\n\t" \
        "movq 0x60(%rbx), %r13\n\t" \
        "movq 0x68(%rbx), %r14\n\t" \
        "movq 0x70(%rbx), %r15\n\t" \
        "movq 0x08(%rbx), %rbx\n\t" \
        );

//
// Copy all the registers (which we care about) into the
// output_regs array.
// Then restore all the registers previous state
// (before we called init_regs) by restoring the previous
// stack pointer (stored statically in save_sp) and
// then popping all the other registers.
//
#define copy_result_regs()                 \
    asm(                                   \
       "movq %rbx, save_rbx\n\t"           \
       "lea result_regs, %rbx\n\t"         \
       "movq %rax, 0x00(%rbx)\n\t"         \
       "movq %rdx, 0x10(%rbx)\n\t"         \
       "movq %rsp, 0x18(%rbx)\n\t"         \
       "movq %rbp, 0x20(%rbx)\n\t"         \
       "movq %r12, 0x28(%rbx)\n\t"         \
       "movq %r13, 0x30(%rbx)\n\t"         \
       "movq %r14, 0x38(%rbx)\n\t"         \
       "movq %r15, 0x40(%rbx)\n\t"         \
       "movq %rbx, %rcx\n\t"               \
       "movq save_rbx, %rbx\n\t"           \
       "movq %rbx, 0x08(%rcx)\n\t"         \
                                           \
       "movq save_rsp, %rsp\n\t"           \
       "pop %r15\n\t"                      \
       "pop %r14\n\t"                      \
       "pop %r13\n\t"                      \
       "pop %r12\n\t"                      \
       "pop %r10\n\t"                      \
       "pop %r9\n\t"                       \
       "pop %r8\n\t"                       \
       "pop %rdi\n\t"                      \
       "pop %rsi\n\t"                      \
       "pop %rbp\n\t"                      \
       "pop %rdx\n\t"                      \
       "pop %rcx\n\t"                      \
       "pop %rbx\n\t"                      \
       "pop %rax\n\t"                      \
       );

//
// Traverse the mem inputs, three qwords at a time.
// Ensure all pages that are referenced are committed.
// When we get to a null address, we've reached the end of the inputs.
//
void init_pages() {
    unsigned long* p = input_mem;
    while (*p) {
        unsigned long* addr = (unsigned long*)*p;
        p += 3;
        // allocate pages the first time
        void* anon = mmap((unsigned long*)(((unsigned long)addr) & PAGE_MASK), PAGE_SIZE,
                          PROT_READ|PROT_WRITE,
                          MAP_ANONYMOUS|MAP_PRIVATE,
                          -1,
                          0);
    }
}

//
// Traverse the mem inputs, three qwords at a time.
// When we get to a null address, we've reached the end of the inputs.
//
void init_mem(int test) {
    unsigned long* p = input_mem;

    // skip to the indicated test, by advancing past (test - 1) 0
    // words (qwords containing 0)
    unsigned long count = test;
    while (count > 0) {
        do { p += 3; } while (*p != 0);
        p++;  // advance p to start of next test data
        count--;
    }
    // p should now be positioned at the correct test data
        
    while (*p) {
        unsigned long* addr = (unsigned long*)*p++;
        unsigned long data = *p++;
        unsigned long mask = *p++;
        *addr = (data & mask) | (~mask & *addr);
    }
}

const char* output_reg_names[NUM_OUTPUT_REGS] = {
    "rax", "rbx", "rdx", "rsp", "rbp", "r12", "r13", "r14", "r15" };

//
// Returns 1 if results are correct, 0 otherwise.
// Assumes the output register values have been copied
// into result_regs[] (via copy_result_regs macro).
//
int check_results(int test) {
    int success = 1;
    // check registers
    for (int i = 0; i < NUM_OUTPUT_REGS; i++) {
        if (output_regs[test * NUM_OUTPUT_REGS + i] != result_regs[i]) {
           printf("Test failed at register: %s, expected: %lx, "
                   "found: %lx, orig rsp: %lx\n",
                   output_reg_names[i], output_regs[i], result_regs[i], save_rsp);
            success = 0;
        }
    }

    // check memory
    unsigned long* p = output_mem;

    // skip to the indicated test, by advancing past (test - 1) 0
    // words (qwords containing 0)
    unsigned long count = test;
    while (count > 0) {
        do { p += 3; } while (*p != 0);
        p++;  // advance p to start of next test data
        count--;
    }
    // p should now be positioned at the correct test data
    
    while (*p) {
        unsigned long* addr = (unsigned long*)*p++;
        unsigned long data = *p++;
        unsigned long mask = *p++;

        unsigned long* rsp = (unsigned long*)output_regs[RSP_INDEX];
        if (addr < rsp && (rsp - addr) < 64)
            continue;    // ignore changes in the 1024 below the top
                         // of the stack (i.e. not on the stack, which
                         // grows down) 
        if ((*addr & mask) != (data & mask)) {
            printf("Test failed at addr: %lx, expected: %lx, "
                   "mask: %lx, found: %lx, orig rsp: %lx\n",
                   (unsigned long)addr, data, mask, *addr, save_rsp);
            success = 0;
        }
    }    
    return success;
}

//
// returns the elapsed time (in instruction clock counts), or 0
// if the outputs did not match expected outputs.
//
static vfunc execaddr = 0;


unsigned long run_variant(int v, int test) {
    test_offset = test * NUM_INPUT_REGS * sizeof(unsigned long);
    execaddr = variant_table[v];
    init_mem(test);
    timer_start(start);
    init_regs();
    execaddr();
    copy_result_regs();
    timer_elapsed(start, elapsed);
    int res = check_results(test);   // make sure all the registers had expected
                                 // output values
    printf("variant %d valid: %s elapsed: %ld\n", v,
           (res ? "yes" : "no"),
           elapsed);
    
    return res == 0 ? 0 : elapsed;
}

#define NUM_ITERATIONS 10

unsigned long run_variant_tests(int v) {
    unsigned long total = 0;
    unsigned long result;
    for (int j = 0; j < NUM_ITERATIONS; j++) {
        for (int k = 0; k < num_tests; k++) {
            result = run_variant(v, k);
            if (result == 0)
                return 0;
            // we only tally the first test case for performance
            if (j > 0 && k == 0)    // ignore first test result as outlier
                total += result;
        }
    }
    return total / (NUM_ITERATIONS - 1);
}

void segfault_sigaction(int signal, siginfo_t *si, void *arg) {
    printf("Caught segfault at address %p\n", si->si_addr);
    // try to make the memory page read/write
    unsigned long addr = (unsigned long)(si->si_addr);
    unsigned long page_start = addr & PAGE_MASK;
    int ret = mprotect((unsigned long*)addr, PAGE_SIZE, PROT_READ|PROT_WRITE);
    if (ret < 0) {
        switch (errno) {
        case EACCES:  printf("Error: EACCESS\n"); break;
        case EINVAL:   printf("Error: EINVAL\n"); break;
        case ENOMEM:   printf("Error: ENOMEM\n"); break;
        default:       printf("Error: unknown code %d\n", errno);
        }
        exit(errno);
    }
    // otherwise continue
}

//
// Our optimal variant is the one with the lowest non-0 result
// (0 signifies not passing output check).
//
int main(int argc, char* argv[]) {
    // set up signal handling
    struct sigaction sa;
    memset(&sa, 0, sizeof(struct sigaction));
    sigemptyset(&sa.sa_mask);
    sa.sa_sigaction = segfault_sigaction;
    sa.sa_flags = SA_SIGINFO;

    sigaction(SIGSEGV, &sa, NULL);

    // show the page size
    long sz = sysconf(_SC_PAGESIZE);
    printf("Page size: %ld\n", sz);

    init_pages(); // allocate any referenced pages
    
    unsigned long best = 0;
    int best_index = -1;
    for (int i = 0; variant_table[i]; i++) {
        unsigned long result = run_variant_tests(i);
        if (result > 0 && (best == 0 || result < best)) {
            best = result;
            best_index = i;
        }
    }
    if (best > 0)
        printf("Best fitness: variant %d, elapsed: %ld\n", best_index, best);
    else
    {
        printf("No variant passed the tests.\n");
        return 1;
    }
    return 0;
}
