//
// File: asm-super-mutant-fitness.c
// Contents: C harness to run and test various asm-super-mutant
// software objects, to determine which variant has the best fitness
// for each test.
//
#define __x86_64__ 1
#define __USE_GNU 1

#include <signal.h>
#include <time.h>
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>
#include <unistd.h>
#include <sys/mman.h>
#include <sys/time.h>
#include <sys/ucontext.h>
#include <sys/syscall.h>
#include <errno.h>
#include <setjmp.h>
#include <papi.h>

// variant function pointer
// This is defined to take no arguments and return nothing.
// In reality the functions it calls may expect arguments and may
// return them, but those are taken care of in the register and memory
// initialization and checking code. So for our purposes, we just call
// the pointer as if it were a standalone block.
//
typedef void (*vfunc)();

extern vfunc variant_table[]; // 0-terminated array of variant
                              // function pointers, defined in the asm
                              // file
#define NUM_REGS 16           // total number of registers (if all are used)
#define DEBUG 1               // set this to 1, to turn on debugging messages

#define PAGE_SIZE 4096
#define PAGE_MASK 0xfffffffffffff000

extern unsigned long input_regs[];
extern unsigned long output_regs[];
extern unsigned long input_mem[];
extern unsigned long output_mem[];
extern unsigned long num_tests;   // number of test cases

extern unsigned long save_rsp;  // save the stack pointer
extern unsigned long save_rbx;  // save the stack pointer

extern unsigned long save_return_address;
extern unsigned long result_return_address;

extern unsigned long result_regs[NUM_REGS]; // for storing the
                                            // resulting values
extern unsigned long test_offset;

//
// Live input register mask contains uses bits to represent
// each live register.
//
unsigned long rax_bit = 0x00000001;
unsigned long rbx_bit = 0x00000002;
unsigned long rcx_bit = 0x00000004;
unsigned long rdx_bit = 0x00000008;
unsigned long rsp_bit = 0x00000010;
unsigned long rbp_bit = 0x00000020;
unsigned long rsi_bit = 0x00000040;
unsigned long rdi_bit = 0x00000080;
unsigned long r08_bit = 0x00000100;
unsigned long r09_bit = 0x00000200;
unsigned long r10_bit = 0x00000400;
unsigned long r11_bit = 0x00000800;
unsigned long r12_bit = 0x00001000;
unsigned long r13_bit = 0x00002000;
unsigned long r14_bit = 0x00004000;
unsigned long r15_bit = 0x00008000;

extern unsigned long live_input_registers; // input live register mask
extern unsigned long live_output_registers;// output live register mask
extern unsigned long num_input_registers;  // number of live input registers
extern unsigned long num_output_registers; // number of live output registers

unsigned long overhead = 0;    // number of instructions in
                               // setup/restore code

static int EventSet = PAPI_NULL;

extern void _init_registers();

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
// Determine which register position contains RSP, given the mask.
// To figure it out, we count one bits in the registers
// mask until we get to RSP bit (which should always be set).
//
int RSP_position(unsigned long mask) {
    return
        ((mask & rax_bit) ? 1 : 0) +
        ((mask & rbx_bit) ? 1 : 0) +
        ((mask & rcx_bit) ? 1 : 0) +
        ((mask & rdx_bit) ? 1 : 0);
}

const char* reg_names[NUM_REGS] = {
    "rax", "rbx", "rcx", "rdx",
    "rsp", "rbp", "rsi", "rdi",
    "r8",  "r9",  "r10", "r11",
    "r12", "r13", "r14", "r15"
};

//
// Return the name of the nth register.
// Finds the nth one-bit, starting from low bit of reg_mask.
// n is zero-based i.e. 0 = 1st reg, 1 = 2nd reg etc.
// Returns the name of the corresponding register.
//
const char* reg_name(unsigned long reg_mask, int n) {
#if DEBUG
    int save_n = n;
#endif
    unsigned long mask = 1;
    int i = 0;
    for (; i < NUM_REGS; i++) {
        if (reg_mask & mask)
            --n;
        if (n < 0) break;
        mask <<= 1;
    }
#if DEBUG
    if (i < NUM_REGS)
        fprintf(stderr, "reg_name:  reg_mask=0x%lx, n=%i, name=%s\n",
            reg_mask, save_n, reg_names[i]);
#endif
    return (i < NUM_REGS) ? reg_names[i] : "";
}

//
// Map a single page as read/write, given an address that falls on
// that page
void map_page(unsigned long* addr) {
    unsigned long* page_addr = (unsigned long*)(((unsigned long)addr) & PAGE_MASK);
    void* anon = mmap(page_addr, PAGE_SIZE,
                          PROT_READ|PROT_WRITE,
                              MAP_ANONYMOUS|MAP_PRIVATE|MAP_FIXED /*|MAP_UNINITIALIZED*/,
                          -1,
                          0);
#if DEBUG
    fprintf(stderr, "Allocated page at address 0x%lx, result: %lx\n",
           (unsigned long)page_addr,
           (unsigned long)anon);
#endif
}

//
// Traverse the mem inputs, three qwords at a time.
// Ensure all pages that are referenced are committed.
// When we get to a null address, we've reached the end of the inputs.
//
void map_pages(unsigned long* p) {
    for (int k = 0; k < num_tests; k++) {
        while (*p) {
            unsigned long* addr = (unsigned long*)*p;
            map_page(addr);
            p += 3;
        }
        p++;
    }
}

void init_pages() {
    // map the initial stack page
    map_page((unsigned long*)(input_regs[RSP_position(live_input_registers)]));
    map_pages(input_mem); // map pages indicated by the memory i/o
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

//
// Returns 1 if results are correct, 0 otherwise.
// Assumes the output register values have been copied
// into result_regs[] (via copy_result_regs macro).
//
int check_results(int variant, int test) {
    int success = 1;

    // check that the return address did not get messed up by stack
    // corruption
    if (result_return_address == 1) {
        fprintf(stderr, "Variant %d, test %d timed out and was terminated.\n",
                variant, test);
    }

    if (save_return_address != result_return_address) {
#if DEBUG
        fprintf(stderr, "Expected return address: %lx, found: %lx\n",
                save_return_address,
                result_return_address);
#endif
        success = 0;
    }

    int rsp_pos = RSP_position(live_output_registers);

    // check registers
    for (int i = 0; i < num_output_registers; i++) {
        if (i != rsp_pos
            && output_regs[test * num_output_registers + i] != result_regs[i]) {
#if DEBUG
            fprintf(stderr, "Test %d failed at register: %s, expected: %lx, "
                   "found: %lx, orig rsp: %lx, rsp_pos: %i\n",
                  test,
                  reg_name(live_output_registers, i),
                  output_regs[test * num_output_registers + i],
                  result_regs[i],
                  save_rsp,
                  rsp_pos);
#endif
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

        unsigned long* rsp = (unsigned long*)output_regs[test * num_output_registers
                                                         + rsp_pos];
        if (addr < rsp && (rsp - addr) < 64)
            continue;    // ignore changes in the 1024 below the top
                         // of the stack (i.e. not on the stack, which
                         // grows down)
        if ((*addr & mask) != (data & mask)) {
            fprintf(stderr, "Test %d failed at addr: %lx, expected: %lx, "
                   "mask: %lx, found: %lx, orig rsp: %lx\n",
                   test,
                   (unsigned long)addr,
                   data, mask, *addr, save_rsp);
            success = 0;
        }
    }
    return success;
}

// Unblock a signal.  Unless we do this, the signal may only be sent once.
static void unblock_signal(int signum)
{
    sigset_t sigs;
    sigemptyset(&sigs);
    sigaddset(&sigs, signum);
    sigprocmask(SIG_UNBLOCK, &sigs, NULL);
}

static void sigunblock() {
    // Unblock the signal
    sigset_t sa_mask;
    sigemptyset(&sa_mask);
    sigaddset(&sa_mask, SIGSEGV);
    sigaddset(&sa_mask, SIGBUS);
    sigprocmask(SIG_UNBLOCK, &sa_mask, NULL);
}

#define REG_RIP 16

void segfault_sigaction(int signal, siginfo_t *si, void *context) {
    //fprintf(stderr, "Caught segfault at address %p\n", si->si_addr);

    unblock_signal(signal);

    ucontext_t* p = (ucontext_t*)context;
    p->uc_mcontext.gregs[REG_RIP] = (greg_t)save_return_address;

    asm("        pushq $1\n\t");
    asm("        popq result_return_address\n\t");
    asm("        movq save_return_address, %rbx\n\t");
    asm("        jmp *%rbx\n\t");
}

void timer_sigaction(int signal, siginfo_t *si, void *arg) {
    //fprintf(stderr, "Execution timer expired\n");
    unblock_signal(signal);
    asm("        pushq $1\n\t");
    asm("        popq result_return_address\n\t");
    asm("        movq save_return_address, %rbx\n\t");
    asm("        jmp *%rbx\n\t");
}

static timer_t timerid;
static struct sigevent sev;
static struct itimerspec timer;
static struct sigaction tsa;
static sigset_t mask;

void start_timer() {
    timer.it_value.tv_nsec = 1000000;  // .001 second timeout
    timer.it_value.tv_sec = 0;
    timer.it_interval.tv_sec = 0;
    timer.it_interval.tv_nsec = 0;
    if (timer_settime(timerid, 0, &timer, NULL) == -1) {
        fprintf(stderr, "timer_settime() error\n");
        exit(1);
    }
}

void end_timer() {
    timer.it_value.tv_nsec = 0;
    timer.it_value.tv_sec = 0;
    timer.it_interval.tv_sec = 0;
    timer.it_interval.tv_nsec = 0;
    if (timer_settime(timerid, 0, &timer, NULL) == -1) {
        fprintf(stderr, "timer_settime() error\n");
        exit(1);
    }
}

#define SIG SIGVTALRM
//#define SIG SIGRTMIN
//#define CLOCKID CLOCK_REALTIME
#define CLOCKID CLOCK_THREAD_CPUTIME_ID

timer_t setup_timer() {

    // Establish handler for timer signal
    memset(&tsa, 0, sizeof(tsa));
    memset(&timer, 0, sizeof(timer));
    memset(&sev, 0, sizeof(sev));

    tsa.sa_sigaction = timer_sigaction;
    tsa.sa_flags = SA_SIGINFO | SA_ONSTACK | SA_NODEFER;
    sigemptyset(&tsa.sa_mask);
    if (sigaction(SIG, &tsa, 0) == -1) {
        fprintf(stderr, "sigaction() error\n");
        exit(1);
    }

    // Create the timer
    sev.sigev_notify = SIGEV_SIGNAL;
    sev.sigev_signo = SIG;
    sev.sigev_value.sival_ptr = &timerid;
    if (timer_create(CLOCKID, &sev, &timerid) == -1) {
        fprintf(stderr, "timer_create() error\n");
        exit(1);
    }

    return timerid;
}

void destroy_timer(timer_t timerid) {
    if (timer_delete(timerid) == -1) {  // delete the timer
        fprintf(stderr, "timer_delete() error: %d\n", errno);
        exit(1);
    }
}

//
// returns the elapsed time (in instruction clock counts), or ULONG_MAX
// if the outputs did not match expected outputs.
//
static vfunc execaddr = 0;

unsigned long run_variant(int v, int test) {
    long_long start_value[1];
    long_long end_value[1];
    long_long stop_value[1];
    int retval;

    test_offset = test * num_input_registers * sizeof(unsigned long);
    execaddr = variant_table[v];
    init_mem(test);

    start_timer();  // start POSIX timer
    sigunblock();

    retval = PAPI_start(EventSet);  // start PAPI counting
    if (retval < 0) {
        fprintf(stderr, "PAPI_start() error: %d\n", retval);
        exit(1);
    }

    retval = PAPI_read(EventSet, start_value);     // get PAPI count
    if (retval < 0) {
        fprintf(stderr, "PAPI_read() error: %d\n", retval);
        exit(1);
    }

    asm("call _init_registers\n\t");        
    execaddr();
    asm("call _restore_registers\n\t");

    retval = PAPI_read(EventSet, end_value);     // get PAPI count
    end_timer();  // stop POSIX timer
    sigunblock();

    if (retval < 0) {
        fprintf(stderr, "PAPI_read() error: %d\n", retval);
        exit(1);
    }

    retval = PAPI_stop(EventSet, stop_value);
    if (retval < 0) {
        fprintf(stderr, "PAPI_stop() error: %d\n", retval);
        exit(1);
    }

    long_long elapsed_instructions = end_value[0] - start_value[0];

    int res = check_results(v, test);   // make sure all the registers had expected
                                 // output values
#if DEBUG
    fprintf(stderr, "variant %d valid: %s instructions: %lld\n", v,
           (res ? "yes" : "no"),
           elapsed_instructions - overhead);
#endif
    if (res && elapsed_instructions == 0) {
        fprintf(stderr, "Error: elapsed_instructions (0) is not valid!");
        elapsed_instructions = ULONG_MAX;
    }
    return res == 0 ? ULONG_MAX : elapsed_instructions - overhead;
}

//
// Determine the number of instructions of overhead,
// initializing registers and then restoring them.
// The result is stored in the 'overhead' variable.
// This is then subtracted from each result to obtain
// a more accurate measurement.
//
unsigned long run_overhead() {
    long_long start_value[1];
    long_long end_value[1];
    long_long stop_value[1];
    int retval;

    retval = PAPI_start(EventSet);  // start PAPI counting
    if (retval < 0) {
        fprintf(stderr, "PAPI_start() error: %d\n", retval);
        exit(1);
    }

    retval = PAPI_read(EventSet, start_value);     // get PAPI count
    if (retval < 0) {
        fprintf(stderr, "PAPI_read() error: %d\n", retval);
        exit(1);
    }

    asm("call _init_registers\n\t");
    asm("call _restore_registers\n\t");
    retval = PAPI_read(EventSet, end_value);     // get PAPI count

    if (retval < 0) {
        fprintf(stderr, "PAPI_read() error: %d\n", retval);
        exit(1);
    }

    retval = PAPI_stop(EventSet, stop_value);
    if (retval < 0) {
        fprintf(stderr, "PAPI_stop() error: %d\n", retval);
        exit(1);
    }

    overhead = (unsigned long)(end_value[0] - start_value[0]);
#if DEBUG
    fprintf(stderr, "Number of overhead instructions: %lu\n", overhead);
#endif
    return (unsigned long) overhead;
}

void run_variant_tests(int v, unsigned long test_results[]) {
#if DEBUG
    fprintf(stderr, "Testing variant %d: ", v);
#endif
    for (int k = 0; k < num_tests; k++) {
        test_results[k] = run_variant(v, k);
        if (test_results[k] == ULONG_MAX)
            return;             // quit on the first failure
    }
}

void papi_init() {
    int retval = PAPI_library_init(PAPI_VER_CURRENT);

    if (retval != PAPI_VER_CURRENT && retval > 0) {
        fprintf(stderr,"PAPI library version mismatch!\n");
        exit(1);
    }
    if (retval < 0) {
        fprintf(stderr, "PAPI initialization error: %d\n", retval);
        exit(1);
    }
#if DEBUG
    fprintf(stderr, "PAPI Version Number %d.%d.%d\n",
            PAPI_VERSION_MAJOR(retval),
            PAPI_VERSION_MINOR(retval),
            PAPI_VERSION_REVISION(retval));
    fflush(stderr);
#endif
    // Create an event set, containint Total Instructions Executed counter
    retval = PAPI_create_eventset(&EventSet);
    if (retval < 0) {
        fprintf(stderr, "PAPI_create_eventset()  error: %d\n", retval);
        exit(1);
    }
    retval = PAPI_add_event(EventSet, PAPI_TOT_INS);
        if (retval < 0) {
        fprintf(stderr, "PAPI_add_event()  error: %d\n", retval);
        exit(1);
    }
}

unsigned char sig_stack_bytes[SIGSTKSZ + 8];
stack_t signal_stack = { sig_stack_bytes + 8, 0, SIGSTKSZ };

#if 0
#define RESTORE(name, syscall) RESTORE2 (name, syscall)
#define RESTORE2(name, syscall)			\
asm						\
  (						\
   ".text\n"					\
   ".byte 0  # Yes, this really is necessary\n" \
   ".align 16\n"				\
   "__" #name ":\n"				\
   "	movq $" #syscall ", %rax\n"		\
   "	syscall\n"				\
   );

/* The return code for realtime-signals.  */
RESTORE (restore_rt, __NR_rt_sigreturn)
void restore_rt (void) asm ("__restore_rt")
  __attribute__ ((visibility ("hidden")));

struct kernel_sigaction
{
    void (*k_sa_sigaction)(int,siginfo_t *,void *);
    unsigned long k_sa_flags;
    void (*k_sa_restorer) (void);
    sigset_t k_sa_mask;
};

void setup_kernel_handler(int sig) {
    // we need to use a kernel handler
    struct kernel_sigaction act;
    act.k_sa_sigaction = segfault_sigaction;
    sigemptyset(&act.k_sa_mask);
    act.k_sa_flags = SA_SIGINFO|SA_NODEFER|SA_ONSTACK;
    act.k_sa_restorer = restore_rt;
    syscall(SYS_rt_sigaction, sig, &act, NULL, _NSIG / 8);
}
#endif

//
//
int main(int argc, char* argv[]) {
    // set up signal handling
    if (sigaltstack(&signal_stack, NULL) == -1) {
        fprintf(stderr, "Error installing alternate signal stack\n");
        exit(1);
    }
    sigunblock();

    struct sigaction sa;
    memset(&sa, 0, sizeof(struct sigaction));
    sigemptyset(&sa.sa_mask);
    sa.sa_sigaction = segfault_sigaction;
    sa.sa_flags = SA_SIGINFO|SA_NODEFER|SA_RESTART|SA_ONSTACK;

    if (sigaction(SIGSEGV, &sa, NULL) == -1) {
        fprintf(stderr, "Error installing segfault signal handler\n");
        exit(1);
    }
    if (sigaction(SIGBUS, &sa, NULL) == -1) {
        fprintf(stderr, "Error installing segfault signal handler\n");
        exit(1);
    }

    setup_timer();

    // show the page size
    long sz = sysconf(_SC_PAGESIZE);
#if DEBUG
    fprintf(stderr, "Page size: %ld\n", sz);
#endif
    papi_init();

    init_pages(); // allocate any referenced pages

    unsigned long best = 0;
    int best_index = -1;
    int num_variants = 0;

    // count the number of variants by scanning the table, looking for
    // terminating 0
    for (int i = 0; variant_table[i]; i++)
        num_variants++;
#if DEBUG
    fprintf(stderr, "Number of variants: %d\n", num_variants);
#endif
    unsigned long* test_results =
        (unsigned long*)malloc(sizeof(unsigned long) * num_tests * num_variants);
    unsigned long* p = test_results;

    for (int i = 0; i < (num_tests * num_variants); i++)
        test_results[i] = ULONG_MAX;   // set fitness to max to
                                       // initialize

    // see how many overhead instructions there are
    unsigned long overhead = run_overhead();
    
    for (int i = 0; variant_table[i]; i++) {
        run_variant_tests(i, test_results + (i * num_tests));
    }

    // output the results, with all the tests results for one variant
    // on each line.
    // Note: this is the only thing sent to stdout by this program.
    //
    for (int i = 0; i < num_variants; i++) {
        for (int j = 0; j < num_tests; j++) {
            if (test_results[i * num_tests + j] == 0) {
                fprintf(stderr, "Error: test_result (0) is not valid!");
                test_results[i * num_tests + j] = ULONG_MAX;
            }
            fprintf(stdout, "%lu ", test_results[i * num_tests + j]);
        }
        fprintf(stdout, "\n");
    }
    // free(test_results);
    return 0;
}
