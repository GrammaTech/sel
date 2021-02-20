from cffi import FFI
ffibuilder = FFI()

# cdef() expects a single string declaring the C types, functions and
# globals needed to use the shared object. It must be in valid C syntax.
ffibuilder.cdef("""
    extern void init(void* cl_object);
""")

# set_source() gives the name of the python extension module to
# produce, and some C source code as a string.  This C code needs
# to make the declarated functions, types and globals available,
# so it is often just the "#include".
ffibuilder.set_source("_tree_sitter_cffi",
"""
     #include "tree-sitter.h"   // the C header of the library
""",
                      libraries=['ecl'],
                      extra_objects['tree-sitter--all-systems.dylib'])

if __name__ == "__main__":
    ffibuilder.compile(verbose=True)
