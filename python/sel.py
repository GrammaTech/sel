from _tree_sitter_cffi import ffi, lib

def start():
    lib.start()

def stop():
    lib.stop()

# For now we'll just startup lisp on import
#
# start = False
# def check_start():
#     if not started:
#         print("Backing lisp engine not started, initializing with `sel.start()`.")
start()

unknown_language = lib.UNKNOWN_LANGUAGE
python = lib.PYTHON
javascript = lib.JAVASCRIPT
c = lib.C
cpp = lib.CPP

def ecl_mapcar(fn, ecl_list):
    results = []
    while True:
        element = lib.car(ecl_list)
        if lib.null(element):
            break
        results.append(fn(element))
        ecl_list = lib.cdr(ecl_list)
    results.reverse()
    return results

def to_string(cdata) -> str:
    return ffi.string(cdata, lib.get_last_string_length())

class AST:
    def __init__(self, language:int = unknown_language, source:str="", handle=None) -> None:
        if handle == None:
            self.handle = lib.convert(language, source.encode('ascii'))
        else:
            self.handle = handle

    # NOTE: Do we need to explicitly free this on the ECL side?
    # def __del__(self) -> None:

    def ast_at_point(self, line:int, column:int):
        return AST(lib.ast_at_point(self, line, column));

    def get_type(self):
        return to_string(lib.get_type(self.handle))

    def get_class(self):
        return to_string(lib.get_class(self.handle))

    def children(self):
        return ecl_mapcar(lambda child: AST(handle=child), lib.children(self.handle))

    def child_slots(self) -> [str]:
        return ecl_mapcar(lambda slot: (to_string(lib.to_string(lib.car(slot))),
                                        lib.to_short(lib.cdr(slot))),
                          lib.child_slots(self.handle))

    def source_text(self) -> str:
        return to_string(lib.source_text(self.handle))

    # AST slot accessors
    def function_name(self) -> str:
        return to_string(lib.function_name(self.handle))

    def function_parameters(self):
        return ecl_mapcar(lambda param: AST(handle=param), lib.function_parameters(self.handle))

    def function_body(self):
        return AST(handle=lib.function_body(self.handle))
