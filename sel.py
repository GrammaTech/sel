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
    while element := lib.car(ecl_list):
        if lib.null(element):
            break;
        results.append(fn(element))
        ecl_list = lib.cdr(ecl_list)
    return result.reverse()

class AST:
    def __init__(self, language:int = unknown_language, source:str="", handle=None) -> None:
        if handle == None:
            self.handle = lib.convert(language, source.encode('ascii'))

    # NOTE: Do we need to explicitly free this on the ECL side?
    # def __del__(self) -> None:

    def ast_at_point(self, line:int, column:int):
        return AST.new(lib.ast_at_point(self, line, column));

    def type(self):
        return ffi.string(lib.type(self.handle))

    def children(self):
        ecl_mapcar(lambda child: AST.new(handle=child), lib.children(self.handle))

    def child_slots(self) -> [str]:
        ecl_mapcar(lambda slot: lib.to_string(slot), lib.child_slots(self.handle))
