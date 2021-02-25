from _tree_sitter_cffi import ffi, lib

def start():
    """Start the underlying ECL runtime used by SEL."""
    lib.start()

def stop():
    """Stop the underlying ECL runtime used by SEL."""
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
    """Map FN over ECL_LIST returning the results in a Python List."""
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
        """Parse source-code string source of language and return the root of the resulting AST.
Language must be an sel language enum value."""
        if handle == None:
            self.handle = lib.convert(language, source.encode('ascii'))
        else:
            self.handle = handle

    # NOTE: Do we need to explicitly free this on the ECL side?
    # def __del__(self) -> None:

    def ast_at_point(self, line:int, column:int):
        """Return the most specific AST covering LINE and COLUMN."""
        return AST(lib.ast_at_point(self, line, column));

    def ast_language(self):
        """Return the AST's language.
The language is a member of the LANGUAGE enumeration."""
        return lib.ast_type(self.handle);

    def ast_type(self):
        """Return the AST's language.
The type is a member of the TYPE enumeration."""
        return lib.ast_type(self.handle);

    def source_text(self) -> str:
        """Return a string of the AST's source text."""
        return to_string(lib.source_text(self.handle))

    def children(self):
        """Return a list of the AST's children."""
        return ecl_mapcar(lambda child: AST(handle=child), lib.children(self.handle))

    def child_slots(self) -> [str]:
        """Return a list of the AST's child slots."""
        return ecl_mapcar(lambda slot: (to_string(lib.to_string(lib.car(slot))),
                                        lib.to_short(lib.cdr(slot))),
                          lib.child_slots(self.handle))

    def child_slot_arity(self, slot: str):
        """Return the arity of the AST's child slot."""
        pair = next((pair for pair in self.child_slots() if pair[0] == slot), None)
        if pair:
            return pair[1]
        else:
            return None

    def child_slot(self, slot: str):
        """Return the contents of the AST's child slot value."""
        arity = self.child_slot_arity(slot)
        if arity == None:
            return None
        else if arity == 0:
            return lib.slot(self.handle, slot.encode('ascii'))
        else:
            return ecl_mapcar(lambda child: AST(handle=child), lib.slot(self.handle, slot.encode('ascii')))

    def parent(self, root):
        """Return AST's parent under root."""
        return lib.parent(root.handle, self.handle);

    def function_asts(self):
        """Return any function ASTs under AST."""
        return ecl_mapcar(lambda fn: AST(handle=fn), lib.function_asts(self.handle))

    def call_asts(self):
        """Return any call ASTs under AST."""
        return ecl_mapcar(lambda fn: AST(handle=fn), lib.call_asts(self.handle))

    # AST slot accessors
    def ensure_type(self, desired_type:int):
        if not lib.ast_type(self.handle) == desired_type:
            raise TypeError("AST is not of required type")

    def function_name(self) -> str:
        """Return AST's name.  AST must be of type function."""
        self.ensure_type(sel.FUNCTION)
        return to_string(lib.function_name(self.handle))

    def function_parameters(self):
        """Return AST's parameters.  AST must be of type function."""
        self.ensure_type(sel.FUNCTION)
        return ecl_mapcar(lambda param: AST(handle=param), lib.function_parameters(self.handle))

    def function_body(self):
        """Return AST's body.  AST must be of type function."""
        self.ensure_type(sel.FUNCTION)
        return AST(handle=lib.function_body(self.handle))

    def call_arguments(self):
        """Return AST's arguments.  AST must be of type call."""
        self.ensure_type(sel.CALL)
        return AST(handle=lib.call_arguments(self.handle))

    def call_module(self):
        """Return AST's module.  AST must be of type call."""
        self.ensure_type(sel.CALL)
        return AST(handle=lib.call_module(self.handle))

    def call_function(self):
        """Return AST's function.  AST must be of type call."""
        self.ensure_type(sel.CALL)
        return AST(handle=lib.call_function(self.handle))
