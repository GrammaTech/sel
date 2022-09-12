import atexit
import collections
import enum
import json
import multiprocessing
import psutil
import pkg_resources
import shutil
import socket
import subprocess
import time

import pygments.lexers

from pathlib import Path
from typing import (
    Any,
    Callable,
    ClassVar,
    ByteString,
    Dict,
    Generator,
    List,
    Optional,
    Tuple,
    Union,
)
from backports.cached_property import cached_property
from typing_extensions import Final
from .utility import add_method, generate_types_file

LiteralOrAST = Union[int, float, str, "AST"]


# Auxillary classes
class ASTLanguage(enum.Enum):
    C = 0
    Cpp = 1
    Java = 2
    Javascript = 3
    Python = 4
    Rust = 5
    TypescriptTs = 6
    TypescriptTsx = 7


class ASTException(Exception):
    """specialization for exceptions in the AST tree-sitter interface"""

    pass


def _guess_language(text: str) -> Optional[ASTLanguage]:
    """Use pygments to guess the source language of text, if possible."""
    lexer = pygments.lexers.guess_lexer(text)
    if isinstance(lexer, pygments.lexers.CLexer):
        return ASTLanguage.C
    elif isinstance(lexer, pygments.lexers.CppLexer):
        return ASTLanguage.Cpp
    elif isinstance(lexer, pygments.lexers.JavaLexer):
        return ASTLanguage.Java
    elif isinstance(lexer, pygments.lexers.JavascriptLexer):
        return ASTLanguage.Javascript
    elif isinstance(lexer, pygments.lexers.PythonLexer):
        return ASTLanguage.Python
    elif isinstance(lexer, pygments.lexers.RustLexer):
        return ASTLanguage.Rust
    elif isinstance(lexer, pygments.lexers.TypeScriptLexer):
        return ASTLanguage.TypescriptTs
    else:
        raise ASTException(
            f"Supported source language could not be derived from:\n{text}"
        )


# Base AST class
class AST:
    def __init__(self, oid: int) -> None:
        """
        Internal constructor creating an AST with the given object id (oid)
        pointing to an object on the Common Lisp side of the interface.

        Clients should not invoke this method and instead use the static
        factory methods below for AST creation.
        """
        assert isinstance(oid, int), "AST object id (oid) must be an integer."
        assert oid >= 0, "AST object id (oid) must be a non-negative integer."

        self._oid = oid

    # AST contruction from source code text
    @staticmethod
    def from_string(
        text: str,
        language: Optional[ASTLanguage] = None,
        *,
        deepest: Optional[bool] = False,
        error_tree: Optional[bool] = True,
    ) -> "AST":
        """
        Parse source-code string source of language and return the root
        of the resulting AST.

        When passing the deepest keyword argument as true, the deepest
        subnode in the tree still encompassing all of the given source
        text will be returned.

        When passing the error_tree keyword argument as true (the default),
        error and text fragment nodes will use the best-effort tree
        representation returned from tree-sitter to store their children
        instead of a flat, text representation.
        """
        language = _guess_language(text) if not language else language
        return _interface.dispatch(
            AST.from_string.__name__,
            text,
            language,
            deepest,
            error_tree,
        )

    # AST construction using templates
    @staticmethod
    def ast_template(
        template: str,
        language: ASTLanguage,
        *args: Tuple[LiteralOrAST],
        **kwargs: Dict[str, LiteralOrAST],
    ) -> "AST":
        """
        Build a single AST using an AST template syntax.

        For instance, `AST.ast_template("$ID = 1", ASTLanguage.Python, id="x")`
        returns `AST.from_string("x = 1", ASTLanguage.Python, deepest=True)`.

        See https://grammatech.github.io/sel/Templates.html or the python
        README for more information.
        """
        return _interface.dispatch(
            AST.ast_template.__name__,
            template,
            language,
            *args,
            **kwargs,
        )

    @staticmethod
    def asts_from_template(
        template: str,
        language: ASTLanguage,
        *args: Tuple[LiteralOrAST],
    ) -> List["AST"]:
        """
        Build and destructure component ASTs using template syntax.

        For instance, `AST.asts_from_template("$1 = $2", ASTLanguage.Python, "x", 1)`
        returns the component ASTs,
        `AST.from_string("x", ASTLanguage.Python, deepest=True)` and
        `AST.from_string("1", ASTLanguage.Python, deepest=True)`.

        See https://grammatech.github.io/sel/Templates.html or the python
        README for more information.
        """
        return _interface.dispatch(
            AST.asts_from_template.__name__,
            template,
            language,
            *args,
        )

    # AST construction by creating a copy
    @staticmethod
    def copy(
        ast: "AST",
        **kwargs: Dict[str, Union[LiteralOrAST, List[LiteralOrAST]]],
    ) -> "AST":
        """
        Create a copy of AST, optionally passing keyword arguments mapping
        child slots to new ASTs.

        Consider `a = AST.copy("x + 1", ASTLanguage.Python, deepest=True)`.
        To create a copy of this AST, you would use `AST.copy(a)`.
        To create a copy with the left-hand side replaced, you would use
        `AST.copy(
            "x + 1",
            python_left=AST.from_string("y", ASTLanguage.Python, deepest=True)
        )`.

        See the python README for more information.
        """
        language = ast.language
        for key, value in kwargs.items():
            if isinstance(value, list):
                kwargs[key] = [AST._ensure_ast(a, language=language) for a in value]
            else:
                kwargs[key] = AST._ensure_ast(value, language=language)

        return _interface.dispatch(AST.copy.__name__, ast, **kwargs)

    # Python method overrides
    def __repr__(self) -> str:
        """Return a string representation of the AST."""
        type_ = type(self)
        module = type_.__module__
        qualname = type_.__qualname__
        return f"<{module}.{qualname} {hex(self.oid)}>"

    def __del__(self) -> None:
        _interface.dispatch(AST.__del__.__name__, self.oid)
        self._oid = None

    def __copy__(self) -> "AST":
        """Return a shallow copy of AST conforming to copy.copy."""
        return AST(oid=_interface.dispatch(AST.__copy__.__name__, self))

    def __deepcopy__(self, memo) -> "AST":
        """Return a deep copy of AST conforming to copy.deepcopy."""
        return AST.copy(self)

    def __iter__(self) -> Generator["AST", None, None]:
        """Traverse self in pre-order, yielding subtrees"""
        yield from self.traverse()

    def __hash__(self) -> int:
        """Return the hashcode for the AST."""
        return self.oid

    def __eq__(self, other: Any) -> bool:
        """Return true if AST has the same oid as other."""
        if isinstance(other, AST):
            return self.oid == other.oid
        else:
            return False

    # AST properties for immutable attributes
    @property
    def oid(self) -> int:
        """Return the oid for this AST."""
        return self._oid

    @cached_property
    def language(self) -> ASTLanguage:
        """Return the AST's language."""
        return ASTLanguage[_interface.dispatch(AST.language.func.__name__, self)]

    @cached_property
    def source_text(self) -> str:
        """Return a string of the AST's source text."""
        return _interface.dispatch(AST.source_text.func.__name__, self)

    @cached_property
    def children(self) -> List["AST"]:
        """Return a list of the AST's children."""
        return _interface.dispatch(AST.children.func.__name__, self) or []

    def child_slots(self, internal: bool = False) -> List[Tuple[str, int]]:
        """
        Return a list of the AST's child slots, optionally including internal slots
        such as before/after ASTs.
        """
        return _interface.dispatch(AST.child_slots.__name__, self, internal) or []

    # AST methods for common, simple operations
    def refcount(self) -> int:
        """Return the AST's reference count."""
        return _interface.dispatch(AST.refcount.__name__, self)

    def ast_at_point(self, line: int, column: int) -> "AST":
        """Return the most specific AST covering LINE and COLUMN."""
        return _interface.dispatch(AST.ast_at_point.__name__, self, line, column)

    def ast_source_ranges(
        self,
    ) -> List[Tuple["AST", Tuple[Tuple[int, int], Tuple[int, int]]]]:
        """Return the source ranges (line, col) for AST its recursive children"""
        return _interface.dispatch(AST.ast_source_ranges.__name__, self)

    def ast_path(self, child: "AST") -> List:
        """Return the path to CHILD in SELF."""
        return _interface.dispatch(AST.ast_path.__name__, self, child) or []

    def lookup(self, path: List) -> Optional["AST"]:
        """Return the AST at PATH in SELF, if possible."""
        return _interface.dispatch(AST.lookup.__name__, self, path)

    def child_slot_arity(self, slot: str) -> Optional[int]:
        """Return the arity of the AST's child slot."""
        pairs = [
            pair for pair in self.child_slots(True) if pair[0].lower() == slot.lower()
        ]
        if pairs:
            return pairs[0][1]
        else:
            return None

    def child_slot(self, slot: str) -> Optional[Union["AST", List["AST"]]]:
        """Return the contents of the AST's child slot value."""
        arity = self.child_slot_arity(slot)
        if arity is None:
            return None
        else:
            return _interface.dispatch(AST.child_slot.__name__, self, slot)

    def parent(self, root: "AST") -> "AST":
        """Return AST's parent under ROOT."""
        return _interface.dispatch(AST.parent.__name__, root, self)

    def parents(self, root: "AST") -> List["AST"]:
        """Return AST's parents to the ROOT."""
        p = self.parent(root)
        return [p] + p.parents(root) if p else []

    def imports(self, root: "AST") -> List[List[str]]:
        """Return a list of imports available at AST."""
        return _interface.dispatch(AST.imports.__name__, root, self) or []

    def provided_by(self, root: "AST") -> Optional[str]:
        """Return library providing AST's identifier."""
        return _interface.dispatch(AST.provided_by.__name__, root, self)

    def function_asts(self) -> List["AST"]:
        """Return any function ASTs under AST."""
        return [c for c in self if isinstance(c, FunctionAST)]

    def call_asts(self) -> List["AST"]:
        """Return any call ASTs under AST."""
        return [c for c in self if isinstance(c, CallAST)]

    def get_vars_in_scope(self, root: "AST", keep_globals: bool = True) -> Dict:
        """Return all variables in enclosing scopes, optionally including globals."""
        vars_in_scope = _interface.dispatch(
            AST.get_vars_in_scope.__name__,
            root,
            self,
            keep_globals,
        )
        return vars_in_scope or []

    # AST traversal
    def traverse(self) -> Generator["AST", None, None]:
        """Traverse self in pre-order, yielding subtrees."""
        yield from self._perform_traverse(post_order=False)

    def post_traverse(self) -> Generator["AST", None, None]:
        """Traverse self in post-order, yielding subtrees."""
        yield from self._perform_traverse(post_order=True)

    def _perform_traverse(
        self,
        post_order: bool = False,
    ) -> Generator["AST", None, None]:
        """Perform an AST traversal in pre- or post-order, yielding subtrees."""
        if not post_order:
            yield self
        for child in self.children:
            yield from child._perform_traverse(post_order=post_order)
        if post_order:
            yield self

    def level_traverse(self) -> Generator["AST", None, None]:
        """Perform an AST traversal in level order, yielding subtrees."""
        queue = collections.deque([])
        queue.append(self)
        while queue:
            node = queue.popleft()
            yield node
            queue.extend(node.children)

    # AST mutation
    @staticmethod
    def cut(root: "AST", pt: "AST") -> "AST":
        """Return a new root with pt removed."""
        AST._root_mutation_check(root, pt)
        return _interface.dispatch(AST.cut.__name__, root, pt)

    @staticmethod
    def replace(root: "AST", pt: "AST", value: LiteralOrAST) -> "AST":
        """Return a new root with pt replaced with value."""
        value = AST._ensure_ast(value, language=root.language)

        AST._root_mutation_check(root, pt)
        AST._mutation_value_check(value)
        return _interface.dispatch(AST.replace.__name__, root, pt, value)

    @staticmethod
    def insert(root: "AST", pt: "AST", value: LiteralOrAST) -> "AST":
        """Return a new root with value inserted at pt."""
        value = AST._ensure_ast(value, language=root.language)

        AST._root_mutation_check(root, pt)
        AST._mutation_value_check(value)
        return _interface.dispatch(AST.insert.__name__, root, pt, value)

    @staticmethod
    def transform(
        root: "AST",
        transformer: Callable[["AST"], Optional[LiteralOrAST]],
    ) -> "AST":
        """
        Walk the AST tree in post-order, calling the transformer function
        on each AST in turn.  When the transformer function returns a new
        AST, the current node in the tree is replaced.

        For instance, to replace all "y" identifiers with "x", you would
        first define a transformer function as shown below:

        ```
        from asts import AST, LiteralOrAST, IdentifierAST
        def y_to_x(ast: AST) -> Optional[LiteralOrAST]:
            if isinstance(ast, IdentifierAST) and "y" == ast.source_text():
                return AST("x", language=ast.language)
        ```

        You would then use the transformer function to create a new AST
        with "y" replaced with "x", as shown below:

        ```
        new_ast = AST.transform(ast, y_to_x)
        ```

        See the python README for more information.
        """

        def transform_helper(
            transformer: Callable[["AST"], Optional[LiteralOrAST]],
            root: "AST",
            path: List = [],
        ) -> "AST":
            """Recursive helper function implementing the transform method."""

            # Transform the children of the AST node at PATH.
            new_root = root
            ast = new_root.lookup(path)
            for child_slot, arity in ast.child_slots():
                slot_value = ast.child_slot(child_slot)
                if slot_value:
                    children = slot_value if arity == 0 else [slot_value]
                else:
                    children = []

                for child in children:
                    new_root = transform_helper(
                        transformer, new_root, root.ast_path(child)
                    )

            # Get the result of calling the TRANSFORMER on the AST at PATH.
            ast = new_root.lookup(path)
            transformed = transformer(ast) or ast
            transformed = AST._ensure_ast(transformed, language=root.language)

            # Replace the current AST if there is a new TRANSFORMER result.
            if path == []:
                new_root = transformed  # special case for root node
            elif transformed != ast:
                new_root = AST.replace(new_root, ast, transformed)

            return new_root

        return transform_helper(transformer, root)

    # AST mutation helpers/sanity checks
    @staticmethod
    def _ensure_ast(value: LiteralOrAST, language: ASTLanguage) -> "AST":
        """Return the given value as an AST."""
        if isinstance(value, AST):
            return value
        else:
            return AST.from_string(str(value), language=language, deepest=True)

    @staticmethod
    def _root_mutation_check(root: "AST", pt: "AST") -> None:
        """Sanity check to ensure we are not mutating the root node directly."""
        assert root != pt, "Cannot mutate the root node of an AST."

    @staticmethod
    def _mutation_value_check(ast: "AST") -> None:
        """Sanity check to ensure we are not inserting a root node into another AST."""
        assert not isinstance(
            ast, RootAST
        ), "Cannot use a root node as a mutation value."


# Tree-sitter interface process management
class _interface:
    """
    interface between python and the sel process
    """

    _DEFAULT_CMD_NAME: Final[str] = "tree-sitter-interface"
    _DEFAULT_HOST: Final[str] = "localhost"
    _DEFAULT_PORT: Final[Optional[int]] = None
    _DEFAULT_STARTUP_WAIT: Final[int] = 3
    _DEFAULT_SOCKET_TIMEOUT: Final[int] = 300
    _DEFAULT_GC_THRESHOLD: Final[int] = 128
    _DEFAULT_QUIT_SENTINEL: Final[ByteString] = b"QUIT\n"

    _proc: ClassVar[Optional[subprocess.Popen]] = None
    _lock: ClassVar[multiprocessing.RLock] = multiprocessing.RLock()
    _gc_oids: ClassVar[List[int]] = []

    @staticmethod
    def is_process_running() -> bool:
        """Return TRUE if the Lisp subprocess is running."""
        return _interface._proc is not None and psutil.pid_exists(_interface._proc.pid)

    @staticmethod
    def _check_for_process_crash() -> None:
        """Check if the Lisp subprocess has crashed and, if so, throw an error."""
        if not _interface.is_process_running():
            stdout = _interface._proc.stdout.read().decode().strip()
            stderr = _interface._proc.stderr.read().decode().strip()

            msg = f"{_interface._DEFAULT_CMD_NAME} crashed."
            if stdout or stderr:
                msg = msg + f"\n\nstdout: {stdout}\n\nstderr: {stderr}"
            raise RuntimeError(msg)

    @staticmethod
    def start() -> None:
        """Start the tree-sitter-interface Lisp process."""
        with _interface._lock:
            if not _interface.is_process_running():
                # Find the interface binary, either on the $PATH or in an
                # installed python wheel.
                cmd = _interface._DEFAULT_CMD_NAME
                if not shutil.which(cmd):
                    cmd = pkg_resources.resource_filename(
                        __name__, _interface._DEFAULT_CMD_NAME
                    )
                    if not Path(cmd).exists():
                        raise RuntimeError(
                            f"{_interface._DEFAULT_CMD_NAME} binary must be on your $PATH."
                        )

                # Build the command line, listing on stdio or a port depending on
                # if a DEFAULT_PORT has been specified.
                if _interface._DEFAULT_PORT:
                    cmdline = [cmd, "--port", str(_interface._DEFAULT_PORT)]
                else:
                    cmdline = [cmd]

                # Startup the interface subprocess.
                _interface._proc = subprocess.Popen(
                    cmdline,
                    stdin=subprocess.PIPE,
                    stdout=subprocess.PIPE,
                    stderr=subprocess.PIPE,
                )

                # If the interface was built using the deploy package for use
                # in a python wheel file, read standard error waiting for the
                # requisite tree-sitter libraries to be loaded and the launching
                # application notification to be given.
                if cmd != _interface._DEFAULT_CMD_NAME:
                    lines = []
                    for line in _interface._proc.stderr:
                        line = line.decode().strip()
                        if line == "==> Launching application.":
                            break
                        lines.append(line)

                    if not _interface.is_process_running():
                        stderr = "\n".join(lines)
                        msg = f"{_interface._DEFAULT_CMD_NAME} crashed."
                        msg = msg + f"\n\nstderr: {stderr}"
                        raise RuntimeError(msg)

                # Wait _DEFAULT_STARTUP_WAIT seconds for the interface to
                # setup the socket for us to connect with if using ports.
                if _interface._DEFAULT_PORT:
                    for _ in range(_interface._DEFAULT_STARTUP_WAIT):
                        _interface._check_for_process_crash()
                        time.sleep(1.0)

                # Check if tree-sitter interface crashed on startup.
                _interface._check_for_process_crash()

    @staticmethod
    def stop() -> None:
        """Stop the tree-sitter-interface Lisp process."""
        if _interface.is_process_running():
            _interface._communicate(_interface._DEFAULT_QUIT_SENTINEL)

    @staticmethod
    def dispatch(*args: Tuple[Any], **kwargs: Dict[str, Any]) -> Any:
        """Dispatch processing to the tree-sitter-interface."""

        def handle_errors(data: Any) -> Any:
            """Check for errors in the subprocess reported in the JSON output."""
            if isinstance(data, dict) and data.get("error", None):
                raise ASTException(data["error"])

            return data

        def serialize(v: Any) -> Any:
            """Serialize V to a form for passing thru the JSON text interface."""
            if isinstance(v, AST):
                return {"type": "ast", "oid": v.oid}
            if isinstance(v, ASTLanguage):
                return v.name
            elif isinstance(v, dict):
                return {serialize(key): serialize(val) for key, val in v.items()}
            elif isinstance(v, list):
                return [serialize(i) for i in v]
            elif isinstance(v, tuple):
                return tuple(serialize(i) for i in v)
            else:
                return v

        def deserialize(v: Any) -> Any:
            """Deserialize V from the form used with the JSON text interface."""
            if isinstance(v, dict) and v.get("oid", None):
                return globals()[v["type"]](oid=v["oid"])
            elif isinstance(v, dict):
                return {deserialize(key): deserialize(val) for key, val in v.items()}
            elif isinstance(v, list):
                return [deserialize(i) for i in v]
            elif isinstance(v, tuple):
                return tuple(deserialize(i) for i in v)
            else:
                return v

        # Pop the function name from *args.
        fn = args[0]
        args = args[1:]

        # Special case: When garbage collection is occuring, place the
        # AST object id (oid) to be garbage collected on a queue to later
        # be flushed to the Lisp subprocess.  This protects us against
        # potential deadlocks in the locked section below if garbage
        # collection is initiated while we are in it and is more efficient
        # than pushing each oid to the Lisp subprocess individually.
        if fn == "__del__" and args[0] is not None:
            _interface._gc_oids.append(args[0])
            return

        # Build the request JSON to send to the subprocess.
        request = [fn] + serialize(list(args)) + serialize(list(kwargs.items()))
        request = f"{json.dumps(request)}\n".encode()

        # Send the request to the tree-sitter-interface and receive the response.
        response = _interface._communicate(request)

        # Load the response from the Lisp subprocess.
        return deserialize(handle_errors(json.loads(response.decode())))

    @staticmethod
    def _gc() -> None:
        """
        Flush the queue of garbage collected AST object ids (oids)
        to the Lisp subprocess.
        """
        if not _interface.is_process_running():
            _interface._gc_oids = []
        elif len(_interface._gc_oids) > _interface._DEFAULT_GC_THRESHOLD:
            request = [
                "gc",
                [_interface._gc_oids.pop() for _ in range(len(_interface._gc_oids))],
            ]
            request = f"{json.dumps(request)}\n".encode()
            _interface._communicate(request)

    @staticmethod
    def _communicate(request: ByteString) -> ByteString:
        """Communicate request to the Lisp subprocess and receive response."""

        def recvline(socket: socket.socket) -> ByteString:
            """Read a single line from the socket."""
            chunks = []
            while True:
                chunk = s.recv(1024)
                chunks.append(chunk)
                if chunk.endswith(b"\n"):
                    break

            response = "".join(chunks)
            return response

        # Send the request to the Lisp subprocess, either over a socket or
        # on standard input, and wait for a response.  This section is locked
        # to prevent issues with multiple threads writing at the same time.
        with _interface._lock:
            # Preliminaries:
            #  (1) Send list of object ids (oids) to garbage collect to the Lisp
            #      subprocess, if applicable.  See comment above re: deadlocks.
            #  (2) Check the process hasn't crashed before communicating with it.
            _interface._gc()
            _interface._check_for_process_crash()

            # Send the request and receive the response.
            if _interface._DEFAULT_PORT:
                with socket.socket(socket.AF_INET, socket.SOCK_STREAM) as s:
                    s.connect((_interface._DEFAULT_HOST, _interface._DEFAULT_PORT))
                    s.settimeout(_interface._DEFAULT_SOCKET_TIMEOUT)
                    s.sendall(request)
                    response = recvline(s).strip()
            else:
                _interface._proc.stdin.write(request)
                _interface._proc.stdin.flush()
                response = _interface._proc.stdout.readline().strip()

            # Post:
            #  (1) Check the process hasn't crashed after communicating with it.
            if request != _interface._DEFAULT_QUIT_SENTINEL:
                _interface._check_for_process_crash()

        return response


_interface.start()
atexit.register(_interface.stop)

# Generated tree-sitter AST types and user-defined method specializations
generate_types_file()
from .types import *  # noqa: E402, F401, F403


@add_method(FunctionAST)
def function_name(self: FunctionAST) -> str:
    """Return AST's name.  AST must be of type function."""
    return _interface.dispatch(FunctionAST.function_name.__name__, self)


@add_method(FunctionAST)
def function_parameters(self: FunctionAST) -> List[AST]:
    """Return AST's parameters.  AST must be of type function."""
    return _interface.dispatch(FunctionAST.function_parameters.__name__, self) or []


@add_method(FunctionAST)
def function_body(self: FunctionAST) -> AST:
    """Return AST's body.  AST must be of type function."""
    return _interface.dispatch(FunctionAST.function_body.__name__, self)


@add_method(CallAST)
def call_function(self: CallAST) -> AST:
    """Return AST's function.  AST must be of type call."""
    return _interface.dispatch(CallAST.call_function.__name__, self)


@add_method(CallAST)
def call_arguments(self: CallAST) -> List[AST]:
    """Return AST's arguments.  AST must be of type call."""
    return _interface.dispatch(CallAST.call_arguments.__name__, self) or []
