import atexit
import json
import multiprocessing
import os
import pkg_resources
import shutil
import subprocess

from typing import Any, List, Optional, Tuple


class AST:
    def __init__(
        self,
        language: Optional[str] = "",
        source: Optional[str] = "",
        handle: Optional[int] = None,
    ) -> None:
        """
        Parse source-code string source of language and return the root
        of the resulting AST.
        """
        if handle is None:
            self.handle = _interface.dispatch(AST.__init__.__name__, language, source)
        else:
            self.handle = handle

    def __del__(self) -> None:
        if hasattr(self, "handle") and self.handle is not None:
            _interface.dispatch(AST.__del__.__name__, self)
            self.handle = None

    def __hash__(self) -> int:
        """Return the hashcode for the AST."""
        return _interface.dispatch(AST.__hash__.__name__, self)

    def __eq__(self, other: "AST") -> bool:
        """Return true if AST is equal to OTHER."""
        if isinstance(other, AST):
            return _interface.dispatch(AST.__eq__.__name__, self, other)
        return False

    def ast_at_point(self, line: int, column: int) -> "AST":
        """Return the most specific AST covering LINE and COLUMN."""
        return _interface.dispatch(AST.ast_at_point.__name__, self, line, column)

    def ast_language(self) -> str:
        """Return the AST's language."""
        return _interface.dispatch(AST.ast_language.__name__, self)

    def ast_refcount(self) -> int:
        """Return the AST's reference count."""
        return _interface.dispatch(AST.ast_refcount.__name__, self)

    def ast_type(self) -> str:
        """Return the AST's type."""
        return _interface.dispatch(AST.ast_type.__name__, self)

    def ast_types(self) -> List[str]:
        """Return the AST's type hierarchy."""
        return _interface.dispatch(AST.ast_types.__name__, self)

    def source_text(self) -> str:
        """Return a string of the AST's source text."""
        return _interface.dispatch(AST.source_text.__name__, self)

    def children(self) -> List["AST"]:
        """Return a list of the AST's children."""
        return _interface.dispatch(AST.children.__name__, self) or []

    def child_slots(self) -> List[Tuple[str, int]]:
        """Return a list of the AST's child slots."""
        return _interface.dispatch(AST.child_slots.__name__, self) or []

    def child_slot_arity(self, slot: str) -> Optional[int]:
        """Return the arity of the AST's child slot."""
        pairs = [pair for pair in self.child_slots() if pair[0].lower() == slot.lower()]
        if pairs:
            return pairs[0][1]
        else:
            return None

    def child_slot(self, slot: str) -> Any:
        """Return the contents of the AST's child slot value."""
        arity = self.child_slot_arity(slot)
        if arity is None:
            return None
        else:
            return _interface.dispatch(AST.child_slot.__name__, self, slot)

    def parent(self, root: "AST") -> "AST":
        """Return AST's parent under ROOT."""
        return _interface.dispatch(AST.parent.__name__, root, self)

    def function_asts(self) -> List["AST"]:
        """Return any function ASTs under AST."""
        return _interface.dispatch(AST.function_asts.__name__, self) or []

    def call_asts(self) -> List["AST"]:
        """Return any call ASTs under AST."""
        return _interface.dispatch(AST.call_asts.__name__, self) or []

    def get_vars_in_scope(self, root: "AST", keep_globals: bool = True) -> "AST":
        """Return all variables in enclosing scopes, optionally including globals."""
        names = _interface.dispatch(
            AST.get_vars_in_scope.__name__,
            root,
            self,
            keep_globals,
        )
        return names or []

    # AST slot accessors
    def ensure_type(self, desired_type: str) -> None:
        if desired_type not in self.ast_types():
            raise TypeError("AST is not of required type")

    def function_name(self) -> str:
        """Return AST's name.  AST must be of type function."""
        self.ensure_type("FUNCTION-AST")
        return _interface.dispatch(AST.function_name.__name__, self)

    def function_parameters(self) -> List["AST"]:
        """Return AST's parameters.  AST must be of type function."""
        self.ensure_type("FUNCTION-AST")
        return _interface.dispatch(AST.function_parameters.__name__, self) or []

    def function_body(self) -> "AST":
        """Return AST's body.  AST must be of type function."""
        self.ensure_type("FUNCTION-AST")
        return _interface.dispatch(AST.function_body.__name__, self)

    def call_module(self, root: "AST") -> Optional[str]:
        """Return AST's module.  AST must be of type call."""
        self.ensure_type("CALL-AST")
        return _interface.dispatch(AST.call_module.__name__, root, self)

    def call_function(self) -> "AST":
        """Return AST's function.  AST must be of type call."""
        self.ensure_type("CALL-AST")
        return _interface.dispatch(AST.call_function.__name__, self)

    def call_arguments(self) -> List["AST"]:
        """Return AST's arguments.  AST must be of type call."""
        self.ensure_type("CALL-AST")
        return _interface.dispatch(AST.call_arguments.__name__, self) or []


class ASTException(Exception):
    """specialization for exceptions in the AST tree-sitter interface"""

    pass


class _interface:
    """
    interface between python and the sel process
    """

    _DEFAULT_CMD_NAME: str = "tree-sitter-interface"
    _proc: Optional[subprocess.Popen] = None
    _lock: multiprocessing.Lock = multiprocessing.Lock()

    @staticmethod
    def is_process_running() -> bool:
        """Return TRUE if the LISP subprocess is running."""
        return _interface._proc is not None and _interface._proc.poll() is None

    @staticmethod
    def start() -> None:
        """Start the tree-sitter-interface LISP process."""
        with _interface._lock:
            if not _interface.is_process_running():
                cmd = _interface._DEFAULT_CMD_NAME
                if not shutil.which(cmd):
                    cmd = pkg_resources.resource_filename(
                        __name__, _interface._DEFAULT_CMD_NAME
                    )
                    if not os.path.exists(cmd):
                        raise RuntimeError(
                            f"{_interface._DEFAULT_CMD_NAME} binary must be on your $PATH."
                        )

                _interface._proc = subprocess.Popen(
                    [cmd],
                    stdin=subprocess.PIPE,
                    stdout=subprocess.PIPE,
                    stderr=subprocess.PIPE,
                )

                if cmd != _interface._DEFAULT_CMD_NAME:
                    for line in _interface._proc.stdout:
                        stdout = line.decode("ascii")
                        if stdout.strip() == "==> Launching application.":
                            break

    @staticmethod
    def stop() -> None:
        """Stop the tree-sitter-interface LISP process."""
        with _interface._lock:
            if _interface.is_process_running():
                _interface._proc.stdin.write(b"QUIT\n")
                _interface._proc.stdin.flush()

    @staticmethod
    def dispatch(fn: str, *args: Any) -> Any:
        """Dispatch processing to the tree-sitter-interface."""

        def handle_errors(data: Any) -> Any:
            """Check for errors in the subprocess reported in the JSON output."""
            if isinstance(data, dict) and data.get("error", None):
                raise ASTException(data["error"])

            return data

        def serialize(v: Any) -> Any:
            """Serialize V to a form for passing thru the JSON text interface."""
            if isinstance(v, AST):
                return {"type": "AST", "handle": v.handle}
            else:
                return v

        def deserialize(v: Any) -> Any:
            """Deserialize V from the form used with the JSON text interface."""
            if isinstance(v, dict) and v.get("handle", None):
                return AST(handle=v["handle"])
            elif isinstance(v, list):
                return [deserialize(e) for e in v]
            else:
                return v

        with _interface._lock:
            if not _interface.is_process_running():
                if fn == "__del__":
                    # Special case: When the python process is terminating,
                    # AST finalizers may be called after the interface is
                    # stopped.  In this case, since the process has been
                    # deallocated, simply return.
                    return None
                else:
                    # If this is not the special case, throw an error as the
                    # interface must be running for commands to be dispatched.
                    raise RuntimeError(
                        f"{_interface._DEFAULT_CMD_NAME} process not running."
                    )

            # This may be too cute, but we assume here the
            # name of the function to call matches the name
            # of the method being called on the AST class
            # (modulo some exceptions for leading/trailing
            # double underscores and underscores instead of
            # hyphens).  This enforces a correspondence in
            # names between the methods on ASTs and the
            # tree-sitter-interface.  Additionally, it helps
            # protect against minor programming errors where
            # the wrong function name is passed in.
            fn = fn.replace("__", "").replace("_", "-")

            # Write the function and arguments to the LISP subprocess.
            inpt = [fn] + [serialize(arg) for arg in args]
            _interface._proc.stdin.write(f"{json.dumps(inpt)}\n".encode("ascii"))
            _interface._proc.stdin.flush()

            # Read the response from stdout.
            stdout = _interface._proc.stdout.readline().decode("ascii").strip()

        # If standard output is not populated, the process crashed.
        # Raise a runtime error with the error message.
        if not stdout:
            stderr = _interface._proc.stderr.read().decode("ascii").strip()
            if stderr:
                msg = f"{_interface._DEFAULT_CMD_NAME} crashed with:\n\n{stderr}"
            else:
                msg = f"{_interface._DEFAULT_CMD_NAME} crashed."
            raise RuntimeError(msg)

        # Load the response from the LISP subprocess.
        return deserialize(handle_errors(json.loads(stdout)))


_interface.start()
atexit.register(_interface.stop)
