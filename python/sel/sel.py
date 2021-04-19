import inspect
import json
import multiprocessing
import shutil
import subprocess
import os
import pkg_resources

from typing import Any, List, Optional, Tuple, TypeVar

AST_Type = TypeVar("AST_Type", bound="AST")


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
            self.handle = _interface.dispatch(language, source)
        else:
            self.handle = handle
        if not self.handle:
            raise Exception("Failed to create AST")

    def __del__(self) -> None:
        _interface.dispatch(self)
        self.handle = None

    def __hash__(self) -> int:
        """Return the hashcode for the AST."""
        return _interface.dispatch(self)

    def __eq__(self, other: AST_Type) -> AST_Type:
        """Return true if AST is equal to OTHER."""
        return isinstance(other, AST) and _interface.dispatch(self, other)

    def ast_at_point(self, line: int, column: int) -> AST_Type:
        """Return the most specific AST covering LINE and COLUMN."""
        return _interface.dispatch(self, line, column)

    def ast_language(self) -> str:
        """Return the AST's language."""
        return _interface.dispatch(self)

    def ast_type(self) -> str:
        """Return the AST's type."""
        return _interface.dispatch(self)

    def ast_types(self) -> List[str]:
        """Return the AST's type hierarchy."""
        return _interface.dispatch(self)

    def source_text(self) -> str:
        """Return a string of the AST's source text."""
        return _interface.dispatch(self)

    def children(self) -> List[AST_Type]:
        """Return a list of the AST's children."""
        return _interface.dispatch(self) or []

    def child_slots(self) -> List[Tuple[str, int]]:
        """Return a list of the AST's child slots."""
        return _interface.dispatch(self) or []

    def child_slot_arity(self, slot: str) -> int:
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
            return _interface.dispatch(self, slot)

    def parent(self, root: AST_Type) -> AST_Type:
        """Return AST's parent under ROOT."""
        return _interface.dispatch(root, self)

    def function_asts(self) -> List[AST_Type]:
        """Return any function ASTs under AST."""
        return _interface.dispatch(self) or []

    def call_asts(self) -> List[AST_Type]:
        """Return any call ASTs under AST."""
        return _interface.dispatch(self) or []

    # AST slot accessors
    def ensure_type(self, desired_type: str) -> None:
        if desired_type not in self.ast_types():
            raise TypeError("AST is not of required type")

    def function_name(self) -> str:
        """Return AST's name.  AST must be of type function."""
        self.ensure_type("FUNCTION-AST")
        return _interface.dispatch(self)

    def function_parameters(self) -> List[AST_Type]:
        """Return AST's parameters.  AST must be of type function."""
        self.ensure_type("FUNCTION-AST")
        return _interface.dispatch(self) or []

    def function_body(self) -> AST_Type:
        """Return AST's body.  AST must be of type function."""
        self.ensure_type("FUNCTION-AST")
        return _interface.dispatch(self)

    def call_module(self, root: AST_Type) -> Optional[str]:
        """Return AST's module.  AST must be of type call."""
        self.ensure_type("CALL-AST")
        return _interface.dispatch(root, self)

    def call_function(self) -> AST_Type:
        """Return AST's function.  AST must be of type call."""
        self.ensure_type("CALL-AST")
        return _interface.dispatch(self)

    def call_arguments(self) -> List[AST_Type]:
        """Return AST's arguments.  AST must be of type call."""
        self.ensure_type("CALL-AST")
        return _interface.dispatch(self) or []


class _interface:
    """
    interface between python and the sel process
    """

    _pkg = False
    _cmd = "tree-sitter-interface"
    _proc = None
    _lock = multiprocessing.Lock()

    @staticmethod
    def is_process_running() -> bool:
        """Return TRUE if the LISP subprocess is running."""
        return _interface._proc is not None and _interface._proc.poll() is None

    @staticmethod
    def start() -> None:
        """Start the tree-sitter-interface LISP process."""
        with _interface._lock:
            if not _interface.is_process_running():
                if not shutil.which(_interface._cmd):
                    _local_cmd = pkg_resources.resource_filename(__name__, _interface._cmd)
                    if os.path.exists(_local_cmd):
                        _interface._cmd = _local_cmd
                        _interface._pkg = True
                    else:
                        Error(f"{_interface._cmd} binary must be on your $PATH.")

                _interface._proc = subprocess.Popen(
                    [_interface._cmd],
                    stdin=subprocess.PIPE,
                    stdout=subprocess.PIPE,
                    stderr=subprocess.PIPE,
                )

                if _interface._pkg:
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
    def dispatch(*args: List[Any]) -> Any:
        """Dispatch processing to the tree-sitter-interface."""

        def fn() -> str:
            """Return the name of the function to call."""

            # This may be too cute, but we assume here the
            # name of the function to call matches the name
            # of the method being called on the AST class
            # (modulo some exceptions for leading/trailing
            # double underscores and underscores instead of
            # hyphens).  This enforces a correspondence in
            # names between the method on ASTs and the
            # tree-sitter-interface.  Additionally, it helps
            # protect against minor programming errors where
            # the wrong function name is passed in.
            name = inspect.stack()[2].function
            return name.replace("__", "").replace("_", "-")

        def serialize(v: Any) -> Any:
            """Serialize V to a form for passing thru the JSON text interface."""
            if isinstance(v, AST):
                return {"type": "AST", "addr": v.handle}
            else:
                return v

        def deserialize(v: Any) -> Any:
            """Deserialize V from the form used with the JSON text interface."""
            if isinstance(v, dict) and v.get("addr", None):
                return AST(handle=v["addr"])
            elif isinstance(v, list):
                return [deserialize(e) for e in v]
            else:
                return v

        with _interface._lock:
            assert (
                _interface.is_process_running()
            ), f"{_interface._cmd} process not running."

            inpt = [fn()] + [serialize(arg) for arg in args]
            _interface._proc.stdin.write(json.dumps(inpt).encode("ascii"))
            _interface._proc.stdin.write(b"\n")
            _interface._proc.stdin.flush()

            # Large files can take a bit to process, so wait for a line with content.
            stdout = None
            for line in _interface._proc.stdout:
                stdout = line.decode("ascii")
                if stdout:
                    break

            # If standard output is not populated, the process crashed.
            # Raise a runtime error with the error message.
            if not stdout:
                stderr = _interface._proc.stderr.read().decode("ascii")
                raise RuntimeError(f"{_interface._cmd} crashed with:\n\n{stderr}")

            return deserialize(json.loads(stdout))


_interface.start()
