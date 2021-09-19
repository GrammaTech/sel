from pathlib import Path
from shutil import which
from subprocess import PIPE, Popen
from typing import Any, Callable, Type


def generate_types_file() -> None:
    """
    Generate a python file with tree-sitter AST types using the
    tree-sitter-py-generator if such file does not yet exist.
    """
    types_file = Path(__file__).parent / "types.py"
    cmd = "tree-sitter-py-generator"

    if not types_file.exists():
        if not which(cmd):
            raise RuntimeError(f"{cmd} binary must be on your $PATH.")

        proc = Popen(cmd, stdout=PIPE, stderr=PIPE)
        stdout, stderr = proc.communicate()

        if stderr:
            raise RuntimeError(f"{cmd} crashed with:\n {stderr}")
        else:
            with open(types_file, "wb") as f:
                f.write(stdout)


def add_method(clazz: Type[Any]):
    """
    Decorator to add a method to a given python class.
    """

    def decorator(func: Callable):
        setattr(clazz, func.__name__, func)
        return func

    return decorator
