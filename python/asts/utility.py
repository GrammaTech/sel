from typing import List

from .asts import AST
from .types import FunctionAST, CallAST


def function_asts(ast: AST) -> List[FunctionAST]:
    """Return any function ASTs under AST or itself."""
    return [c for c in ast if isinstance(c, FunctionAST)]


def call_asts(ast: AST) -> List[CallAST]:
    """Return any call ASTs under AST or itself."""
    return [c for c in ast if isinstance(c, CallAST)]
