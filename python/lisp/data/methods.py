from typing import Any, List


# Mock classes for typechecking
class AST:
    pass


class _interface:
    def dispatch(*args: Any) -> Any:
        pass


# Implementations
class FunctionAST:
    def function_name(self: "FunctionAST") -> str:
        """Return the function name of this AST."""
        return _interface.dispatch(FunctionAST.function_name.__name__, self)

    def function_parameters(self: "FunctionAST") -> List[AST]:
        """Return the function parameters of this AST."""
        return _interface.dispatch(FunctionAST.function_parameters.__name__, self) or []

    def function_body(self: "FunctionAST") -> AST:
        """Return the function body of this AST."""
        return _interface.dispatch(FunctionAST.function_body.__name__, self)


class CallAST:
    def call_function(self: "CallAST") -> AST:
        """Return the call function of this AST."""
        return _interface.dispatch(CallAST.call_function.__name__, self)

    def call_arguments(self: "CallAST") -> List[AST]:
        """Return the call arguments of this AST."""
        return _interface.dispatch(CallAST.call_arguments.__name__, self) or []
