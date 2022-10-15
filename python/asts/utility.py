from typing import Any, Callable, Type


def add_method(clazz: Type[Any]):
    """
    Decorator to add a method to a given python class.
    """

    def decorator(func: Callable):
        setattr(clazz, func.__name__, func)
        return func

    return decorator
