import unittest
import asts


class BinaryOperationTestDriver(unittest.TestCase):
    root = None
    binop = None

    def setUp(self):
        self.root = asts.AST("python", "x + 88")
        self.binop = self.root.children()[0].children()[0]
        return

    # AST at point
    def test_ast_at_point(self):
        self.assertEqual("88", self.root.ast_at_point(1, 5).source_text())

    # AST children
    def test_children(self):
        self.assertEqual(3, len(self.binop.children()))

    # AST parent
    def test_parent(self):
        children = self.binop.children()
        for child in children:
            self.assertEqual(self.binop, child.parent(self.root))

    # AST children-slots
    def test_child_slots(self):
        child_slots = self.binop.child_slots()
        # NOTE: Currently ((BEFORE-COMMENTS . 0)
        #                  (PYTHON-LEFT . 1)
        #                  (PYTHON-OPERATOR . 1)
        #                  (PYTHON-RIGHT . 1)
        #                  (CHILDREN . 0)
        #                  (AFTER-COMMENTS . 0))
        #       Consider removing CHILDREN.
        self.assertEqual(6, len(child_slots))
        self.assertTrue("PYTHON-OPERATOR" in (list(map(lambda x: x[0], child_slots))))

    # AST child-slot-arity
    def test_child_slot_arity(self):
        self.assertEqual(1, self.binop.child_slot_arity("PYTHON-RIGHT"))

    # AST child-slot
    def test_child_slot_accessor(self):
        self.assertEqual("88", self.binop.child_slot("PYTHON-RIGHT").source_text())

    # AST type
    def test_ast_type(self):
        integer = self.binop.child_slot("PYTHON-RIGHT")
        self.assertEqual("PYTHON-INTEGER", integer.ast_type())

    # AST language
    def test_ast_language(self):
        self.assertEqual("PYTHON", self.root.ast_language())


class SelfReferentialTestDriver(unittest.TestCase):
    source = None
    root = None

    def setUp(self):
        with open(__file__, "r") as f:
            self.source = f.read()
            self.root = asts.AST("python", self.source)

    # AST creation
    # AST source text
    def test_ast_creation_from_source(self):
        self.assertEqual(self.source, self.root.source_text())


class FunctionTestDriver(unittest.TestCase):
    # Function asts
    # Function name
    # Function parameters
    # Function body

    def test_no_functions(self):
        ast = asts.AST("python", "")
        self.assertEqual([], ast.function_asts())

    def test_no_params(self):
        ast = asts.AST("python", "def foo(): return None")
        self.assertEqual(1, len(ast.function_asts()))

        function = ast.function_asts()[0]
        self.assertEqual("foo", function.function_name())
        self.assertEqual([], function.function_parameters())
        self.assertEqual("return None", function.function_body().source_text())

    def test_multiple_parameters(self):
        ast = asts.AST("python", "def bar(a, b): return a*b")
        self.assertEqual(1, len(ast.function_asts()))

        function = ast.function_asts()[0]
        params = [p.source_text() for p in function.function_parameters()]
        self.assertEqual("bar", function.function_name())
        self.assertEqual(["a", "b"], params)
        self.assertEqual("return a*b", function.function_body().source_text())


class CallsiteTestDriver(unittest.TestCase):
    # Callsite asts
    # Callsite module
    # Callsite function
    # Callsite arguments

    def test_no_calls(self):
        root = asts.AST("python", "")
        self.assertEqual([], root.call_asts())

    def test_no_arguments(self):
        root = asts.AST("python", "foo()")
        self.assertEqual(1, len(root.call_asts()))

        call = root.call_asts()[0]
        self.assertEqual(None, call.call_module(root))
        self.assertEqual("foo", call.call_function().source_text())
        self.assertEqual([], call.call_arguments())

    def test_multiple_arguments(self):
        root = asts.AST("python", "bar(a, b)")
        self.assertEqual(1, len(root.call_asts()))

        call = root.call_asts()[0]
        args = [a.source_text() for a in call.call_arguments()]
        self.assertEqual(None, call.call_module(root))
        self.assertEqual("bar", call.call_function().source_text())
        self.assertEqual(["a", "b"], args)

    def test_module(self):
        root = asts.AST("python", "os.path.join(a, b)")
        self.assertEqual(1, len(root.call_asts()))

        call = root.call_asts()[0]
        self.assertEqual("os.path", call.call_module(root))
