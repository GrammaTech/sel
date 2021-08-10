import unittest
import asts
import copy


class BinaryOperationTestDriver(unittest.TestCase):
    root = None
    binop = None

    def setUp(self):
        self.root = asts.AST("x + 88", asts.ASTLanguage.Python)
        self.binop = self.root.children()[0].children()[0]
        return

    # AST at point
    def test_ast_at_point(self):
        self.assertEqual("88", self.root.ast_at_point(1, 5).source_text())

    # AST source ranges
    def test_ast_source_ranges(self):
        ranges = [rnge for ast, rnge in self.root.ast_source_ranges()]
        self.assertEqual([[1, 1], [1, 7]], ranges[0])
        self.assertEqual([[1, 1], [1, 7]], ranges[1])
        self.assertEqual([[1, 1], [1, 7]], ranges[2])
        self.assertEqual([[1, 1], [1, 2]], ranges[3])
        self.assertEqual([[1, 2], [1, 4]], ranges[4])
        self.assertEqual([[1, 4], [1, 7]], ranges[5])

    # AST children
    def test_children(self):
        self.assertEqual(3, len(self.binop.children()))

    # AST parent
    def test_parent(self):
        children = self.binop.children()
        for child in children:
            self.assertEqual(self.binop.oid(), child.parent(self.root).oid())

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
        self.assertEqual(asts.ASTLanguage.Python, self.root.ast_language())

    # Reference count
    def test_ast_refcount(self):
        self.assertEqual(1, asts.AST.ast_refcount(self.root))

    # AST copy
    def test_ast_copy(self):
        root_copy = copy.copy(self.root)
        self.assertEqual(2, asts.AST.ast_refcount(root_copy))

    # AST deep copy
    def test_ast_deep_copy(self):
        root_copy = copy.deepcopy(self.root)
        self.assertEqual(2, asts.AST.ast_refcount(root_copy))

    # AST constructor deepest parameter
    def test_ast_constructor_deepest_parameter(self):
        new = asts.AST(
            self.root.source_text(), language=self.root.ast_language(), deepest=True
        )
        self.assertEqual(new.source_text(), self.root.source_text())
        self.assertNotEqual(new.ast_type(), self.root.ast_type())

    # AST Traverse
    def test_ast_traverse(self):
        self.assertEqual(6, len(list(self.root.traverse())))

    # AST __iter__
    def test_ast_iter(self):
        self.assertEqual(6, len(list(self.root)))


class SelfReferentialTestDriver(unittest.TestCase):
    source = None
    root = None

    def setUp(self):
        with open(__file__, "r") as f:
            self.source = f.read()
            self.root = asts.AST(self.source)

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
        ast = asts.AST("", asts.ASTLanguage.Python)
        self.assertEqual([], ast.function_asts())

    def test_no_params(self):
        ast = asts.AST("def foo(): return None", asts.ASTLanguage.Python)
        self.assertEqual(1, len(ast.function_asts()))

        function = ast.function_asts()[0]
        self.assertEqual("foo", function.function_name())
        self.assertEqual([], function.function_parameters())
        self.assertEqual("return None", function.function_body().source_text())

    def test_multiple_parameters(self):
        ast = asts.AST("def bar(a, b): return a*b", asts.ASTLanguage.Python)
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
        root = asts.AST("", asts.ASTLanguage.Python)
        self.assertEqual([], root.call_asts())

    def test_no_arguments(self):
        root = asts.AST("foo()", asts.ASTLanguage.Python)
        self.assertEqual(1, len(root.call_asts()))

        call = root.call_asts()[0]
        self.assertEqual(None, call.provided_by(root))
        self.assertEqual("foo", call.call_function().source_text())
        self.assertEqual([], call.call_arguments())

    def test_multiple_arguments(self):
        root = asts.AST("bar(a, b)", asts.ASTLanguage.Python)
        self.assertEqual(1, len(root.call_asts()))

        call = root.call_asts()[0]
        args = [a.source_text() for a in call.call_arguments()]
        self.assertEqual(None, call.provided_by(root))
        self.assertEqual("bar", call.call_function().source_text())
        self.assertEqual(["a", "b"], args)

    def test_provided_by(self):
        root = asts.AST("import os\nos.path.join(a, b)", asts.ASTLanguage.Python)
        self.assertEqual(1, len(root.call_asts()))

        call = root.call_asts()[0]
        self.assertEqual("os.path", call.provided_by(root))


class ErrorTestDriver(unittest.TestCase):
    def test_error_handling(self):
        with self.assertRaises(asts.ASTException):
            asts.AST("foo()", language="foo")


class VarsInScopeTestDriver(unittest.TestCase):
    def test_no_vars_in_scope(self):
        root = asts.AST("", asts.ASTLanguage.Python)
        self.assertEqual([], root.get_vars_in_scope(root))

    def test_vars_in_scope(self):
        root = asts.AST("def bar(a, b): return a*b", asts.ASTLanguage.Python)
        ast = root.children()[-1].children()[-1].children()[-1]
        vars_in_scope = ast.get_vars_in_scope(root)

        names = [var["name"] for var in vars_in_scope]
        self.assertEqual(names[0], "a")
        self.assertEqual(names[1], "b")
        self.assertEqual(names[2], "bar")

        scopes = [var["scope"].ast_type() for var in vars_in_scope]
        self.assertEqual(scopes[0], "PYTHON-FUNCTION-DEFINITION-2")
        self.assertEqual(scopes[1], "PYTHON-FUNCTION-DEFINITION-2")
        self.assertEqual(scopes[2], "PYTHON-MODULE")

        decls = [var["decl"].ast_type() for var in vars_in_scope]
        self.assertEqual(decls[0], "PYTHON-IDENTIFIER")
        self.assertEqual(decls[1], "PYTHON-IDENTIFIER")
        self.assertEqual(decls[2], "PYTHON-FUNCTION-DEFINITION-2")

    def test_vars_in_scope_no_globals(self):
        root = asts.AST("def bar(a, b): return a*b", asts.ASTLanguage.Python)
        ast = root.children()[-1].children()[-1].children()[-1]
        vars_in_scope = ast.get_vars_in_scope(root, keep_globals=False)

        names = [var["name"] for var in vars_in_scope]
        self.assertEqual(names[0], "a")
        self.assertEqual(names[1], "b")

        scopes = [var["scope"].ast_type() for var in vars_in_scope]
        self.assertEqual(scopes[0], "PYTHON-FUNCTION-DEFINITION-2")
        self.assertEqual(scopes[1], "PYTHON-FUNCTION-DEFINITION-2")

        decls = [var["decl"].ast_type() for var in vars_in_scope]
        self.assertEqual(decls[0], "PYTHON-IDENTIFIER")
        self.assertEqual(decls[1], "PYTHON-IDENTIFIER")


class ImportsTestDriver(unittest.TestCase):
    def test_no_imports(self):
        root = asts.AST("", asts.ASTLanguage.Python)
        self.assertEqual([], root.imports(root))

    def test_imports(self):
        code = "import os\nimport sys as s\nfrom json import dump\nprint('Hello')"
        root = asts.AST(code, asts.ASTLanguage.Python)
        ast = root.children()[-1]
        imports = ast.imports(root)
        self.assertEqual([["os"], ["sys", "s"], ["json", None, "dump"]], imports)


class UTF8TestDriver(unittest.TestCase):
    def test_utf8_multibyte_characters(self):
        root = asts.AST('"反复请求多次"', asts.ASTLanguage.Python)
        rnge = root.ast_source_ranges()[0][1]
        self.assertEqual('"反复请求多次"', root.source_text())
        self.assertEqual([[1, 1], [1, 9]], rnge)
