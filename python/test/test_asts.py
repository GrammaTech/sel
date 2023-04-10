import unittest
import copy

from asts.asts import AST, ASTException, ASTLanguage, LiteralOrAST, PathOrAST
from asts.types import (
    CCastExpression,
    CComment,
    CompoundAST,
    CSourceTextFragment,
    CSourceTextFragmentTree,
    CStringLiteral,
    ErrorVariationPoint,
    ExpressionStatementAST,
    IdentifierAST,
    InnerParent,
    PythonAdd,
    PythonAssignment,
    PythonBinaryOperator,
    PythonCall,
    PythonComparisonOperator,
    PythonExpressionStatement0,
    PythonFunctionDefinition,
    PythonGreaterThan,
    PythonIdentifier,
    PythonInteger,
    PythonModule,
    RootAST,
    SourceTextFragmentVariationPoint,
    VariationPoint,
)
from asts.utility import call_asts, function_asts
from pathlib import Path
from typing import Optional, Text
from unittest.mock import patch

DATA_DIR = Path(__file__).parent / "data"


def slurp(path: Path) -> Text:
    """Return the text in the file at the given path."""
    with open(path, "r") as f:
        return f.read()


def contains_type(ast, type):
    """Return TRUE if AST or any of its children are instance of the given TYPE."""
    return any(isinstance(c, type) for c in ast)


class BinaryOperationTestDriver(unittest.TestCase):
    def setUp(self):
        self.root = AST.from_string("x + 88", ASTLanguage.Python)
        self.binop = self.root.children[0].children[0]
        return

    # AST at point
    def test_ast_at_point(self):
        self.assertEqual("88", self.root.ast_at_point(1, 5).source_text)

    # AST source ranges
    def test_ast_source_ranges(self):
        ranges = [rnge for ast, rnge in self.root.ast_source_ranges()]
        self.assertEqual(((1, 1), (1, 7)), ranges[0])
        self.assertEqual(((1, 1), (1, 7)), ranges[1])
        self.assertEqual(((1, 1), (1, 7)), ranges[2])
        self.assertEqual(((1, 1), (1, 2)), ranges[3])
        self.assertEqual(((1, 2), (1, 4)), ranges[4])
        self.assertEqual(((1, 4), (1, 7)), ranges[5])

    # AST path
    def test_ast_path(self):
        self.assertEqual(
            self.root.ast_path(self.binop), [("CHILDREN", 0), ("CHILDREN", 0)]
        )

    # AST lookup
    def test_ast_lookup(self):
        self.assertEqual(
            self.root.lookup([("CHILDREN", 0), ("CHILDREN", 0)]), self.binop
        )

    # AST children
    def test_children(self):
        self.assertEqual(3, len(self.binop.children))

    # AST parent
    def test_parent(self):
        for child in self.binop.children:
            self.assertEqual(self.binop, child.parent(self.root))

    # AST parents
    def test_parents(self):
        self.assertEqual([], self.root.parents(self.root))
        self.assertEqual(
            [PythonExpressionStatement0, PythonModule],
            [type(p) for p in self.binop.parents(self.root)],
        )

    # AST children-slots
    def test_child_slots(self):
        child_slots = self.binop.child_slots(internal=False)
        self.assertEqual(
            [("LEFT", 1), ("OPERATOR", 1), ("RIGHT", 1), ("CHILDREN", 0)], child_slots
        )

        child_slots = self.binop.child_slots(internal=True)
        self.assertEqual(
            [
                ("BEFORE-ASTS", 0),
                ("LEFT", 1),
                ("OPERATOR", 1),
                ("RIGHT", 1),
                ("CHILDREN", 0),
                ("AFTER-ASTS", 0),
            ],
            child_slots,
        )

    # AST child-slot-arity
    def test_child_slot_arity(self):
        self.assertEqual(1, self.binop.child_slot_arity("RIGHT"))

    # AST child-slot accessor
    def test_child_slot_accessor(self):
        def test_child_slot_accessor_helper(slotname):
            slot = self.binop.child_slot(slotname)
            assert slot
            self.assertEqual("88", slot.source_text)

        test_child_slot_accessor_helper("RIGHT")
        test_child_slot_accessor_helper("right")
        test_child_slot_accessor_helper("Right")

    # AST child-slot property
    def test_child_slot_property(self):
        self.assertEqual("88", self.binop.right.source_text)

    # AST size
    def test_size_property(self):
        self.assertEqual(6, self.root.size)
        self.assertEqual(len(list(self.root)), self.root.size)

    # AST type
    def test_ast_type(self):
        integer = self.binop.right
        self.assertIsInstance(integer, PythonInteger)

    # AST language
    def test_ast_language(self):
        self.assertEqual(ASTLanguage.Python, self.root.language)

    # Reference count
    def test_ast_refcount(self):
        self.assertEqual(1, self.root.refcount())

    # AST copy
    def test_ast_copy(self):
        root_copy = copy.copy(self.root)
        self.assertEqual(2, root_copy.refcount())

    # AST deep copy
    def test_ast_deep_copy(self):
        root_copy = copy.deepcopy(self.root)
        self.assertEqual(1, self.root.refcount())
        self.assertEqual(1, root_copy.refcount())
        self.assertEqual(root_copy.source_text, self.root.source_text)
        self.assertEqual(root_copy.serial_number, self.root.serial_number)
        self.assertNotEqual(root_copy.oid, self.root.oid)

    # AST constructor deepest parameter
    def test_ast_constructor_deepest_parameter(self):
        new = AST.from_string(
            self.root.source_text,
            language=self.root.language,
            deepest=True,
        )
        self.assertEqual(new.source_text, self.root.source_text)
        self.assertNotEqual(type(new), type(self.root))

    # AST Traverse
    def test_ast_traverse(self):
        asts = list(self.root.traverse())
        self.assertEqual(6, len(asts))
        self.assertEqual(
            [
                PythonModule,
                PythonExpressionStatement0,
                PythonBinaryOperator,
                PythonIdentifier,
                PythonAdd,
                PythonInteger,
            ],
            [type(ast) for ast in asts],
        )

    def test_ast_post_traverse(self):
        asts = list(self.root.post_traverse())
        self.assertEqual(6, len(asts))
        self.assertEqual(
            [
                PythonIdentifier,
                PythonAdd,
                PythonInteger,
                PythonBinaryOperator,
                PythonExpressionStatement0,
                PythonModule,
            ],
            [type(ast) for ast in asts],
        )

    # AST __iter__
    def test_ast_iter(self):
        asts = list(self.root)
        self.assertEqual(6, len(asts))
        self.assertEqual(
            [
                PythonModule,
                PythonExpressionStatement0,
                PythonBinaryOperator,
                PythonIdentifier,
                PythonAdd,
                PythonInteger,
            ],
            [type(ast) for ast in asts],
        )


class PrintFunctionCallTestDriver(unittest.TestCase):
    def setUp(self):
        self.root = AST.from_string("print(x)", ASTLanguage.Python, deepest=True)

    def test_lookup(self):
        x = self.root.lookup(["ARGUMENTS", ("CHILDREN", 0)])
        assert x
        self.assertEqual(x.source_text, "x")
        self.assertIsInstance(x, PythonIdentifier)

    def test_ast_path(self):
        x = self.root.children[1].children[0]
        self.assertEqual(self.root.ast_path(x), ["ARGUMENTS", ("CHILDREN", 0)])


class ASTTemplatesTestDriver(unittest.TestCase):
    def test_ast_template(self):
        a = AST.ast_template("$ID = 1", ASTLanguage.Python, id="x")
        self.assertEqual(a.source_text, "x = 1")
        self.assertIsInstance(a, PythonAssignment)

        a = AST.ast_template("fn(@ARGS)", ASTLanguage.Python, args=[1, 2, 3])
        self.assertEqual(a.source_text, "fn(1, 2, 3)")
        self.assertIsInstance(a, PythonCall)

        a = AST.ast_template("$1 = $2", ASTLanguage.Python, "x", 1)
        self.assertEqual(a.source_text, "x = 1")
        self.assertIsInstance(a, PythonAssignment)

        a = AST.ast_template("fn(@1)", ASTLanguage.Python, [1, 2, 3])
        self.assertEqual(a.source_text, "fn(1, 2, 3)")
        self.assertIsInstance(a, PythonCall)

        lhs = AST.from_string("x", ASTLanguage.Python, deepest=True)
        a = AST.ast_template("$1 = value", ASTLanguage.Python, lhs)
        self.assertEqual(a.source_text, "x = value")
        self.assertIsInstance(a, PythonAssignment)

        template = "$LEFT_HAND_SIDE = $RIGHT_HAND_SIDE"
        a = AST.ast_template(
            template, ASTLanguage.Python, left_hand_side="x", right_hand_side=1
        )
        self.assertEqual(a.source_text, "x = 1")
        self.assertIsInstance(a, PythonAssignment)

    def test_asts_from_template(self):
        asts = AST.asts_from_template("$1;", ASTLanguage.C, '"Foo: %d"')
        self.assertEqual(len(asts), 1)
        self.assertEqual(asts[0].source_text, '"Foo: %d"')
        self.assertIsInstance(asts[0], CStringLiteral)

        asts = AST.asts_from_template("$1 + $2", ASTLanguage.Python, "x", 1)
        self.assertEqual(len(asts), 2)
        self.assertEqual(asts[0].source_text, "x")
        self.assertIsInstance(asts[0], PythonIdentifier)
        self.assertEqual(asts[1].source_text, "1")
        self.assertIsInstance(asts[1], PythonInteger)


class CopyTestDriver(unittest.TestCase):
    def setUp(self):
        self.root = AST.from_string("x + 1", ASTLanguage.Python, deepest=True)

    def test_copy_no_kwargs(self):
        copy = AST.copy(self.root)
        self.assertEqual(1, self.root.refcount())
        self.assertEqual(1, copy.refcount())
        self.assertEqual(copy.source_text, self.root.source_text)
        self.assertEqual(copy.serial_number, self.root.serial_number)
        self.assertNotEqual(copy.oid, self.root.oid)

    def test_copy_with_kwargs(self):
        copy = AST.copy(
            self.root,
            left=AST.from_string("y", ASTLanguage.Python, deepest=True),
        )
        self.assertEqual(copy.source_text, "y + 1")
        self.assertEqual(copy.serial_number, self.root.serial_number)
        self.assertNotEqual(copy.oid, self.root.oid)

        copy = AST.copy(self.root, left=0.5)
        self.assertEqual(copy.source_text, "0.5 + 1")
        self.assertEqual(copy.serial_number, self.root.serial_number)
        self.assertNotEqual(copy.oid, self.root.oid)

        copy = AST.copy(self.root, left=2)
        self.assertEqual(copy.source_text, "2 + 1")
        self.assertEqual(copy.serial_number, self.root.serial_number)
        self.assertNotEqual(copy.oid, self.root.oid)

        copy = AST.copy(self.root, left='"hi"')
        self.assertEqual(copy.source_text, '"hi" + 1')
        self.assertEqual(copy.serial_number, self.root.serial_number)
        self.assertNotEqual(copy.oid, self.root.oid)

        copy = AST.copy(self.root, left="y")
        self.assertEqual(copy.source_text, "y + 1")
        self.assertEqual(copy.serial_number, self.root.serial_number)
        self.assertNotEqual(copy.oid, self.root.oid)


class MutationTestDriver(unittest.TestCase):
    def setUp(self):
        self.root = AST.from_string("x = 88\n", ASTLanguage.Python)
        self.statement = self.root.children[0]
        self.path = self.root.ast_path(self.statement)
        return

    def cut_driver(self, pt: PathOrAST):
        new_root = AST.cut(self.root, pt)
        self.assertNotEqual(new_root.oid, self.root.oid)
        self.assertEqual(new_root.serial_number, self.root.serial_number)
        self.assertEqual(0, len(new_root.children))
        self.assertEqual("", new_root.source_text)

    def replace_driver(self, pt: PathOrAST):
        new_ast = AST.from_string("y = 2\n", language=ASTLanguage.Python, deepest=True)
        new_root = AST.replace(self.root, pt, new_ast)
        self.assertNotEqual(new_root.oid, self.root.oid)
        self.assertEqual(new_root.serial_number, self.root.serial_number)
        self.assertEqual(1, len(new_root.children))
        self.assertEqual("y = 2\n", new_root.source_text)

    def replace_root_driver(self, pt: PathOrAST):
        new_ast = AST.from_string("y = 2\n", language=ASTLanguage.Python, deepest=True)
        new_root = AST.replace(self.root, pt, new_ast)
        self.assertNotEqual(new_root.oid, self.root.oid)
        self.assertNotEqual(new_root.serial_number, self.root.serial_number)
        self.assertEqual(1, len(new_root.children))
        self.assertEqual("y = 2", new_root.source_text)

    def insert_driver(self, pt: PathOrAST):
        new_ast = AST.from_string("y = 2\n", language=ASTLanguage.Python, deepest=True)
        new_root = AST.insert(self.root, pt, new_ast)
        self.assertNotEqual(new_root.oid, self.root.oid)
        self.assertEqual(new_root.serial_number, self.root.serial_number)
        self.assertEqual(2, len(new_root.children))
        self.assertEqual("y = 2\nx = 88\n", new_root.source_text)

    def test_cut_path(self):
        self.cut_driver(self.statement)
        self.cut_driver(self.path)

    def test_replace_ast(self):
        self.replace_driver(self.statement)
        self.replace_driver(self.path)

    def test_replace_root_ast(self):
        self.replace_root_driver(self.root)
        self.replace_root_driver([])

    def test_insert_ast(self):
        self.insert_driver(self.statement)
        self.insert_driver(self.path)

    def test_replace_literal(self):
        lhs = self.statement.children[0].children[0]
        new_root = AST.replace(self.root, lhs, "y")
        self.assertNotEqual(new_root.oid, self.root.oid)
        self.assertEqual(new_root.serial_number, self.root.serial_number)
        self.assertEqual("y = 88\n", new_root.source_text)

    def test_cut_throws_exception_not_in_tree(self):
        new_pt = AST.from_string("new = 0\n", ASTLanguage.Python, deepest=True)
        with self.assertRaises(ASTException):
            AST.cut(self.root, new_pt)

    def test_replace_throws_exception_no_replacement(self):
        with self.assertRaises(ASTException):
            AST.replace(self.root, self.statement, None)

    def test_insert_throws_exception_no_replacement(self):
        with self.assertRaises(ASTException):
            AST.insert(self.root, self.statement, None)


class TransformTestDriver(unittest.TestCase):
    def setUp(self):
        text = slurp(DATA_DIR / "transform" / "original.py")
        self.root = AST.from_string(text, ASTLanguage.Python)

    def test_transform_x_to_y(self):
        def x_to_y(ast: AST) -> Optional[LiteralOrAST]:
            """Convert 'x' identifier ASTs to 'y'."""
            result = None
            if isinstance(ast, IdentifierAST) and "x" == ast.source_text:
                result = "y"
            return result

        transformed = AST.transform(self.root, x_to_y)
        expected = slurp(DATA_DIR / "transform" / "transform_x_to_y.py")
        self.assertEqual(transformed.source_text, expected)

    def test_transform_x_to_z_gt(self):
        def x_to_z_gt(ast: AST) -> Optional[LiteralOrAST]:
            """Convert 'x' identifiers in greater than operations to 'z'."""
            result = None
            if isinstance(ast, PythonComparisonOperator):
                children = ast.child_slot("CHILDREN") or []
                operators = ast.child_slot("OPERATORS") or []
                lhs, *rest = children
                operator, *_ = operators
                if isinstance(operator, PythonGreaterThan) and lhs.source_text == "x":
                    result = AST.copy(ast, children=["z", *rest])
            return result

        transformed = AST.transform(self.root, x_to_z_gt)
        expected = slurp(DATA_DIR / "transform" / "transform_x_to_z_gt.py")
        self.assertEqual(transformed.source_text, expected)

    def test_delete_print_statements(self):
        def is_print_statement(ast: AST) -> bool:
            """Return TRUE if AST is an statement calling the print function."""
            if isinstance(ast, ExpressionStatementAST):
                fn_calls = [c.call_function().source_text for c in call_asts(ast)]
                return "print" in fn_calls
            return False

        def delete_print_statements(ast: AST) -> Optional[LiteralOrAST]:
            """Delete all print statements from the children of AST."""
            result = None
            if isinstance(ast, RootAST) or isinstance(ast, CompoundAST):
                # Build a list of new children under the AST, eliding print statements.
                new_children = [c for c in ast.children if not is_print_statement(c)]

                # Special case; if no children remain, add a "pass" statement nop to
                # avoid syntax errors.
                pass_stmt = AST.from_string("pass\n", ASTLanguage.Python, deepest=True)
                new_children = new_children if new_children else [pass_stmt]
                result = AST.copy(ast, children=new_children)
            return result

        transformed = AST.transform(self.root, delete_print_statements)
        expected = slurp(DATA_DIR / "transform" / "delete_print_statements.py")
        self.assertEqual(transformed.source_text, expected)


class CCastExpressionTransformTestDriver(unittest.TestCase):
    def setUp(self):
        text = slurp(DATA_DIR / "transform" / "ccast-original.c")
        self.root = AST.from_string(text, ASTLanguage.C)

    def test_remove_cast(self):
        def remove_cast(ast: AST):
            if isinstance(ast, CCastExpression):
                return ast.value

        transformed = AST.transform(self.root, remove_cast)
        expected = slurp(DATA_DIR / "transform" / "ccast-transformed.c")
        self.assertEqual(transformed.source_text, expected)


class FunctionTestDriver(unittest.TestCase):
    # Function asts
    # Function name
    # Function parameters
    # Function body

    def test_no_functions(self):
        ast = AST.from_string("", ASTLanguage.Python)
        self.assertEqual([], function_asts(ast))

    def test_no_params(self):
        ast = AST.from_string("def foo(): return None", ASTLanguage.Python)
        self.assertEqual(1, len(function_asts(ast)))

        function = function_asts(ast)[0]
        self.assertEqual("foo", function.function_name())
        self.assertEqual([], function.function_parameters())
        self.assertEqual("return None", function.function_body().source_text)

    def test_multiple_parameters(self):
        ast = AST.from_string("def bar(a, b): return a*b", ASTLanguage.Python)
        self.assertEqual(1, len(function_asts(ast)))

        function = function_asts(ast)[0]
        params = [p.source_text for p in function.function_parameters()]
        self.assertEqual("bar", function.function_name())
        self.assertEqual(["a", "b"], params)
        self.assertEqual("return a*b", function.function_body().source_text)


class CallsiteTestDriver(unittest.TestCase):
    # Callsite asts
    # Callsite module
    # Callsite function
    # Callsite arguments

    def test_no_calls(self):
        root = AST.from_string("", ASTLanguage.Python)
        self.assertEqual([], call_asts(root))

    def test_no_arguments(self):
        root = AST.from_string("foo()", ASTLanguage.Python)
        self.assertEqual(1, len(call_asts(root)))

        call = call_asts(root)[0]
        self.assertEqual(None, call.provided_by(root))
        self.assertEqual("foo", call.call_function().source_text)
        self.assertEqual([], call.call_arguments())

    def test_multiple_arguments(self):
        root = AST.from_string("bar(a, b)", ASTLanguage.Python)
        self.assertEqual(1, len(call_asts(root)))

        call = call_asts(root)[0]
        args = [a.source_text for a in call.call_arguments()]
        self.assertEqual(None, call.provided_by(root))
        self.assertEqual("bar", call.call_function().source_text)
        self.assertEqual(["a", "b"], args)

    def test_provided_by(self):
        root = AST.from_string("import os\nos.path.join(a, b)", ASTLanguage.Python)
        self.assertEqual(1, len(call_asts(root)))

        call = call_asts(root)[0]
        self.assertEqual("os.path", call.provided_by(root))


class ErrorTestDriver(unittest.TestCase):
    def test_error_handling(self):
        with self.assertRaises(ASTException):
            AST.from_string("foo()", language="foo")


class VarsInScopeTestDriver(unittest.TestCase):
    def test_no_vars_in_scope(self):
        root = AST.from_string("", ASTLanguage.Python)
        self.assertEqual([], root.get_vars_in_scope(root))

    def test_vars_in_scope(self):
        root = AST.from_string("def bar(a, b): return a*b", ASTLanguage.Python)
        ast = root.children[-1].children[-1].children[-1]
        vars_in_scope = ast.get_vars_in_scope(root)

        names = [var["name"] for var in vars_in_scope]
        self.assertEqual(names[0], "a")
        self.assertEqual(names[1], "b")
        self.assertEqual(names[2], "bar")

        scopes = [var["scope"] for var in vars_in_scope]
        self.assertIsInstance(scopes[0], PythonFunctionDefinition)
        self.assertIsInstance(scopes[1], PythonFunctionDefinition)
        self.assertIsInstance(scopes[2], PythonModule)

        decls = [var["decl"] for var in vars_in_scope]
        self.assertIsInstance(decls[0], PythonIdentifier)
        self.assertIsInstance(decls[1], PythonIdentifier)
        self.assertIsInstance(decls[2], PythonFunctionDefinition)

    def test_vars_in_scope_no_globals(self):
        root = AST.from_string("def bar(a, b): return a*b", ASTLanguage.Python)
        ast = root.children[-1].children[-1].children[-1]
        vars_in_scope = ast.get_vars_in_scope(root, keep_globals=False)

        names = [var["name"] for var in vars_in_scope]
        self.assertEqual(names[0], "a")
        self.assertEqual(names[1], "b")

        scopes = [var["scope"] for var in vars_in_scope]
        self.assertIsInstance(scopes[0], PythonFunctionDefinition)
        self.assertIsInstance(scopes[1], PythonFunctionDefinition)

        decls = [var["decl"] for var in vars_in_scope]
        self.assertIsInstance(decls[0], PythonIdentifier)
        self.assertIsInstance(decls[1], PythonIdentifier)


class ImportsTestDriver(unittest.TestCase):
    def test_no_imports(self):
        root = AST.from_string("", ASTLanguage.Python)
        self.assertEqual([], root.imports(root))

    def test_imports(self):
        code = "import os\nimport sys as s\nfrom json import dump\nprint('Hello')"
        root = AST.from_string(code, ASTLanguage.Python)
        ast = root.children[-1]
        imports = ast.imports(root)
        self.assertEqual([["os"], ["sys", "s"], ["json", None, "dump"]], imports)


class EncodingTestDriver(unittest.TestCase):
    def test_encoding(self):
        def run_test_encoding(text, encoding):
            root = AST.from_string(
                text.encode(encoding).decode(encoding),
                ASTLanguage.Python,
            )
            self.assertEqual(text, root.source_text)

        run_test_encoding('"反复请求多次"', "utf_8")
        run_test_encoding("This file is in UTF-8! 👍!", "utf_8")
        run_test_encoding("This file is in Latin-1! Wöw!", "latin_1")


class InnerParentTestDriver(unittest.TestCase):
    def test_inner_parent_asts(self):
        text = """__all__ = [
    "c", # first comment
    # second comment
]"""
        root = AST.from_string(text, ASTLanguage.Python)
        inner_parent = root.children[0].children[0].children[1].children[1]
        self.assertIsInstance(inner_parent, InnerParent)
        self.assertIsNone(inner_parent.language)
        self.assertTrue(inner_parent.source_text.startswith(" # first comment"))


class SimpleParseTestDriver(unittest.TestCase):
    def test_typescript_parse(self):
        self.simple_parse_driver(
            "let message: string = 'Hello World!'\nconsole.log(message)",
            ASTLanguage.TypescriptTs,
        )

    def test_java_parse(self):
        self.simple_parse_driver("import foo;", ASTLanguage.Java)

    def test_rust_parse(self):
        self.simple_parse_driver('println!("Hello, world!");', ASTLanguage.Rust)

    def simple_parse_driver(self, text, language):
        root = AST.from_string(text, language)
        self.assertEqual(root.language, language)
        self.assertEqual(root.source_text, text)


class CommentTestDriver(unittest.TestCase):
    def setUp(self):
        self.root = AST.from_string("{ /* comment */ }", ASTLanguage.C)

    def test_lookup(self):
        path = [["CHILDREN", 0], ["INTERNAL-ASTS-0", 0]]
        self.assertIsInstance(self.root.lookup(path), CComment)

    def test_transform_does_not_crash(self):
        transformed = AST.transform(self.root, lambda ast: None)
        self.assertEqual(self.root.source_text, transformed.source_text)


class VariationPointTestDriver(unittest.TestCase):
    def test_error_variation_point(self):
        text = "int"
        root = AST.from_string(text, ASTLanguage.C, error_tree=False)
        self.assertTrue(contains_type(root, ErrorVariationPoint))
        self.assertEqual(root.source_text, text)

    def test_source_text_fragment_variation_point(self):
        text = "int i"
        root = AST.from_string(text, ASTLanguage.C, error_tree=False)
        self.assertTrue(contains_type(root, SourceTextFragmentVariationPoint))
        self.assertEqual(root.source_text, text)

    def test_error_tree_true(self):
        root = AST.from_string("int i", ASTLanguage.C, error_tree=True)
        self.assertTrue(contains_type(root, CSourceTextFragmentTree))
        self.assertFalse(contains_type(root, CSourceTextFragment))

    def test_error_tree_false(self):
        root = AST.from_string("int i", ASTLanguage.C, error_tree=False)
        self.assertTrue(contains_type(root, CSourceTextFragment))
        self.assertFalse(contains_type(root, CSourceTextFragmentTree))

    def test_variation_point_lookup(self):
        text = "int"
        root = AST.from_string(text, ASTLanguage.C)
        child_slots = root.child_slots(internal=True)
        self.assertEqual(
            [
                ("BEFORE-ASTS", 0),
                ("CHILDREN", 0),
                ("INTERNAL-ASTS-0", 0),
                ("AFTER-ASTS", 0),
            ],
            child_slots,
        )
        pt = root.lookup([("INTERNAL-ASTS-0", 0)])
        self.assertIsInstance(pt, VariationPoint)


class NoneChildrenTestDriver(unittest.TestCase):
    def test_children_nil_slot(self):
        root = AST.from_string("int i", ASTLanguage.C)
        to_cut = root.children[0].children[0]
        new_root = AST.cut(root, to_cut)

        self.assertEqual(new_root.children[0].children, [])
        self.assertEqual(new_root.size, 2)
        self.assertEqual(len(list(new_root)), 2)


class KeyboardInterruptionTestDriver(unittest.TestCase):
    def test_keyboard_interruption(self):
        # Keyboard Interrupt signals can unroll the stack before the response
        # is received from the lisp interface. The python interface should be
        # able to recover from the disjunct packages when this happens.
        ast = AST.from_string("x = 4", ASTLanguage.Python)

        with patch(
            "asts.asts._interface._proc.stdout.readline", side_effect=KeyboardInterrupt
        ):
            try:
                ast.children
            except KeyboardInterrupt:
                pass

        source_text = ast.source_text
        assert isinstance(source_text, str)

    def test_keyboard_interruption_exception(self):
        # The Python interface doesn't raise an exception unless the erroneous
        # message matches the ID of the current request.
        root = AST.from_string("x = 88\n", ASTLanguage.Python)
        new_pt = AST.from_string("new = 0\n", ASTLanguage.Python, deepest=True)

        with patch(
            "asts.asts._interface._proc.stdout.readline", side_effect=KeyboardInterrupt
        ):
            try:
                AST.cut(root, new_pt)
            except KeyboardInterrupt:
                pass

        source_text = root.source_text
        assert isinstance(source_text, str)
