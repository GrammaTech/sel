import os
import unittest
import sel

class TestDriver(unittest.TestCase):
    source = "x + 88"
    ast = None
    binop = None

    def tearDown(self):
        return


    def setUp(self):
        self.ast = sel.AST(sel.lib.PYTHON, "x + 88")
        self.binop = self.ast.children()[0].children()[0]
        return


    # AST creation
    # AST source text
    def test_ast_creation_from_source(self):
        self.assertEqual(self.source, self.ast.source_text())


    # AST at point
    # def test_ast_at_point(self):
    #     self.assertEqual("88", self.ast.ast_at_point(1, 5).source_text())


    # AST children
    def test_children(self):
        self.assertEqual(3, len(self.binop.children()))


    # AST children-slots
    def test_child_slots(self):
        child_slots = self.binop.child_slots()
        # NOTE: Currently ((PYTHON-LEFT . 1) (PYTHON-OPERATOR . 1) (PYTHON-RIGHT . 1) (CHILDREN . 0))
        #       I think the last should probably be removed.
        self.assertEqual(4, len(child_slots))
        self.assertTrue("PYTHON-OPERATOR" in (list(map(lambda x: x[0], child_slots))))


    def test_child_slot_arity(self):
        self.assertEqual(1, self.binop.child_slot_arity("PYTHON-RIGHT"))


    def test_child_slot_accessor(self):
        self.assertEqual("88", self.binop.child_slot("PYTHON-RIGHT").source_text())


    # AST prefix
    # AST suffix
    # AST hash


    # AST type (ast_type)
    def test_ast_type(self):
        self.assertEqual(sel.lib.INTEGER, self.binop.child_slot("PYTHON-RIGHT").ast_type())


    # AST language (ast_language)
    def test_ast_language(self):
        self.assertEqual(sel.lib.PYTHON, self.ast.ast_language())


    # AST accessors
    # function name
    # function parameters
    # function body
    # call-function
    # call-arguments

    # More like light weight static analysis
    # AST parent
    # def test_parent(self):
    #     self.assertTrue(sel.lib.EXPRESSION == self.binop.parent(self.ast).ast_type())

    # AST enclosing function
    # AST in scope names
    # AST defined functions
    # AST callsites
    # AST callsite signatures
    # AST callsite module

if __name__ == "__main__":
    unittest.main()
