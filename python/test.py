import os
import unittest
from sel import AST

class TestDriver(unittest.TestCase):


    def tearDown(self):
        return


    def setUp(self):
        return


    # AST creation
    # AST source text
    def test_ast_creation_from_source(self):
        source = "x + 88"
        self.assertTrue(source == AST(source=source).source_text())


    # AST at point
    # AST children
    # AST children-slots
    # AST prefix
    # AST suffix
    # AST hash
    # AST type (ast_type)
    # AST language (ast_language)

    # AST accessors
    # function name
    # function parameters
    # function body
    # call-function
    # call-arguments

    # More like light weight static analysis
    # AST parent
    # AST enclosing function
    # AST in scope names
    # AST defined functions
    # AST callsites
    # AST callsite signatures
    # AST callsite module

if __name__ == "__main__":
    unittest.main()
