Generic Tree-Sitter AST API
===========================

The ASTs package provides a Python API into GrammaTech's Software
Evolution Library ([SEL][]) for source code manipulation.  SEL
generalizes over GitHub's [tree-sitter][] parsing libraries providing
a uniform interface over multiple programming languages (primarily
Python, JavaScript/TypeScript, C, C++, and Java; with lesser support
for Bash, CSS, Go HTML, and Rust), and providing additional
functionality for software inspection and modification.

[tree-sitter]: https://tree-sitter.github.io/tree-sitter/
[SEL]: https://grammatech.github.io/sel/index.html#Software-Evolution-Library

<!-- TODO: Setup automatic documentation building. -->
See the methods provided by asts.py for more information.

Example usage:

```
$ python3
Python 3.8.5
Type "help", "copyright", "credits" or "license" for more information.
>>> import asts
>>> it = asts.AST("x + 88", language=asts.ASTLanguage.Python)
>>> it.children()
[<asts.asts.AST object at 0x7f8e91fb52b0>]
>>> it.children()[0].children()
[<asts.asts.AST object at 0x7f8e918b7100>]
>>> it.children()[0].children()[0].children()
[<asts.asts.AST object at 0x7f8e918b7490>, <asts.asts.AST object at 0x7f8e918b73a0>, <asts.asts.AST object at 0x7f8e918b73d0>]
>>> it.children()[0].children()[0].children()[0].source_text()
'x'
>>> it.children()[0].children()[0].children()[1].source_text()
'+'
>>> it.children()[0].children()[0].children()[2].source_text()
'88'
>>> it.children()[0].children()[0].source_text()
'x + 88'
>>> it.children()[0].children()[0].child_slots()
[['PYTHON-LEFT', 1], ['PYTHON-OPERATOR', 1], ['PYTHON-RIGHT', 1], ['CHILDREN', 0]]
>>> list(map(lambda x:x.source_text(), it.children()[0].children()[0].children()))
['x', '+', '88']
>>> list(map(lambda x:x.ast_type(), it.children()[0].children()[0].children()))
['PYTHON-IDENTIFIER', 'PYTHON-+', 'PYTHON-INTEGER']
```
