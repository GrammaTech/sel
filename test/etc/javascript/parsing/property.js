var o = {p: 42, q: true};
var {p, q} = o;
pp.parseTemplate = function({isTagged = false} = {}) {
  let node = this.startNode()
  this.next()
  node.expressions = []
  let curElt = this.parseTemplateElement({isTagged})
  node.quasis = [curElt]
  while (!curElt.tail) {
    if (this.type === tt.eof) this.raise(this.pos, "Unterminated template literal")
    this.expect(tt.dollarBraceL)
    node.expressions.push(this.parseExpression())
    this.expect(tt.braceR)
    node.quasis.push(curElt = this.parseTemplateElement({isTagged}))
  }
  this.next()
  return this.finishNode(node, "TemplateLiteral")
}
