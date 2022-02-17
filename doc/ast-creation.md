\begin{tikzpicture}
  [
    node distance = 4em,
    stage/.style = {text width=12em, text centered},
    connection/.style = {thick},
    flow/.style = {connection, ->, dotted}
  ]

  \node[] (lib) {{\tt /usr/lib/tree-sitter/\{language\}.so}};
  \node[below=of lib] (tree) {AST functional tree};
  \node[below=of tree] (ttree) {Transformed tree};
  \node[below=of ttree] (atree) {Annotated tree};
  \node[below=of atree] (btree) {Matched tree};
  \node[below=of btree] (ctree) {Tree of classes};
  \node[below=of ctree] (itree) {Tree of indented classes};

  \draw[flow] (lib) to node[auto,right,stage] {
    \begin{enumerate}
      \setcounter{enumi}{0}
    \item Call {\tt .so} and convert C struct to lisp tree.
    \end{enumerate}
  } (tree);
  \draw[flow] (tree) to node[auto,right,stage] {
    \begin{enumerate}
      \setcounter{enumi}{1}
    \item Traverse tree applying any transformations.
    \end{enumerate}
  } (ttree);
  \draw[flow] (ttree) to node[auto,right,stage] {
    \begin{enumerate}
      \setcounter{enumi}{2}
    \item Add surrounding text and inner white-space.
    \end{enumerate}
  } (atree);
  \draw[flow] (atree) to node[auto,right,stage] {
    \begin{enumerate}
      \setcounter{enumi}{3}
    \item Choice expansion subclasses found if needed.
    \end{enumerate}
  } (btree);
  \draw[flow] (btree) to node[auto,right,stage] {
    \begin{enumerate}
      \setcounter{enumi}{4}
    \item Classes instantiated and slots populated.
    \end{enumerate}
  } (ctree);
  \draw[flow] (ctree) to node[auto,near start,right,stage] {
    \begin{enumerate}
      \setcounter{enumi}{5}
    \item Indentation converted from strings to numbers.
    \end{enumerate}
  } (itree);

  %% Local Variables:
  %% mode: latex
  %% end:
\end{tikzpicture}
