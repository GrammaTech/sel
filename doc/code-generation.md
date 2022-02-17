\begin{tikzpicture}
  [
    stage/.style = {text width=12em, text centered},
    ast/.style = {circle,draw=alum-6,fill=alum-1},
    module/.style = {draw,text width=8em},
    connection/.style = {thick},
    flow/.style = {connection, ->, dotted}
  ]

  \node[stage,text width=8em] (1) {
    \begin{enumerate}
      \setcounter{enumi}{0}
    \item Collect all\\ languages.
    \end{enumerate}
  };

  \node[above left=1 and -1 of 1] (lib) {{\tt /usr/lib/tree-sitter/\{language\}.so}};
  \node[above right=1 and -1 of 1] (share) {{\tt /usr/share/tree-sitter/\{language\}.json}};
  \node[below left=of 1] (nodes) {{\tt node-types.json}};
  \node[below right=of 1] (grammars) {{\tt grammar.json}};

  \draw[flow] (lib) to (1);
  \draw[flow] (share) to (1);
  \draw[flow] (1) to (nodes);
  \draw[flow] (1) to (grammars);

  \node[below=of nodes] (nodes-lisp) {{\tt node-types.lisp}};
  \node[below=of grammars] (grammars-lisp) {{\tt grammar.lisp}};

  \draw[flow] (nodes) to node[near start,left,text width=9em] {
    \begin{enumerate}
      \setcounter{enumi}{1}
    \item Load into lisp
    \end{enumerate}
  } (nodes-lisp);
  \draw[flow] (grammars) to node[near start,right,text width=9em] {
    \begin{enumerate}
      \setcounter{enumi}{1}
    \item Load into lisp
    \end{enumerate}
  } (grammars-lisp);

  \node[below=of nodes-lisp, stage] (node-patch) {
    \begin{enumerate}
      \setcounter{enumi}{2}
    \item Define classes
      \begin{itemize}
      \item Add slots
      \item Add mixins
      \item Patch node\\ structure
      \end{itemize}
    \end{enumerate}
  };

  \node[below=of grammars-lisp, stage] (grammar-patch) {
    \begin{enumerate}
      \setcounter{enumi}{3}
    \item Patch up grammars
    \end{enumerate}
  };

  \node[below=of node-patch, stage] (extend) {
    \begin{enumerate}
      \setcounter{enumi}{4}
    \item Extend classes with
      \begin{itemize}
      \item Methods
      \item Subclasses
      \item Extra slots
      \end{itemize}
    \end{enumerate}
  };

  \node[stage] (final) at (extend -| grammar-patch) {
    Final Lisp implementation\\
    is evaluated defining AST\\
    structure and functionality
  };

  \draw[flow] (nodes-lisp) to (node-patch);
  \draw[flow] (grammars-lisp) to (grammar-patch);
  \draw[flow] (node-patch) to (extend);
  \draw[flow] (grammar-patch) to (extend);
  \draw[flow] (extend) to (final);

  %% Local Variables:
  %% mode: latex
  %% end:
\end{tikzpicture}
