FROM quay.io/pypa/manylinux2010_x86_64

# Install required system packages
RUN yum install -y autoconf build-essential git curl wget sbcl libffi libffi-devel zlib-devel

# Install QuickLisp
RUN curl -O https://beta.quicklisp.org/quicklisp.lisp
RUN sbcl --load quicklisp.lisp \
        --eval '(quicklisp-quickstart:install)' \
        --eval '(let ((ql-util::*do-not-prompt* t)) (ql:add-to-init-file))'

# Install the latest version of SBCL
WORKDIR /
RUN git clone --branch sbcl-2.1.3 https://github.com/sbcl/sbcl
WORKDIR /sbcl
RUN find src/runtime -type f -exec sed -i 's/-ldl/-ldl -lrt/' {} \+
# RUN sh make.sh --dynamic-space-size=4Gb
RUN sh make.sh --dynamic-space-size=4Gb --fancy --with-sb-linkable-runtime --with-sb-dynamic-core
RUN sh install.sh

# Install tree-sitter
WORKDIR /
RUN git clone https://github.com/tree-sitter/tree-sitter
WORKDIR /tree-sitter
RUN PREFIX=/usr make all install
WORKDIR /
# Withheld languages: agda c-sharp julia ocaml/interface ocaml/ocaml php ql ruby scala
RUN for language in bash c cpp css go html java javascript jsdoc json python regex rust typescript/tsx typescript/typescript;do \
        [ -d tree-sitter-${language%/*} ] || git clone --depth=1 https://github.com/tree-sitter/tree-sitter-${language%/*};     \
        cd /tree-sitter-${language}/src;                                                                                        \
        if test -f "scanner.cc"; then                                                                                           \
            g++ -fPIC scanner.cc -c -lstdc++;                                                                               \
            gcc -std=c99 -fPIC parser.c -c;                                                                                   \
            g++ -shared scanner.o parser.o -o /usr/lib/tree-sitter-$(echo ${language}|sed 's|/|-|').so;                     \
        elif test -f "scanner.c"; then                                                                                          \
            gcc -std=c99 -fPIC scanner.c -c;                                                                                  \
            gcc -std=c99 -fPIC parser.c -c;                                                                                   \
            gcc -shared scanner.o parser.o -o /usr/lib/tree-sitter-$(echo ${language}|sed 's|/|-|').so;                       \
        else                                                                                                                    \
            gcc -std=c99 -fPIC parser.c -c;                                                                                   \
            gcc -shared parser.o -o /usr/lib/tree-sitter-$(echo ${language}|sed 's|/|-|').so;                                 \
        fi;                                                                                                                     \
        mkdir -p /usr/share/tree-sitter/${language}/;                                                                           \
        cp grammar.json node-types.json /usr/share/tree-sitter/${language};                                                     \
        cd -;                                                                                                                   \
    done
RUN git clone https://github.com/death/cl-tree-sitter /root/quicklisp/local-projects/cl-tree-sitter
# Work around bug in cl-unicode in quicklisp.
RUN git clone https://github.com/edicl/cl-unicode.git /root/quicklisp/local-projects/cl-unicode

# Pre-download and compile a number of dependency packages.
COPY . /root/quicklisp/local-projects/sel
RUN sed -i 's|(:default "libtree-sitter")|(:default "libtree-sitter") (:default "/usr/lib/libtree-sitter")|' /root/quicklisp/local-projects/cl-tree-sitter/low-level.lisp
RUN make -C /root/quicklisp/local-projects/sel dependencies
RUN sbcl --load "/root/quicklisp/setup.lisp" \
         --eval '(ql:quickload :software-evolution-library/deploy-tree-sitter-interface)' \
         --eval '(asdf:make :software-evolution-library/deploy-tree-sitter-interface)' \
         --eval '(quit)'
RUN sbcl --load "/root/quicklisp/setup.lisp" \
         --eval '(ql:quickload :software-evolution-library/run-tree-sitter-interface)' \
         --eval '(asdf:make :software-evolution-library/run-tree-sitter-interface)' \
         --eval '(quit)'

WORKDIR /root/quicklisp/local-projects/sel
