FROM quay.io/pypa/manylinux_2_24_x86_64

# Install required system packages
RUN apt-get -y --fix-missing update && \
    apt-get install -y autoconf build-essential git curl wget sbcl libffi-dev zlib1g-dev

# Rebuild SBCL from scratch from a tag, enabling dynamic core so users
# can expand the memory with a command line option
RUN git clone --branch sbcl-2.2.5 https://git.code.sf.net/p/sbcl/sbcl /root/sbcl
RUN cd /root/sbcl && bash make.sh --prefix=/usr --with-sb-linkable-runtime --with-sb-dynamic-core --dynamic-space-size=8Gb
RUN apt-get -y remove sbcl
RUN cd /root/sbcl && bash install.sh

# Install QuickLisp
RUN curl -O https://beta.quicklisp.org/quicklisp.lisp
RUN sbcl --load quicklisp.lisp \
        --eval '(quicklisp-quickstart:install)' \
        --eval '(let ((ql-util::*do-not-prompt* t)) (ql:add-to-init-file))'

# Install tree-sitter
WORKDIR /
RUN git clone https://github.com/tree-sitter/tree-sitter
WORKDIR /tree-sitter
RUN PREFIX=/usr make all install
WORKDIR /
# Withheld languages: agda c-sharp julia ocaml/interface ocaml/ocaml php ql ruby scala
# Special case: pin tree-sitter-python to 24b530c until https://github.com/tree-sitter/tree-sitter/issues/1654 is resolved
RUN for language in bash c cpp css go html java javascript jsdoc json python regex rust typescript/tsx typescript/typescript;do \
        [ -d tree-sitter-${language%/*} ] || git clone https://github.com/tree-sitter/tree-sitter-${language%/*};               \
        cd /tree-sitter-${language}/src;                                                                                        \
        if [ ${language} = "python" ];then                                                                                      \
            git reset --hard 24b530c;                                                                                           \
        fi;                                                                                                                     \
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
ENV LD_LIBRARY_PATH=/usr/lib:$LD_LIBRARY_PATH
RUN git clone https://github.com/death/cl-tree-sitter /root/quicklisp/local-projects/cl-tree-sitter
# Work around bug in cl-unicode in quicklisp.
RUN git clone https://github.com/edicl/cl-unicode.git /root/quicklisp/local-projects/cl-unicode

# Install pre-release version of ASDF needed for CCL package-local nicknames
RUN mkdir /root/common-lisp
RUN curl https://gitlab.common-lisp.net/asdf/asdf/-/archive/3.3.4.8/asdf-3.3.4.8.tar.gz| tar xzC /root/common-lisp

WORKDIR /root/quicklisp/local-projects
