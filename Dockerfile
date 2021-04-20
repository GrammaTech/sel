FROM ubuntu:20.04

# Install required system packages
RUN export DEBIAN_FRONTEND=noninteractive
RUN ln -fs /usr/share/zoneinfo/America/New_York /etc/localtime
RUN apt-get -y --fix-missing update \
    && apt-get -y --fix-missing install autoconf build-essential \
    texinfo graphviz python-is-python3 python3-pip python3-pytest git curl sshpass wget expect time \
    clang clang-format clang-tidy bear astyle \
    sbcl emacs-nox elpa-paredit slime jq \
    pkg-config libboost-iostreams-dev libboost-system-dev libboost-serialization-dev
RUN update-alternatives --install /usr/bin/pytest pytest /usr/bin/pytest-3 1
# Install NPM
RUN curl -sL https://deb.nodesource.com/setup_14.x | bash - && apt-get install -y nodejs
RUN npm install --global acorn
RUN npm install --global prettier
RUN pip3 install yapf


# # Install Clozure
RUN mkdir /usr/share/ccl
RUN git clone --branch=v1.12 https://github.com/Clozure/ccl.git /usr/share/ccl
RUN curl -L https://github.com/Clozure/ccl/releases/download/v1.12/linuxx86.tar.gz \
    | tar xzvf - -C /usr/share/ccl
RUN cd /usr/share/ccl && echo "(ccl:rebuild-ccl :full t)" \
    | ./lx86cl64 --no-init --quiet --batch
RUN echo '#!/bin/sh\n\
export CCL_DEFAULT_DIRECTORY=/usr/share/ccl\n\
exec ${CCL_DEFAULT_DIRECTORY}/lx86cl64 "$@"\n\
' > /usr/bin/ccl
RUN chmod a+x /usr/bin/ccl

# Install QuickLisp
RUN curl -O https://beta.quicklisp.org/quicklisp.lisp
RUN sbcl --load quicklisp.lisp \
        --eval '(quicklisp-quickstart:install)' \
        --eval '(let ((ql-util::*do-not-prompt* t)) (ql:add-to-init-file))'
RUN ccl --load /root/quicklisp/setup.lisp \
        --eval '(let ((ql-util::*do-not-prompt* t)) (ql:add-to-init-file))'

# Install the lisp-format pre-commit format checker.
RUN curl https://raw.githubusercontent.com/eschulte/lisp-format/master/lisp-format \
    > /usr/bin/lisp-format
RUN chmod +x /usr/bin/lisp-format
RUN echo "(add-to-list 'load-path \"/usr/share/emacs/site-lisp/\")" > /root/.lisp-formatrc
RUN curl https://raw.githubusercontent.com/llvm-mirror/clang/master/tools/clang-format/git-clang-format \
    |sed "s/clang-format/lisp-format/g;s/clangFormat/lispFormat/;" \
    |sed "s/default_extensions =.*\$/default_extensions = ','.join(['lisp','cl','asd','scm','el'])/;" \
    |sed "/# From clang\/lib\/Frontend\/FrontendOptions.cpp, all lower case/,/])/d" \
    > /usr/bin/git-lisp-format
RUN chmod +x /usr/bin/git-lisp-format

# Install pre-release version of ASDF needed for CCL package-local nicknames
RUN mkdir /root/common-lisp
RUN curl https://gitlab.common-lisp.net/asdf/asdf/-/archive/3.3.4.8/asdf-3.3.4.8.tar.gz| tar xzC /root/common-lisp

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
            clang++ -fPIC scanner.cc -c -lstdc++;                                                                               \
            clang -std=c99 -fPIC parser.c -c;                                                                                   \
            clang++ -shared scanner.o parser.o -o /usr/lib/tree-sitter-$(echo ${language}|sed 's|/|-|').so;                     \
        elif test -f "scanner.c"; then                                                                                          \
            clang -std=c99 -fPIC scanner.c -c;                                                                                  \
            clang -std=c99 -fPIC parser.c -c;                                                                                   \
            clang -shared scanner.o parser.o -o /usr/lib/tree-sitter-$(echo ${language}|sed 's|/|-|').so;                       \
        else                                                                                                                    \
            clang -std=c99 -fPIC parser.c -c;                                                                                   \
            clang -shared parser.o -o /usr/lib/tree-sitter-$(echo ${language}|sed 's|/|-|').so;                                 \
        fi;                                                                                                                     \
        mkdir -p /usr/share/tree-sitter/${language}/;                                                                           \
        cp grammar.json node-types.json /usr/share/tree-sitter/${language};                                                     \
        cd -;                                                                                                                   \
    done
RUN git clone https://github.com/death/cl-tree-sitter /root/quicklisp/local-projects/cl-tree-sitter
# Work around bug in cl-unicode in quicklisp.
RUN git clone https://github.com/edicl/cl-unicode.git /root/quicklisp/local-projects/cl-unicode

# Pre-download and compile a number of dependency packages.
COPY .cl-make /root/quicklisp/local-projects/sel/.cl-make
COPY Makefile /root/quicklisp/local-projects/sel/Makefile
COPY .qlfile.external /root/quicklisp/local-projects/sel/.qlfile.external
COPY .qlfile.grammatech /root/quicklisp/local-projects/sel/.qlfile.grammatech
RUN make -C /root/quicklisp/local-projects/sel dependencies
RUN rm -rf /root/quicklisp/local-projects/sel

WORKDIR /root/quicklisp/local-projects

