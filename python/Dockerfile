FROM quay.io/pypa/manylinux_2_24_x86_64

# Install required system packages
RUN export DEBIAN_FRONTEND=noninteractive
RUN ln -fs /usr/share/zoneinfo/America/New_York /etc/localtime
RUN apt-get -y --fix-missing update \
    && apt-get -y --fix-missing install autoconf build-essential \
    texinfo graphviz python3 python3-pip python3-setuptools python3-venv \
    libffi-dev software-properties-common git curl sshpass wget expect time \
    clang clang-format clang-tidy bear astyle \
    sbcl emacs-nox elpa-paredit slime jq \
    pkg-config libboost-iostreams-dev libboost-system-dev libboost-serialization-dev
RUN update-alternatives --install /usr/bin/python python /usr/bin/python3.5 1
RUN curl -sL https://deb.nodesource.com/setup_14.x | bash - && apt-get install -y nodejs
RUN npm install --global acorn
RUN npm install --global prettier
RUN pip3 install yapf
RUN pip3 install cffi
RUN export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:/usr/local/lib
RUN apt-get -y remove cl-asdf

# Newer SBCL than available in the debian image
RUN git clone --branch=sbcl-2.1.2 https://github.com/sbcl/sbcl.git /usr/share/sbcl
RUN cd /usr/share/sbcl && sh make.sh
RUN cd /usr/share/sbcl && sh isntall.sh

# Newer ASDF for SBCL to support package-local nicknames
RUN curl https://gitlab.common-lisp.net/asdf/asdf/-/archive/3.3.4.8/asdf-3.3.4.8.tar.gz| tar xzC /usr/share
RUN make -C /usr/share/asdf-3.3.4.8/
# https://common-lisp.net/project/asdf/asdf.html#Loading-ASDF-from-source
RUN echo '(load "/usr/share/asdf-3.3.4.8/build/asdf.lisp")' >> /root/.sbclrc

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
RUN for language in agda bash c c-sharp cpp css go html java javascript jsdoc json julia ocaml/ocaml ocaml/interface php python ql regex ruby rust scala typescript/tsx typescript/typescript;do \
        [ -d tree-sitter-${language%/*} ] || git clone --depth=1 https://github.com/tree-sitter/tree-sitter-${language%/*};                                                                      \
        cd /tree-sitter-${language}/src;                                                                                                                                                         \
        if test -f "scanner.cc"; then                                                                                                                                                            \
            clang++ -fPIC scanner.cc -c -lstdc++;                                                                                                                                                \
            clang -std=c99 -fPIC parser.c -c;                                                                                                                                                    \
            clang++ -shared scanner.o parser.o -o /usr/lib/tree-sitter-$(echo ${language}|sed 's|/|-|').so;                                                                                      \
        elif test -f "scanner.c"; then                                                                                                                                                           \
            clang -std=c99 -fPIC scanner.c -c;                                                                                                                                                   \
            clang -std=c99 -fPIC parser.c -c;                                                                                                                                                    \
            clang -shared scanner.o parser.o -o /usr/lib/tree-sitter-$(echo ${language}|sed 's|/|-|').so;                                                                                        \
        else                                                                                                                                                                                     \
            clang -std=c99 -fPIC parser.c -c;                                                                                                                                                    \
            clang -shared parser.o -o /usr/lib/tree-sitter-$(echo ${language}|sed 's|/|-|').so;                                                                                                  \
        fi;                                                                                                                                                                                      \
        mkdir -p /usr/share/tree-sitter/${language}/;                                                                                                                                            \
        cp grammar.json node-types.json /usr/share/tree-sitter/${language};                                                                                                                      \
        cd -;                                                                                                                                                                                    \
    done

# Pre-download and compile a number of dependency packages.
COPY . /root/quicklisp/local-projects/sel
RUN make -C /root/quicklisp/local-projects/sel dependencies

WORKDIR /root/quicklisp/local-projects/
