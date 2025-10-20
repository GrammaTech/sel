ARG CCL_VERSION=1.13
ARG SBCL_VERSION=2.5.9

FROM ubuntu:22.04

# Install required system packages
RUN export DEBIAN_FRONTEND=noninteractive
RUN ln -fs /usr/share/zoneinfo/America/New_York /etc/localtime
RUN apt-get -y --fix-missing update \
    && apt-get -y --fix-missing install autoconf build-essential \
    texinfo graphviz python-is-python3 python3-pip git curl sshpass wget expect time \
    clang clang-format clang-tidy bear astyle \
    sbcl emacs-nox elpa-paredit jq \
    pkg-config libboost-iostreams-dev libboost-system-dev libboost-serialization-dev \
    locales ca-certificates libffi-dev file
ENV LC_ALL=C.UTF-8 LANG=C.UTF-8

# Install NPM
RUN curl -sL https://deb.nodesource.com/setup_20.x | bash - && apt-get install -y nodejs
RUN npm install --global prettier

# Upgrade pip and install pytest, yapf
RUN python -m pip install --upgrade pip && \
    python -m pip install pytest yapf

# Install Clozure
ARG CCL_VERSION
RUN mkdir /usr/share/ccl
RUN git clone --depth=1 --branch=v${CCL_VERSION} https://github.com/Clozure/ccl.git /usr/share/ccl
RUN curl -L https://github.com/Clozure/ccl/releases/download/v${CCL_VERSION}/linuxx86.tar.gz \
    | tar xzvf - -C /usr/share/ccl
RUN cd /usr/share/ccl && echo "(ccl:rebuild-ccl :full t)" \
    | ./lx86cl64 --no-init --quiet --batch
RUN echo '#!/bin/sh\n\
export CCL_DEFAULT_DIRECTORY=/usr/share/ccl\n\
exec ${CCL_DEFAULT_DIRECTORY}/lx86cl64 "$@"\n\
' > /usr/bin/ccl
RUN chmod a+x /usr/bin/ccl

# Install SBCL. Build SBCL ourselves to enable dynamic core so users
# can expand the memory with a command line option. Note SBCL versions
# <2.2.6 (such as the version included in the Ubuntu 22 APT
# repositories) cannot be used to bootstrap newer SBCL versions, so we
# bootstrap with Clozure CL.
ARG SBCL_VERSION
RUN rm -rf /root/sbcl && \
    git clone --depth=1 --branch sbcl-${SBCL_VERSION} https://git.code.sf.net/p/sbcl/sbcl \
    /root/sbcl
RUN cd /root/sbcl && \
    bash make.sh --xc-host='ccl --batch --no-init'\
    --prefix=/usr \
    --with-sb-linkable-runtime --with-sb-dynamic-core \
    --dynamic-space-size=8Gb
RUN apt-get -y remove sbcl
RUN cd /root/sbcl && bash install.sh && sbcl --version

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

# Install up-to-date ASDF (must be >=3.3.4.8 for package-local nickname support.)
RUN mkdir /root/common-lisp
RUN curl https://gitlab.common-lisp.net/asdf/asdf/-/archive/3.3.7/asdf-3.3.7.tar.gz| tar xzC /root/common-lisp

# Install tree-sitter and tree-sitter parsers
COPY tools/tree-sitter-install.sh /bin
# To build without pinning, pass --build-arg NOPIN=1 to docker-build.
ARG NOPIN
RUN env NOPIN=${NOPIN} WORKDIR= tree-sitter-install.sh
# Work around bug in cl-unicode in quicklisp.
RUN git clone --depth=1 https://github.com/edicl/cl-unicode.git /root/quicklisp/local-projects/cl-unicode

# Pre-download and compile a number of dependency packages.
COPY .cl-make /root/quicklisp/local-projects/sel/.cl-make
COPY Makefile /root/quicklisp/local-projects/sel/Makefile
COPY .qlfile.external /root/quicklisp/local-projects/sel/.qlfile.external
COPY .qlfile.grammatech /root/quicklisp/local-projects/sel/.qlfile.grammatech
RUN make -C /root/quicklisp/local-projects/sel dependencies libcxx-src
RUN rm -rf /root/quicklisp/local-projects/sel
RUN rm /root/quicklisp/local-projects/system-index.txt

# Setup Ancestral Git
RUN git config --system user.name "Software Evolution Library"
RUN git config --system user.email "sel@grammatech.com"

WORKDIR /root/quicklisp/local-projects

