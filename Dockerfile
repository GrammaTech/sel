FROM ubuntu:20.04

# Install required system packages
RUN export DEBIAN_FRONTEND=noninteractive
RUN ln -fs /usr/share/zoneinfo/America/New_York /etc/localtime
RUN apt-get -y --fix-missing update \
    && apt-get -y --fix-missing install autoconf build-essential texinfo python git curl wget expect \
    clang bear astyle \
    nodejs npm \
    sbcl emacs-nox elpa-paredit slime \
    pkg-config libboost-iostreams-dev libboost-system-dev libboost-serialization-dev
RUN npm install --global acorn
RUN npm install --global prettier

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
RUN curl https://llvm.org/svn/llvm-project/cfe/trunk/tools/clang-format/git-clang-format \
    |sed "s/clang-format/lisp-format/g;s/clangFormat/lispFormat/;" \
    |sed "s/default_extensions =.*\$/default_extensions = ','.join(['lisp','cl','asd','scm','el'])/;" \
    |sed "/# From clang\/lib\/Frontend\/FrontendOptions.cpp, all lower case/,/])/d" \
    > /usr/bin/git-lisp-format
RUN chmod +x /usr/bin/git-lisp-format

# Pre-download and compile a number of dependency packages.
RUN echo "Last updated 2020-05-14" \
    && git clone https://github.com/GrammaTech/cl-utils.git /root/quicklisp/local-projects/gt \
    && git clone https://github.com/GrammaTech/functional-trees.git /root/quicklisp/local-projects/functional-trees \
    && sbcl --eval "(mapcar #'ql:quickload '(:gt/full :swank :clack :elf :eclector))" \
    && ccl --eval "(mapcar #'ql:quickload '(:gt/full :swank :clack :elf :eclector))" \
    && rm -rf /root/quicklisp/local-projects/functional-trees /root/quicklisp/local-projects/gt

WORKDIR /root/quicklisp/local-projects
