FROM quay.io/pypa/manylinux_2_28_x86_64

# Install required system packages
RUN yum --setopt=skip_missing_names_on_install=False install -y \
    autoconf @development git clang curl wget libffi-devel zlib-devel


# Install SBCL; not in EPEL.
RUN curl -LO http://prdownloads.sourceforge.net/sbcl/sbcl-2.2.5-x86-64-linux-binary.tar.bz2 \
    && tar -xaf sbcl-2.2.5-x86-64-linux-binary.tar.bz2 \
    && cd sbcl-2.2.5-x86-64-linux \
    && sh install.sh \
    && cd .. \
    && rm -rf sbcl-2.2.5-x76-64-linux
# Rebuild SBCL from scratch from a tag, enabling dynamic core so users
# can expand the memory with a command line option
RUN git clone --branch sbcl-2.2.5 https://git.code.sf.net/p/sbcl/sbcl /root/sbcl
RUN cd /root/sbcl && bash make.sh --prefix=/usr --with-sb-linkable-runtime --with-sb-dynamic-core --dynamic-space-size=8Gb
RUN cd /root/sbcl && bash install.sh

# Install QuickLisp
RUN curl -O https://beta.quicklisp.org/quicklisp.lisp
RUN sbcl --load quicklisp.lisp \
        --eval '(quicklisp-quickstart:install)' \
        --eval '(let ((ql-util::*do-not-prompt* t)) (ql:add-to-init-file))'

# Install pre-release version of ASDF needed for CCL package-local nicknames
RUN mkdir /root/common-lisp
RUN curl https://gitlab.common-lisp.net/asdf/asdf/-/archive/3.3.4.8/asdf-3.3.4.8.tar.gz| tar xzC /root/common-lisp

# Install tree-sitter and tree-sitter parsers
COPY tools/tree-sitter-install.sh /bin
# To build without pinning, pass --build-arg NOPIN=1 to docker-build.
ARG NOPIN
RUN env NOPIN=${NOPIN} WORKDIR= tree-sitter-install.sh
ENV LD_LIBRARY_PATH=/usr/lib:$LD_LIBRARY_PATH

# Work around bug in cl-unicode in quicklisp.
RUN git clone https://github.com/edicl/cl-unicode.git /root/quicklisp/local-projects/cl-unicode

WORKDIR /root/quicklisp/local-projects
