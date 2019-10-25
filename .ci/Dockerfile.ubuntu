FROM ubuntu:16.04 as coq-serapi
RUN apt-get -y update && \
    apt-get -y install make m4 opam
# Install ocaml 4.06.0 via opam (for SerAPI)
RUN opam init -j 4 --compiler="4.06.0" -y
# Configure ocaml, install coq-serapi at fixed version
RUN eval `opam config env` && \
    opam install coq-serapi=8.7.2+0.4.13


# FROM cl-ubuntu as cl
# FROM java-mutator-ubuntu as java-mutator
# FROM clang-mutate-ubuntu as clang-mutate
FROM docker.grammatech.com/synthesis/cl as cl
FROM docker.grammatech.com/synthesis/java-mutator as java-mutator
FROM docker.grammatech.com/synthesis/clang-mutate as clang-mutate
FROM ubuntu:16.04

RUN apt-get -y update && \
    apt-get -y install g++ gcc gcc-multilib graphviz libffi-dev man-db maven opam openjdk-8-jdk pandoc pkg-config texinfo unzip expect wget curl libboost-all-dev

COPY --from=coq-serapi /root/.opam /root/.opam
COPY --from=clang-mutate /usr/synth/ /usr/synth/
COPY --from=cl /usr/synth/quicklisp /usr/synth/quicklisp
COPY --from=cl /usr/synth/lib/ccl /usr/synth/lib/ccl
COPY --from=cl /usr/synth/lib/sbcl /usr/synth/lib/sbcl
COPY --from=cl /usr/synth/bin/ccl /usr/synth/bin/ccl
COPY --from=cl /usr/synth/bin/sbcl /usr/synth/bin/sbcl
COPY --from=cl /usr/synth/share/ccl /usr/synth/share/ccl
COPY --from=cl /usr/synth/share/doc/ccl /usr/synth/share/doc/ccl
COPY --from=cl /usr/synth/share/doc/sbcl /usr/synth/share/doc/sbcl
COPY --from=java-mutator /usr/synth/bin/java-mutator /usr/synth/bin/java-mutator

# Artistic Style Version 3.0
# Build astyle from source as the package manager only has version 2.0.
RUN wget https://downloads.sourceforge.net/project/astyle/astyle/astyle%203.1/astyle_3.1_linux.tar.gz && \
    tar xf astyle_3.1_linux.tar.gz && \
    cd astyle/build/gcc && \
    make release && \
    cp bin/astyle /usr/bin && \
    cd ../../../ && \
    rm -rf astyle && \
    rm -rf astyle_3.1_linux.tar.gz

# Javascript
RUN curl -sL https://deb.nodesource.com/setup_8.x | bash - && \
    apt-get -y update && \
    apt-get -y install nodejs && \
    npm install --global acorn && \
    npm install --global prettier

ARG GT
ARG LISP
# Use /root/quicklisp/local-projects.
RUN cp -R /usr/synth/quicklisp /root/quicklisp
COPY . /root/quicklisp/local-projects/sel
WORKDIR /root/quicklisp/local-projects/sel
RUN export __TEMP_LD_LIBRARY_PATH=$(if [ -z "$LD_LIBRARY_PATH" ]; then echo "" ;else echo ":$LD_LIBRARY_PATH";fi)
ENV LISP=$LISP \
    PATH=/root/quicklisp/local-projects/sel/bin:/root/.opam/4.06.0/bin:/usr/synth/bin:$PATH \
    LD_LIBRARY_PATH=/root/quicklisp/dists/trace-db/software/trace-db/:/usr/synth/lib/$__TEMP_LD_LIBRARY_PATH \
    SERAPI=/root/.opam/4.06.0/bin/sertop \
    COQLIB=/root/.opam/4.06.0/lib/coq/ \
    QUICK_LISP=/root/quicklisp/ \
    SBCL_HOME=/usr/synth/lib/sbcl \
    LISP_STACK=32678
RUN make real-clean all
CMD /bin/bash
