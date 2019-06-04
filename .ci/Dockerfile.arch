# FROM cl-arch as cl
# FROM java-mutator-arch as java-mutator
# FROM clang-mutate-arch
FROM docker.grammatech.com/synthesis/cl:arch as cl
FROM docker.grammatech.com/synthesis/java-mutator:arch as java-mutator
FROM docker.grammatech.com/synthesis/clang-mutate:arch

# Some of the SEL tests require multilib support for m32 tests.
# Also, our functionality for looking up the library providing
# standard functions requires that the system has man pages installed.
# Also, python is required for testbot to submit results to the datamanager.
RUN sed -i 's/#\[multilib\]/\[multilib\]/; /^\[multilib\]/,/^$/ s/^#//' /etc/pacman.conf
RUN pacman --noconfirm -Syu archlinux-keyring && \
    pacman --noconfirm -Syu astyle boost boost-libs diffutils expect findutils gawk gcc-multilib graphviz grep jdk8-openjdk less lib32-fakeroot lib32-gcc-libs lib32-glibc lib32-libltdl libffi m4 make man-db man-pages maven nodejs npm opam pandoc patch python sed subversion tar texinfo unzip

# Install ocaml 4.06.0 via opam (for SerAPI)
RUN opam init --disable-sandboxing -j 4 --compiler="ocaml-base-compiler.4.06.0" -y

# Configure ocaml, install coq-serapi at fixed version
RUN eval `opam config env` && \
    opam install -y coq-serapi=8.7.2+0.4.13

COPY --from=cl /usr/synth/ /usr/synth
COPY --from=java-mutator /usr/synth/bin/java-mutator /usr/synth/bin/java-mutator

# Javascript
RUN npm install --global acorn && \
    npm install --global prettier

ARG GT
ARG LISP
# Use /root/quicklisp/local-projects.
RUN cp -R /usr/synth/quicklisp /root/quicklisp
COPY . /root/quicklisp/local-projects/sel
WORKDIR /root/quicklisp/local-projects/sel
RUN export __TEMP_LD_LIBRARY_PATH=$(if [ -z "$LD_LIBRARY_PATH" ]; then echo "" ;else echo ":$LD_LIBRARY_PATH";fi)
ENV LISP=$LISP \
    PATH=/root/quicklisp/local-projects/sel/bin:/root/.opam/ocaml.4.06.0/bin:/usr/synth/bin:$PATH \
    LD_LIBRARY_PATH=/root/quicklisp/dists/trace-db/software/trace-db/:/usr/synth/lib/$__TEMP_LD_LIBRARY_PATH \
    QUICK_LISP=/root/quicklisp/ \
    SBCL_HOME=/usr/synth/lib/sbcl \
    LISP_STACK=32678
RUN make real-clean all
CMD /bin/bash
