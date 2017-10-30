FROM docker.grammatech.com:14850/synthesis/cl:arch-linux

RUN pacman --noconfirm -Syu

# This is installed in a previous layer but must be removed to install
# some of the packages in the following line.
RUN pacman --noconfirm -R libtinfo
RUN pacman --noconfirm -Syu graphviz texinfo pandoc libffi

ENV PATH=/gt/sel/bin:$PATH

# Checkout the latest into the image.
# Include the hash in command so it is not cached.
RUN mkdir -p /gt/sel && \
    git clone git@git.grammatech.com:synthesis/sel.git /gt/sel && \
    cd /gt/sel && \
    git checkout CI_COMMIT_SHA && \
    make

WORKDIR /gt/sel

CMD /bin/bash
