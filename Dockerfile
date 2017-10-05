FROM docker.grammatech.com:14850/synthesis/cl

RUN apt-get -y update && \
    apt-get -y install man-db graphviz texinfo pandoc pkg-config libffi-dev

ENV PATH=/gt/sel/bin:$PATH \
    GT_DOCKER_CHOWN_PATHS=""

# Checkout the latest into the image.
# Include the hash in command so it is not cached.
RUN mkdir -p /gt/sel && \
    git clone git@git.grammatech.com:synthesis/sel.git /gt/sel && \
    cd /gt/sel && \
    git checkout CI_COMMIT_SHA && \
    make

CMD /bin/bash
