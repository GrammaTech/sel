FROM docker.grammatech.com:14850/synthesis/cl:arch-linux

ENV PATH=/gt/sel/bin:$PATH

# Some of the SEL tests require multilib support for m32 tests.
# Also, our functionality for looking up the library providing
# standard functions requires that the system has man pages installed.
# Also, python is required for testbot to submit results to the datamanager.
RUN sed -i 's/#\[multilib\]/\[multilib\]/; /^\[multilib\]/,/^$/ s/^#//' /etc/pacman.conf
RUN yes|pacman -Syu --confirm gcc-multilib man-db man-pages python

# Checkout the latest into the image.
# Include the hash in command so it is not cached.
RUN mkdir -p /gt/sel && \
    git clone git@git.grammatech.com:synthesis/sel.git /gt/sel && \
    cd /gt/sel && \
    git checkout CI_COMMIT_SHA && \
    make

WORKDIR /gt/sel

CMD /bin/bash
