FROM ubuntu:latest

ENV DEBIAN_FRONTEND noninteractive

RUN apt update -y && \
    apt install -y --no-install-recommends \
    bc bison build-essential bzip2 ca-certificates ccache clang cmake \
    cpio curl cvs diffutils ecj fastjar file findutils flex gawk \
    gcc-multilib gettext git gkermit grep gzip java-propose-classpath \
    libelf-dev libncurses5-dev libssl-dev locales lrzsz mercurial \
    python2 python2-dev python3 python3-dev rsync screen sed subversion \
    swig tar time tmux unzip util-linux wget xsltproc xz-utils zlib1g-dev \
    nvi cscope openssh-client dropbear-bin sparse indent\
    && \
    apt autoremove -y && \
    apt clean -y

