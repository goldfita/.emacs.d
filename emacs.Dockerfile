# docker build --build-arg USER=$USER --build-arg VERSION=30.1 -f emacs.Dockerfile -t emacs .
# https://www.masteringemacs.org/article/speed-up-emacs-libjansson-native-elisp-compilation


ARG ICONS_DIR=all-the-icons


################################## Build environment ##################################
FROM ubuntu:22.04 AS build

ARG VERSION
ARG ICONS_DIR
ARG EMACS_BRANCH="emacs-${VERSION}"
WORKDIR /opt

ENV DEBIAN_FRONTEND=noninteractive
RUN apt update && \
    apt install -y git libjansson4 libjansson-dev apt-transport-https ca-certificates \
        curl gnupg-agent software-properties-common libtree-sitter-dev && \
    add-apt-repository ppa:ubuntu-toolchain-r/ppa && \
    apt install -y gcc-10 libgccjit0 libgccjit-10-dev

RUN git clone --depth 1 --branch $EMACS_BRANCH https://git.savannah.gnu.org/git/emacs.git . && \
    git clone --depth 1 https://github.com/domtronn/all-the-icons.el $ICONS_DIR

RUN sed -i 's/# deb-src/deb-src/' /etc/apt/sources.list && apt update && apt-get build-dep -y emacs

ENV CC="gcc-10"
RUN git config --global --add safe.directory /opt && \
    ./autogen.sh && \
    ./configure --with-native-compilation=aot --with-tree-sitter && \
    make -j 2 && \
    make install prefix=/opt/build


################################## Run environment ##################################
FROM ubuntu:22.04

ARG VERSION
ARG USER
ARG ICONS_DIR
ARG HOST_MOUNT=/host
ARG INST_PATH=/usr/local/share
WORKDIR /opt
COPY --from=build /opt/build /usr/local
COPY --from=build "/opt/${ICONS_DIR}/fonts" "${INST_PATH}/fonts/${ICONS_DIR}"

# Silence dbind-WARNING
ENV NO_AT_BRIDGE=1
# Set emacs home dir
ENV HOME=/home/share
# Add home to path
ENV PATH="/home/${USER}:${PATH}"
ENV EMACSLOADPATH=\
"${INST_PATH}/emacs/${VERSION}/lisp/":\
"${INST_PATH}/emacs/${VERSION}/lisp/emacs-lisp/":\
"${HOST_MOUNT}/home/${USER}/.emacs.d/"

# So apt doesn't ask for user input
ENV DEBIAN_FRONTEND=noninteractive
RUN apt update && \
    apt install -y libtree-sitter0 libjpeg62 libtiff5 libpng16-16 libgif7 libgtk-3-0 libsm6 \
        libasound2 libgpm2 libotf1 libm17n-0 libjansson4 libgccjit0 dbus-x11 sudo openssh-client
        
# Set up user, configure environment (this needs to match your group on the host)
#gsettings set org.gnome.desktop.wm.keybindings activate-window-menu "[]"
RUN adduser --gecos "" --uid 1000 --disabled-password $USER && \
    echo "${USER} ALL=(ALL:ALL) NOPASSWD: ALL" > "/etc/sudoers.d/${USER}" && \
    echo "    IdentityFile ${HOST_MOUNT}/home/${USER}/.ssh/id_rsa" >> /etc/ssh/ssh_config && \
    echo "    UserKnownHostsFile ${HOST_MOUNT}/home/${USER}/.ssh/known_hosts" >> /etc/ssh/ssh_config

# Install dependencies and other nice-to-haves
RUN apt install -o DPkg::Options::="--force-confnew" -y python3 \
        ispell fonts-hack-ttf ripgrep markdown tree && \
    fc-cache -f
