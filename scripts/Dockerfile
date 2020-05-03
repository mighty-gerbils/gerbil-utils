# Build (from the top of the gerbil-utils checkout) with:
# docker build -t fahree/gerbil-utils -f scripts/Dockerfile .
# Push with:
# docker push fahree/gerbil-utils:latest
# Pull with:
# docker pull fahree/gerbil-utils:latest

FROM fahree/gerbil-nix:latest
MAINTAINER fahree@gmail.com

USER root
RUN \
  chown -R gerbil.gerbil /gerbil && \
  chmod -R u+w /gerbil

#RUN cd /gerbil ; git clone --branch $TREE https://github.com/fare/gerbil-utils.git
COPY \
  --chown=gerbil:gerbil \
  . /gerbil/gerbil-utils

USER gerbil
RUN \
  cd /gerbil/gerbil-utils && \
  ./build.ss
