# Build (from the top of the gerbil-utils checkout) with:
# docker build -t fahree/gerbil-utils -f scripts/Dockerfile .
# Push with:
# docker push fahree/gerbil-utils:latest
# Pull with:
# docker pull fahree/gerbil-utils:latest

FROM fahree/gerbil-nix
MAINTAINER fahree@gmail.com

USER root
RUN \
  chown -R user.user /home && \
  chmod -R u+w /home && \
  chmod 4755 /tmp && \
  cachix use mukn

USER user
WORKDIR /home
ENV GAMBOPT i8,t8

# gerbil-utils is now included in the nix-built image already!
##RUN cd /gerbil ; git clone --branch $TREE https://github.com/fare/gerbil-utils.git
#COPY --chown=gerbil:gerbil . /gerbil/gerbil-utils
#RUN cd /gerbil/gerbil-utils && ./build.ss
