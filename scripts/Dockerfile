# Build (from the top of the gerbil-utils checkout) with:
# docker build -t fahree/gerbil-utils -f scripts/Dockerfile .
# Push with:
# docker push fahree/gerbil-utils:latest
# Pull with:
# docker pull fahree/gerbil-utils:latest

FROM fahree/gerbil-nix:latest
MAINTAINER fahree@gmail.com

#RUN cd /gerbil ; git clone https://github.com/fare/gerbil-utils.git
COPY . /gerbil/gerbil-utils

RUN \
  chown -R gerbil.gerbil /gerbil && \
  chmod -R u+w /gerbil
USER gerbil
RUN \
  echo "#" > /gerbil/.zshrc && \
  cd /gerbil/gerbil-utils && \
  ./build.ss
