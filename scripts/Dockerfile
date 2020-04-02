# Build (from the top of the gerbil-utils checkout) with:
# docker build -t fahree/gerbil-utils -f scripts/Dockerfile .
# Push with:
# docker push fahree/gerbil-utils:latest
# Pull with:
# docker pull fahree/gerbil-utils:latest

FROM gerbil/scheme:latest
MAINTAINER fahree@gmail.com

#RUN git clone https://github.com/fare/gerbil-utils.git && \
#    cd gerbil-utils && \
#    ./build.ss

COPY . /root/gerbil-utils
RUN cd /root/gerbil-utils && ./build.ss
