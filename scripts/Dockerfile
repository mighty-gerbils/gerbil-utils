# Build with:
# docker build -t fahree/gerbil-utils .
# Push with:
# docker push fahree/gerbil-utils:latest
# Pull with:
# docker pull fahree/gerbil-utils:latest
FROM gerbil/scheme:latest
MAINTAINER fahree@gmail.com
RUN cd /root && \
    git clone https://github.com/fare/gerbil-utils.git && \
    cd /root/gerbil-utils && \
    ./build.ss
