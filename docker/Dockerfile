FROM ubuntu

ENV PATH $PATH:/root/.local/bin
RUN set -eux ; \
    : "This image is compatible local development with latest ubuntu lts." ; \
    apt-get update && \
    apt-get install -y \
      wget \
      mysql-client \
      default-libmysqlclient-dev \
      pkg-config \
      libpcre3 \
      libpcre3-dev \
      libtinfo-dev &&\
    wget -qO- https://get.haskellstack.org/ | sh
