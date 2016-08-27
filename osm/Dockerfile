FROM golang:latest 
MAINTAINER <pfrance@gmail.com>

RUN apt-get update && apt-get install -y \
      libprotobuf-dev libleveldb-dev libgeos-dev \
      --no-install-recommends

RUN ln -s /usr/lib/libgeos_c.so /usr/lib/libgeos.so

WORKDIR $GOPATH

RUN go get github.com/omniscale/imposm3
RUN go install github.com/omniscale/imposm3

COPY *mapping* $GOPATH/
COPY *run.sh* $GOPATH/bin/

EXPOSE 5432

ENTRYPOINT ["run.sh"]
