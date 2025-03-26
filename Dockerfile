FROM ubuntu:latest

RUN apt update -y
RUN apt-get install -y software-properties-common
RUN add-apt-repository ppa:plt/racket
RUN apt-get install -y racket

RUN raco pkg install --auto syntax-spec-v2

WORKDIR /app
RUN cd /app

RUN mkdir staged-miniKanren
ADD . staged-miniKanren

WORKDIR /app/staged-miniKanren

CMD ["racket"]