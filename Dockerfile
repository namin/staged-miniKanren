FROM racket/racket:8.15-full

# for benchmark collation
RUN apt-get update
RUN apt-get install -y python3 xvfb

RUN raco pkg install --auto --no-docs syntax-spec-v2

WORKDIR /app
RUN cd /app

RUN mkdir staged-miniKanren
ADD . staged-miniKanren

WORKDIR /app/staged-miniKanren

# warm-up
RUN racket all.rkt

CMD ["racket"]
