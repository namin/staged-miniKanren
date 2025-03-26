FROM racket/racket:8.15-full

RUN raco pkg install --auto --no-docs syntax-spec-v2

WORKDIR /app
RUN cd /app

RUN mkdir staged-miniKanren
ADD . staged-miniKanren

WORKDIR /app/staged-miniKanren

CMD ["racket"]