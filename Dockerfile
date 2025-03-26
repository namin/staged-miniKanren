FROM debian:bullseye-slim

# Install dependencies
RUN apt-get update && apt-get install -y \
    curl \
    libffi-dev \
    libncurses-dev \
    libjpeg-dev \
    libx11-dev \
    libgl1-mesa-dev \
    libglu1-mesa-dev \
    x11-xserver-utils \
    && rm -rf /var/lib/apt/lists/*

RUN curl -fL -o racket-8.15.sh \
    https://mirror.racket-lang.org/installers/8.15/racket-8.15-x86_64-linux.sh && \
    chmod +x racket-8.15.sh && \
    ./racket-8.15.sh --in-place --dest /usr/racket && \
    rm racket-8.15.sh

ENV PATH="/usr/racket/bin:${PATH}"

RUN raco pkg install --auto syntax-spec-v2

WORKDIR /app
RUN cd /app

RUN mkdir staged-miniKanren
ADD . staged-miniKanren

WORKDIR /app/staged-miniKanren

CMD ["racket"]