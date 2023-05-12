FROM debian:bullseye-slim

RUN apt-get update
RUN apt-get install -y \
  curl \
  g++ \
  gcc \
  git \
  gnupg \
  libc6-dev \
  libffi-dev \
  libgmp-dev \
  libtinfo-dev \
  libz3-dev \
  make \
  netbase \
  xz-utils \
  z3 \
  zlib1g-dev

RUN curl -sSL https://get.haskellstack.org/ | sh

ENV PATH /root/.local/bin:$PATH

WORKDIR /test-task
COPY stack.yaml package.yaml Setup.hs ./
RUN touch README.md
RUN mkdir -p app/pkg-path
RUN stack build --only-dependencies

COPY app app
RUN stack install autotool-task-test:test-task

RUN stack run pkg-path-from-stack

WORKDIR /

ENTRYPOINT ["test-task", "task.hs", "solution.hs"]
