FROM f4814n/css-intermediate:latest as builder
WORKDIR /app
ADD . /app
RUN stack install

FROM ubuntu:16.04
WORKDIR /app
COPY --from=builder /root/.local/bin/server /app/
COPY --from=builder /app/config /app/config
COPY --from=builder /app/static /app/static
RUN apt update
RUN apt install -y libgmp-dev ca-certificates netbase
EXPOSE 3000
CMD ["./server"]
