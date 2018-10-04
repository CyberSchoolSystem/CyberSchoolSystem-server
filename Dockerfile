FROM f4814n/css-intermediate:latest as builder
WORKDIR /app
ADD . /app
RUN stack install

FROM nginx:latest
WORKDIR /app
COPY --from=builder /root/.local/bin/server /app/
COPY --from=builder /app/scripts /app/scripts
COPY --from=builder /app/config /app/config
COPY --from=builder /app/static /app/static
RUN apt update
RUN apt install -y libgmp-dev ca-certificates netbase  openssl
EXPOSE 80
EXPOSE 443
CMD ["./scripts/startReverseProxy.sh"]
