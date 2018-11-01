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
RUN apt install -y libgmp-dev ca-certificates netbase  openssl git
EXPOSE 80
EXPOSE 443

RUN git clone https://github.com/jgm/pandoc.git data
RUN mkdir /root/.stack/snapshots/x86_64-linux/lts-10.4/8.2.2/share/x86_64-linux-ghc-8.2.2/pandoc-2.0.6/data -p
RUN mv data/data/docx /root/.stack/snapshots/x86_64-linux/lts-10.4/8.2.2/share/x86_64-linux-ghc-8.2.2/pandoc-2.0.6/data
RUN rm -R data
CMD ["./scripts/startReverseProxy.sh"]
