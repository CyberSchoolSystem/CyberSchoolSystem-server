FROM fpco/stack-build:lts-10.4 as builder
WORKDIR /app
ADD . /app
RUN stack install
CMD ["stack", "exec", "server"]

# FROM fpco/stack-build:lts-10.4
# WORKDIR /app
# COPY --from=builder /root/.local/bin/server /app/
# COPY --from=builder /app/config /app/conifg
# COPY --from=builder /app/static /app/static
# RUN apt update
# RUN apt install nginx
# CMD ["./server"]
