FROM erlang:22.3.2-alpine AS builder
COPY . /root/binance
WORKDIR /root/binance
RUN apk add git bash
RUN rebar3 do compile, release

FROM alpine:latest
WORKDIR /root
RUN apk add ncurses
RUN mkdir ssl
COPY --from=builder /root/binance/priv/ssl/ca_certificate.pem ./ssl
COPY --from=builder /root/binance/priv/ssl/client_certificate.pem ./ssl
COPY --from=builder /root/binance/priv/ssl/client_key.pem ./ssl
COPY --from=builder /root/binance/_build/default/rel/binance .
CMD ./bin/binance console
