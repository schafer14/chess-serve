.Phony: nats-start
nats-start:
	docker run -p 4222:4222 -d nats:latest

.Phony: nats-sub
nats-sub:
	nats-sub ">"

.Phony: etcd-start
etcd-start:
	etcd > /dev/null 2>&1 &

.Phony: elm-react
elm-react:
	cd elm && \
	elm make src/Main.elm --output elm.js && \
	cp elm.js ../static/elm.js

.Phony: elm-react-index
elm-react-index:
	cp elm/index.html static/index.html

