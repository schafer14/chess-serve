.Phony: nats-start
nats-start:
	./scripts/run-nats.sh

.Phony: nats-sub
nats-sub:
	nats-sub ">"

.Phony: elm-react
elm-react:
	cd elm && \
	elm make src/Main.elm --output elm.js && \
	cp elm.js ../static/elm.js

.Phony: elm-react-index
elm-react-index:
	cp elm/index.html static/index.html

.Phony: build
build:
	cd elm && \
	elm make src/Main.elm --output elm.js --optimize && \
	cp elm.js ../static/elm.js
	cp elm/index.html static/index.html
