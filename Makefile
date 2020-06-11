.Phony: nats-start
nats-start:
	docker run -p 4222:4222 -d nats:latest

.Phony: nats-sub
nats-sub:
	nats-sub ">"

.Phony: etcd-start
etcd-start:
	etcd > /dev/null 2>&1 &
