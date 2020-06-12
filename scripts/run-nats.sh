if [ ! "$(docker ps -q -f name=some-nats)" ]; then 
  if [ "$(docker ps -aq -f status=exited -f name=some-nats)" ]; then 
      docker rm some-nats 
  fi 
  docker run --rm --name some-nats -p 4222:4222 -d nats:latest 
fi
