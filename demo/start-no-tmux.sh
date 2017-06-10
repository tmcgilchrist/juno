#!/bin/sh

stack exec xserver -- -c conf/10000-cluster.yaml --apiPort 8000 &
stack exec xserver -- -c conf/10001-cluster.yaml --apiPort 8001 &
stack exec xserver -- -c conf/10002-cluster.yaml --apiPort 8002 &
stack exec xserver -- -c conf/10003-cluster.yaml --apiPort 8003
