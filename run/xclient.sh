#!/bin/bash

stack exec xclient -- -c "run/conf/$(ls run/conf | grep -m 1 client)"
