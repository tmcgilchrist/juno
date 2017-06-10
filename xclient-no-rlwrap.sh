#!/bin/bash

stack exec xclient -- -c "conf/$(ls conf | grep -m 1 client)"
