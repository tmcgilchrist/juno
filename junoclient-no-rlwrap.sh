#!/bin/bash

stack exec junoclient -- -c "conf/$(ls conf | grep -m 1 client)"
