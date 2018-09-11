#!/bin/sh

echo "DEPLOYING KOLHYDRATRÄKNAREN TO PHONE"

./build.sh && cordova run --device
