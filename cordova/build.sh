#!/bin/sh

echo "BUILDING KOLHYDRATRÄKNAREN"

cd .. && elm-app build && cd cordova && cordova build android
