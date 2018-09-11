#!/bin/sh

if [ -z ${1+x} ]; then
	echo "Private RSA key store needs to be specified as first parameter!";
	exit
fi 

echo "RELEASING KOLHYDRATRÄKNAREN"

./build.sh

cordova clean
cordova build --release

cp platforms/android/app/build/outputs/apk/release/app-release-unsigned.apk platforms/android/app/build/outputs/apk/release/app-release-signed.apk

jarsigner -verbose -sigalg SHA1withRSA -digestalg SHA1 -keystore $1 platforms/android/app/build/outputs/apk/release/app-release-signed.apk android-app-key -tsa http://sha256timestamp.ws.symantec.com/sha256/timestamp

zipalign -v 4 platforms/android/app/build/outputs/apk/release/app-release-signed.apk platforms/android/app/build/outputs/apk/release/app-release.apk

