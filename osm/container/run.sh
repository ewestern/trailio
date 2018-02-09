#!/bin/bash

$GOPATH/bin/imposm3 import  -mapping $MAPPING -read /root/data/$FILENAME -write -optimize -connection "postgis://trailio:trailio@$HOSTIP:5432/trailio" -overwritecache

