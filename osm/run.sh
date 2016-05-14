#!/bin/bash

DDIR=$(mktemp -d)
curl $READURL > $DDIR/proto.pbf
imposm3 import  -mapping $MAPPING -read $DDIR/proto.pbf -write -optimize -connection "postgis://trailio:trailio@$HOSTIP:5432/trailio" -overwritecache
