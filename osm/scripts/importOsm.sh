#! /usr/bin/env bash
set -eu

$GOPATH/bin/imposm3 import -mapping $1 -read "$2" -write -optimize -connection postgis://trailio:trailio@localhost:5432/trailio  -overwritecache
