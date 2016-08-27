#!/bin/bash


READURL=
MAPPING=

while getopts "r:m:" opt; do
  case $opt in
    r)
      READURL=$OPTARG
      ;;
    m)
      MAPPING=$OPTARG
      ;;
    \?) 
      echo "Invalid Option"
      exit 1
      ;;
  esac
done



set -u
HOSTIP=$(ipconfig getifaddr en0)
#HOSTIP=$(docker-machine ip)

docker run -p 5432:5432 -e "HOSTIP=$HOSTIP" -e "READURL=$READURL" -e "MAPPING=$MAPPING" --net=host -t -i ewestern/osm
