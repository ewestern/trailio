#!/bin/bash


#READURL=
#MAPPING=

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
 
HOSTIP="10.200.10.1"
sudo ifconfig lo0 alias $HOSTIP
echo $HOSTIP

DATADIR=$(pwd)/data/
filename=$(basename $READURL)
if [ ! -f $DATADIR/$filename ]; then
  wget -nc $READURL -P $DATADIR
fi
CONTDATA=/root/data/

LOCALHOST="0.0.0.0"
#docker run -p 5432:5432 -v "$DATADIR:$CONTDATA" -e "HOSTIP=$LOCALHOST" -e "FILENAME=$filename" -e "MAPPING=$MAPPING" --net=host -t -i ewestern/osm  bin/run.sh
docker run -v "$DATADIR:$CONTDATA" -e "HOSTIP=$HOSTIP" -e "FILENAME=$filename" -e "MAPPING=$MAPPING" -t -i --net=host ewestern/osm bin/run.sh

docker ps --all | awk '{ print $1 }' | tail -n +2  | xargs docker rm
