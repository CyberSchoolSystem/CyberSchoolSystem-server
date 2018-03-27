#!/bin/sh

startServer() {
    stack exec server &
}

stopServer() {
    kill %stack
}

insertDummyVotes() {
    for i in `seq 1 $1`; do
        curl --data " { \"description\": \"desc $i\", \"choices\": [ \"choice 1\", \"choice 2\", 
            \"choice 3\" ] }" -X POST 127.0.0.1:3000/api/vote/add
    done
}

deleteDummyVotes() {
    for i in `seq 1 $1`; do
        curl --data "{ \"id\": $i }" -X POST 127.0.0.1:3000/api/vote/remove
    done
}

usage() {
    echo "Usage: [script] -a/-d [num]"
}

if [ -z ${2+x} ]
then
    usage
    exit
fi

startServer
sleep 1
if [ "$1" == '-a' ]
then
    insertDummyVotes $2
elif [ "$1" == '-d' ]
then
    deleteDummyVotes $2
fi
stopServer
