#!/bin/bash

if [ -e /cert/key.pem ] && [ -e /cert/cert.pem ]
then
    echo "Found certificate"
else
    echo "Generating certificate"
    openssl req -x509 -newkey rsa:4096 -keyout /cert/key.pem -out /cert/cert.pem -days 365 -nodes
exit

nginx -c /app/config/nginx.conf
/app/server
