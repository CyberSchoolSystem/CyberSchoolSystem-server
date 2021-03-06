#!/bin/bash

if [ -e /cert/key.pem ] && [ -e /cert/cert.pem ]
then
    echo "Found certificate"
else
    echo "Generating certificate"
    mkdir /cert -p
    openssl req -x509 -newkey rsa:4096 -keyout /cert/key.pem \
         -out /cert/cert.pem -days 365 -nodes \
         -subj "/C=DE/ST=Baden Württemberg/L=Loc/O=Schule Als Staat/OU=Wahlsystem/CN=fred-sas/emailAddress=noname"
fi

nginx -c /app/config/nginx.conf
/app/server
