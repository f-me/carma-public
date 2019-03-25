#!/usr/bin/env bash

# based on https://jamielinux.com/docs/openssl-certificate-authority/index.html
# TODO: check if root

set -e

ROOT_STORE=/etc/ssl/caroperator-svc/root
ROOT_KEY=$ROOT_STORE/private/ca.key
ROOT_CERT=$ROOT_STORE/certs/ca.cert

IMDT_STORE=/etc/ssl/caroperator-svc/intermediate
IMDT_KEY=$IMDT_STORE/private/intermediate.key
IMDT_CERT=$IMDT_STORE/certs/intermediate.cert
IMDT_CSR=$ROOT_STORE/csr/intermedite.csr
CA_CHAIN=$IMDT_STORE/certs/ca-chain.cert

case $1 in
    init-ca)
        echo === Prepare the root directory
        mkdir -p $ROOT_STORE/{certs,crl,csr,newcerts,private}
        chmod 700 $ROOT_STORE/private
        touch $ROOT_STORE/index.txt
        echo 1000 > $ROOT_STORE/serial

        echo === Create the root key
        openssl genrsa -out $ROOT_KEY 4096
        chmod 400 $ROOT_KEY

        echo === Create the root certificate
        openssl req -config conf/openssl-root.conf \
          -new -x509 -days 7300 -extensions v3_ca \
          -key $ROOT_KEY \
          -out $ROOT_CERT
        chmod 444 $ROOT_CERT

        echo === Verify the root certificate
        openssl x509 -noout -text -in $ROOT_CERT


        echo === Prepare the intermediate directory
        mkdir -p $IMDT_STORE/{certs,crl,csr,newcerts,private}
        chmod 700 $IMDT_STORE/private
        touch $IMDT_STORE/index.txt
        echo 1000 > $IMDT_STORE/serial
        echo 1000 > $IMDT_STORE/crlnumber

        echo === Create the intermediate key
        openssl genrsa -out $IMDT_KEY 4096
        chmod 400 $IMDT_KEY

        echo === Create the intermediate certificate
        openssl req -config conf/openssl-intermediate.conf \
          -new \
          -key $IMDT_KEY \
          -out $IMDT_CSR

        openssl ca -config conf/openssl-root.conf \
          -extensions v3_intermediate_ca \
          -days 3650 -notext -md sha256 \
          -in $IMDT_CSR \
          -out $IMDT_CERT
        chmod 444 $IMDT_CERT

        echo === Verify the intermediate certificate
        openssl verify -CAfile $ROOT_CERT $IMDT_CERT

        echo === Create the certificate chain file
        cat $IMDT_CERT $ROOT_CERT > $CA_CHAIN
        chmod 444 $CA_CHAIN
        ;;
    gen-csr)
        CLIENT_NAME=$2
        KEY=$IMDT_STORE/private/$CLIENT_NAME.key
        CSR=$IMDT_STORE/csr/$CLIENT_NAME.csr

        openssl genrsa -out $KEY 2048
        chmod 400 $KEY
        openssl req -config conf/openssl-intermediate.conf \
          -key $KEY \
          -new -sha256 -out $CSR
        ;;
    sign-user-cert)
        CLIENT_NAME=$2
        CSR=$IMDT_STORE/csr/$CLIENT_NAME.csr
        CERT=$IMDT_STORE/certs/$CLIENT_NAME.cert

        openssl ca -config conf/openssl-intermediate.conf \
          -extensions usr_cert -days 375 -notext -md sha256 \
          -in $CSR \
          -out $CERT
        chmod 444 $CERT
        ;;
    *)
        echo "Usage:"
        echo "    init-ca"
        echo "    gen-csr <client-name>"
        echo "    sign-user-cert <client-name>"
        exit 1
        ;;
esac
