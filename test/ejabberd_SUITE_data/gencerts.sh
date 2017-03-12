#!/bin/sh
# Update openssl.cnf if needed (in particular section [alt_names])

rm -rf ssl
mkdir -p ssl/newcerts
touch ssl/index.txt
echo 01 > ssl/serial
echo 1000 > ssl/crlnumber
openssl genrsa -out ssl/client.key
openssl req -new -key ssl/client.key -out ssl/client.csr -config openssl.cnf -batch -subj /C=AU/ST=Some-State/O=Internet\ Widgits\ Pty\ Ltd/CN=localhost
openssl ca -keyfile ca.key -cert ca.pem -in ssl/client.csr -out ssl/client.crt -config openssl.cnf -days 10000 -batch -notext
openssl req -new -key ssl/client.key -out ssl/self-signed-client.csr -batch -subj /C=AU/ST=Some-State/O=Internet\ Widgits\ Pty\ Ltd/CN=localhost
openssl x509 -req -in ssl/self-signed-client.csr -signkey ssl/client.key -out ssl/self-signed-client.crt -days 10000
cat ssl/client.crt > cert.pem
cat ssl/self-signed-client.crt > self-signed-cert.pem
cat ssl/client.key >> cert.pem
cat ssl/client.key >> self-signed-cert.pem
rm -rf ssl
