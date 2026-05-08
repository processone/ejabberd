# HOWTO - collection of tips'n'tricks around chaning mod\_invite's templates

## How to create checksums (SRI) for included CSS and JS files used in templates

```console
openssl dgst -sha384 -binary priv/mod_invites/static/invite.js | openssl base64 -A
```
