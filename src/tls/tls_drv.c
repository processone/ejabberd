/*
 * ejabberd, Copyright (C) 2002-2008   ProcessOne
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 2 of the
 * License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *                         
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
 * 02111-1307 USA
 *
 */

#include <stdio.h>
#include <string.h>
#include <erl_driver.h>
#include <openssl/ssl.h>
#include <openssl/err.h>


#define BUF_SIZE 1024

typedef struct {
      ErlDrvPort port;
      SSL_CTX *ctx;
      BIO *bio_read;
      BIO *bio_write;
      SSL *ssl;
} tls_data;


static ErlDrvData tls_drv_start(ErlDrvPort port, char *buff)
{
   tls_data *d = (tls_data *)driver_alloc(sizeof(tls_data));
   d->port = port;
   d->ctx = NULL;
   d->bio_read = NULL;
   d->bio_write = NULL;
   d->ssl = NULL;

   set_port_control_flags(port, PORT_CONTROL_FLAG_BINARY);

   return (ErlDrvData)d;
}

static void tls_drv_stop(ErlDrvData handle)
{
   tls_data *d = (tls_data *)handle;

   if (d->ssl != NULL)
      SSL_free(d->ssl);

   if (d->ctx != NULL)
      SSL_CTX_free(d->ctx);

   driver_free((char *)handle);
}


static int verify_callback(int preverify_ok, X509_STORE_CTX *ctx)
{
   return 1;
}

#define SET_CERTIFICATE_FILE_ACCEPT 1
#define SET_CERTIFICATE_FILE_CONNECT 2
#define SET_ENCRYPTED_INPUT  3
#define SET_DECRYPTED_OUTPUT 4
#define GET_ENCRYPTED_OUTPUT 5
#define GET_DECRYPTED_INPUT  6
#define GET_PEER_CERTIFICATE 7
#define GET_VERIFY_RESULT    8


#define die_unless(cond, errstr)				\
	 if (!(cond))						\
	 {							\
	    int errstrlen = strlen(errstr);			\
	    unsigned long error_code = ERR_get_error();		\
	    char *error_string = error_code ?			\
	       ERR_error_string(error_code, NULL) :		\
	       NULL;						\
	    int error_string_length = error_string ?		\
	       strlen(error_string) : 0;			\
	    if (error_code)					\
	       rlen = errstrlen + error_string_length + 3;	\
	    else						\
	       rlen = errstrlen + 1;				\
	    b = driver_alloc_binary(rlen);			\
	    b->orig_bytes[0] = 1;				\
	    strncpy(b->orig_bytes + 1, errstr, errstrlen);	\
	    if (error_code) {					\
	       strncpy(b->orig_bytes + 1 + errstrlen,		\
		       ": ", 2);				\
	       strncpy(b->orig_bytes + 3 + errstrlen,		\
		       error_string, error_string_length);	\
	    }							\
	    *rbuf = (char *)b;					\
	    return rlen;					\
	 }


static int tls_drv_control(ErlDrvData handle,
			   unsigned int command,
			   char *buf, int len,
			   char **rbuf, int rlen)
{
   tls_data *d = (tls_data *)handle;
   int res;
   int size;
   ErlDrvBinary *b;
   X509 *cert;

   ERR_clear_error();
   switch (command)
   {
      case SET_CERTIFICATE_FILE_ACCEPT:
      case SET_CERTIFICATE_FILE_CONNECT:
	 d->ctx = SSL_CTX_new(SSLv23_method());
	 die_unless(d->ctx, "SSL_CTX_new failed");

	 res = SSL_CTX_use_certificate_chain_file(d->ctx, buf);
	 die_unless(res > 0, "SSL_CTX_use_certificate_file failed");

	 res = SSL_CTX_use_PrivateKey_file(d->ctx, buf, SSL_FILETYPE_PEM);
	 die_unless(res > 0, "SSL_CTX_use_PrivateKey_file failed");

	 res = SSL_CTX_check_private_key(d->ctx);
	 die_unless(res > 0, "SSL_CTX_check_private_key failed");

	 SSL_CTX_set_default_verify_paths(d->ctx);

	 if (command == SET_CERTIFICATE_FILE_ACCEPT)
	 {
	    SSL_CTX_set_verify(d->ctx,
			       SSL_VERIFY_PEER|SSL_VERIFY_CLIENT_ONCE,
			       verify_callback);
	 }
	 
	 d->ssl = SSL_new(d->ctx);
	 die_unless(d->ssl, "SSL_new failed");

	 d->bio_read = BIO_new(BIO_s_mem());
	 d->bio_write = BIO_new(BIO_s_mem());

	 SSL_set_bio(d->ssl, d->bio_read, d->bio_write);

	 if (command == SET_CERTIFICATE_FILE_ACCEPT)
	    SSL_set_accept_state(d->ssl);
	 else
	    SSL_set_connect_state(d->ssl);
	 break;
      case SET_ENCRYPTED_INPUT:
	 die_unless(d->ssl, "SSL not initialized");
	 BIO_write(d->bio_read, buf, len);
	 break;
      case SET_DECRYPTED_OUTPUT:
	 die_unless(d->ssl, "SSL not initialized");
	 res = SSL_write(d->ssl, buf, len);
	 if (res <= 0) 
	 {
	    res = SSL_get_error(d->ssl, res);
	    if (res == SSL_ERROR_WANT_READ || res == SSL_ERROR_WANT_WRITE) 
	    {
	       b = driver_alloc_binary(1);
	       b->orig_bytes[0] = 2;
	       *rbuf = (char *)b;
	       return 1;
	    } else {
	       die_unless(0, "SSL_write failed");
	    }
	 }
	 break;
      case GET_ENCRYPTED_OUTPUT:
	 die_unless(d->ssl, "SSL not initialized");
	 size = BUF_SIZE + 1;
	 rlen = 1;
	 b = driver_alloc_binary(size);
	 b->orig_bytes[0] = 0;
	 while ((res = BIO_read(d->bio_write,
				b->orig_bytes + rlen, BUF_SIZE)) > 0)
	 {
	    //printf("%d bytes of encrypted data read from state machine\r\n", res);

	    rlen += res;
	    size += BUF_SIZE;
	    b = driver_realloc_binary(b, size);
	 }
	 b = driver_realloc_binary(b, rlen);
	 *rbuf = (char *)b;
	 return rlen;
      case GET_DECRYPTED_INPUT:
	 if (!SSL_is_init_finished(d->ssl))
	 {
	    res = SSL_do_handshake(d->ssl);
	    if (res <= 0)
	       die_unless(SSL_get_error(d->ssl, res) == SSL_ERROR_WANT_READ,
			  "SSL_do_handshake failed");
	 } else {
	    size = BUF_SIZE + 1;
	    rlen = 1;
	    b = driver_alloc_binary(size);
	    b->orig_bytes[0] = 0;

	    while ((res = SSL_read(d->ssl,
				   b->orig_bytes + rlen, BUF_SIZE)) > 0)
	    {
	       //printf("%d bytes of decrypted data read from state machine\r\n",res);
	       rlen += res;
	       size += BUF_SIZE;
	       b = driver_realloc_binary(b, size);
	    }

	    if (res < 0)
	    {
	       int err = SSL_get_error(d->ssl, res);

	       if (err == SSL_ERROR_WANT_READ)
	       {
		  //printf("SSL_read wants more data\r\n");
		  //return 0;
	       }
	       // TODO
	    }
	    b = driver_realloc_binary(b, rlen);
	    *rbuf = (char *)b;
	    return rlen;
	 }
	 break;
      case GET_PEER_CERTIFICATE:
	 cert = SSL_get_peer_certificate(d->ssl);
	 if (cert == NULL)
	 {
	    b = driver_alloc_binary(1);
	    b->orig_bytes[0] = 1;
	    *rbuf = (char *)b;
	    return 1;
	 } else {
	    unsigned char *tmp_buf;
	    rlen = i2d_X509(cert, NULL);
	    if (rlen >= 0)
	    {
	       rlen++;
	       b = driver_alloc_binary(rlen);
	       b->orig_bytes[0] = 0;
	       tmp_buf = (unsigned char *)&b->orig_bytes[1];
	       i2d_X509(cert, &tmp_buf);
	       X509_free(cert);
	       *rbuf = (char *)b;
	       return rlen;
	    } else
	       X509_free(cert);
	 }
	 break;
      case GET_VERIFY_RESULT:
	 b = driver_alloc_binary(1);
	 b->orig_bytes[0] = SSL_get_verify_result(d->ssl);
	 *rbuf = (char *)b;
	 return 1;
	 break;
   }

   b = driver_alloc_binary(1);
   b->orig_bytes[0] = 0;
   *rbuf = (char *)b;
   return 1;
}


ErlDrvEntry tls_driver_entry = {
   NULL,			/* F_PTR init, N/A */
   tls_drv_start,		/* L_PTR start, called when port is opened */
   tls_drv_stop,		/* F_PTR stop, called when port is closed */
   NULL,			/* F_PTR output, called when erlang has sent */
   NULL,			/* F_PTR ready_input, called when input descriptor ready */
   NULL,			/* F_PTR ready_output, called when output descriptor ready */
   "tls_drv",			/* char *driver_name, the argument to open_port */
   NULL,			/* F_PTR finish, called when unloaded */
   NULL,			/* handle */
   tls_drv_control,		/* F_PTR control, port_command callback */
   NULL,			/* F_PTR timeout, reserved */
   NULL				/* F_PTR outputv, reserved */
};

DRIVER_INIT(tls_drv) /* must match name in driver_entry */
{
   OpenSSL_add_ssl_algorithms();
   SSL_load_error_strings();
   return &tls_driver_entry;
}


