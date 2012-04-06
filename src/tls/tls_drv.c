/*
 * ejabberd, Copyright (C) 2002-2012   ProcessOne
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
#include <sys/types.h>
#include <sys/stat.h>
#include <stdint.h>

#define BUF_SIZE 1024

typedef struct {
      ErlDrvPort port;
      BIO *bio_read;
      BIO *bio_write;
      SSL *ssl;
} tls_data;

#ifdef _WIN32
typedef unsigned __int32 uint32_t;
#endif

#ifndef SSL_OP_NO_TICKET
#define SSL_OP_NO_TICKET 0
#endif

/*
 * R15B changed several driver callbacks to use ErlDrvSizeT and
 * ErlDrvSSizeT typedefs instead of int.
 * This provides missing typedefs on older OTP versions.
 */
#if ERL_DRV_EXTENDED_MAJOR_VERSION < 2
typedef int ErlDrvSizeT;
typedef int ErlDrvSSizeT;
#endif

/*
 * str_hash is based on the public domain code from
 * http://www.burtleburtle.net/bob/hash/doobs.html
 */
static uint32_t str_hash(char *s)
{
   unsigned char *key = (unsigned char *)s;
   uint32_t hash = 0;
   size_t i;

   for (i = 0; key[i] != 0; i++) {
      hash += key[i];
      hash += (hash << 10);
      hash ^= (hash >> 6);
   }
   hash += (hash << 3);
   hash ^= (hash >> 11);
   hash += (hash << 15);
   return hash;
}

/* Linear hashing */

#define MIN_LEVEL 8
#define MAX_LEVEL 20

struct bucket {
      uint32_t hash;
      char *key_file;
      time_t mtime;
      SSL_CTX *ssl_ctx;
      struct bucket *next;
};

struct hash_table {
      int split;
      int level;
      struct bucket **buckets;
      int size;
};

struct hash_table ht;

static void init_hash_table()
{
   size_t size = 1 << (MIN_LEVEL + 1);
   size_t i;
   ht.buckets = (struct bucket **)driver_alloc(sizeof(struct bucket *) * size);
   ht.split = 0;
   ht.level = MIN_LEVEL;
   for (i = 0; i < size; i++)
      ht.buckets[i] = NULL;
   
}

static void hash_table_insert(char *key_file, time_t mtime,
			     SSL_CTX *ssl_ctx)
{
   int level, split;
   uint32_t hash = str_hash(key_file);
   size_t bucket;
   int do_split = 0;
   struct bucket *el;
   struct bucket *new_bucket_el;

   split = ht.split;
   level = ht.level;

   bucket = hash & ((1 << level) - 1);
   if (bucket < split)
      bucket = hash & ((1 << (level + 1)) - 1);

   el = ht.buckets[bucket];
   while (el != NULL) {
      if (el->hash == hash && strcmp(el->key_file, key_file) == 0) {
	 el->mtime = mtime;
	 if (el->ssl_ctx != NULL)
	    SSL_CTX_free(el->ssl_ctx);
	 el->ssl_ctx = ssl_ctx;
	 break;
      }
      el = el->next;
   }

   if (el == NULL) {
      if (ht.buckets[bucket] != NULL)
	 do_split = !0;

      new_bucket_el = (struct bucket *)driver_alloc(sizeof(struct bucket));
      new_bucket_el->hash = hash;
      new_bucket_el->key_file = (char *)driver_alloc(strlen(key_file) + 1);
      strcpy(new_bucket_el->key_file, key_file);
      new_bucket_el->mtime = mtime;
      new_bucket_el->ssl_ctx = ssl_ctx;
      new_bucket_el->next = ht.buckets[bucket];
      ht.buckets[bucket] = new_bucket_el;
   }

   if (do_split) {
      struct bucket **el_ptr = &ht.buckets[split];
      size_t new_bucket = split + (1 << level);
      while (*el_ptr != NULL) {
	 uint32_t hash = (*el_ptr)->hash;
	 if ((hash & ((1 << (level + 1)) - 1)) == new_bucket) {
	    struct bucket *moved_el = *el_ptr;
	    *el_ptr = (*el_ptr)->next;
	    moved_el->next = ht.buckets[new_bucket];
	    ht.buckets[new_bucket] = moved_el;
	 } else
	    el_ptr = &(*el_ptr)->next;
      }
      split++;
      if (split == 1 << level) {
	 size_t size;
	 size_t i;
	 split = 0;
	 level++;
	 size = 1 << (level + 1);
	 ht.split = split;
	 ht.level = level;
	 ht.buckets = (struct bucket **)
	    driver_realloc(ht.buckets, sizeof(struct bucket *) * size);
	 for (i = 1 << level; i < size; i++)
	    ht.buckets[i] = NULL;
      } else
	 ht.split = split;
   }
}

static SSL_CTX *hash_table_lookup(char *key_file, time_t *pmtime)
{
   int level, split;
   uint32_t hash = str_hash(key_file);
   size_t bucket;
   struct bucket *el;

   split = ht.split;
   level = ht.level;

   bucket = hash & ((1 << level) - 1);
   if (bucket < split)
      bucket = hash & ((1 << (level + 1)) - 1);

   el = ht.buckets[bucket];
   while (el != NULL) {
      if (el->hash == hash && strcmp(el->key_file, key_file) == 0) {
	 *pmtime = el->mtime;
	 return el->ssl_ctx;
      }
      el = el->next;
   }

   return NULL;
}


static ErlDrvData tls_drv_start(ErlDrvPort port, char *buff)
{
   tls_data *d = (tls_data *)driver_alloc(sizeof(tls_data));
   d->port = port;
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

   driver_free((char *)handle);
}

static void tls_drv_finish()
{
   int level;
   struct bucket *el;
   int i;

   level = ht.level;
   for (i = 0; i < 1 << (level + 1); i++) {
      el = ht.buckets[i];
      while (el != NULL) {
	 if (el->ssl_ctx != NULL)
	    SSL_CTX_free(el->ssl_ctx);
	 driver_free(el->key_file);
	 el = el->next;
      }
   }

   driver_free(ht.buckets);
}

static int is_key_file_modified(char *file, time_t *key_file_mtime)
{
   struct stat file_stat;

   if (stat(file, &file_stat))
   {
      *key_file_mtime = 0;
      return 1;
   } else {
      if (*key_file_mtime != file_stat.st_mtime)
      {
	 *key_file_mtime = file_stat.st_mtime;
	 return 1;
      } else
	 return 0;
   }
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
#define VERIFY_NONE 0x10000


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


static ErlDrvSSizeT tls_drv_control(ErlDrvData handle,
			   unsigned int command,
			   char *buf, ErlDrvSizeT len,
			   char **rbuf, ErlDrvSizeT rlen)
{
   tls_data *d = (tls_data *)handle;
   int res;
   int size;
   ErlDrvBinary *b;
   X509 *cert;
   unsigned int flags = command;

   command &= 0xffff;

   ERR_clear_error();
   switch (command)
   {
      case SET_CERTIFICATE_FILE_ACCEPT:
      case SET_CERTIFICATE_FILE_CONNECT: {
	 time_t mtime = 0;
	 SSL_CTX *ssl_ctx = hash_table_lookup(buf, &mtime);
	 if (is_key_file_modified(buf, &mtime) || ssl_ctx == NULL)
	 {
	    SSL_CTX *ctx;

	    hash_table_insert(buf, mtime, NULL);

	    ctx = SSL_CTX_new(SSLv23_method());
	    die_unless(ctx, "SSL_CTX_new failed");

	    res = SSL_CTX_use_certificate_chain_file(ctx, buf);
	    die_unless(res > 0, "SSL_CTX_use_certificate_file failed");

	    res = SSL_CTX_use_PrivateKey_file(ctx, buf, SSL_FILETYPE_PEM);
	    die_unless(res > 0, "SSL_CTX_use_PrivateKey_file failed");

	    res = SSL_CTX_check_private_key(ctx);
	    die_unless(res > 0, "SSL_CTX_check_private_key failed");

	    SSL_CTX_set_session_cache_mode(ctx, SSL_SESS_CACHE_OFF);
	    SSL_CTX_set_default_verify_paths(ctx);
#ifdef SSL_MODE_RELEASE_BUFFERS
	    SSL_CTX_set_mode(ctx, SSL_MODE_RELEASE_BUFFERS);
#endif
	    /* SSL_CTX_load_verify_locations(ctx, "/etc/ejabberd/ca_certificates.pem", NULL); */
	    /* SSL_CTX_load_verify_locations(ctx, NULL, "/etc/ejabberd/ca_certs/"); */

	    /* This IF is commented to allow verification in all cases: */
	    /* if (command == SET_CERTIFICATE_FILE_ACCEPT) */
	    /* { */
	       SSL_CTX_set_verify(ctx,
				  SSL_VERIFY_PEER|SSL_VERIFY_CLIENT_ONCE,
				  verify_callback);
	    /* } */

	    ssl_ctx = ctx;
	    hash_table_insert(buf, mtime, ssl_ctx);
	 }

	 d->ssl = SSL_new(ssl_ctx);
	 die_unless(d->ssl, "SSL_new failed");

	 if (flags & VERIFY_NONE)
	    SSL_set_verify(d->ssl, SSL_VERIFY_NONE, verify_callback);

	 d->bio_read = BIO_new(BIO_s_mem());
	 d->bio_write = BIO_new(BIO_s_mem());

	 SSL_set_bio(d->ssl, d->bio_read, d->bio_write);

	 if (command == SET_CERTIFICATE_FILE_ACCEPT) {
	    SSL_set_options(d->ssl, SSL_OP_NO_TICKET);
	    SSL_set_accept_state(d->ssl);
	 } else {
	    SSL_set_options(d->ssl, SSL_OP_NO_SSLv2|SSL_OP_NO_TICKET);
	    SSL_set_connect_state(d->ssl);
	 }
	 break;
      }
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
	 size = BIO_ctrl_pending(d->bio_write) + 1;
	 b = driver_alloc_binary(size);
	 b->orig_bytes[0] = 0;
	 BIO_read(d->bio_write, b->orig_bytes + 1, size - 1);
	 *rbuf = (char *)b;
	 return size;
      case GET_DECRYPTED_INPUT:
	 if (!SSL_is_init_finished(d->ssl))
	 {
	    res = SSL_do_handshake(d->ssl);
	    if (res <= 0)
	       die_unless(SSL_get_error(d->ssl, res) == SSL_ERROR_WANT_READ,
			  "SSL_do_handshake failed");
	 }
	 if (SSL_is_init_finished(d->ssl)) {
	    size_t req_size = 0;
	    if (len == 4)
	    {
	       req_size =
		  (buf[0] << 24) | (buf[1] << 16) | (buf[2] << 8) | buf[3];
	    }
	    size = BUF_SIZE + 1;
	    rlen = 1;
	    b = driver_alloc_binary(size);
	    b->orig_bytes[0] = 0;

	    res = 0;

	    while ((req_size == 0 || rlen < req_size + 1) &&
		   (res = SSL_read(d->ssl,
				   b->orig_bytes + rlen,
				   (req_size == 0 || req_size + 1 >= size) ?
				   size - rlen : req_size + 1 - rlen)) > 0)
	    {
	       //printf("%d bytes of decrypted data read from state machine\r\n",res);
	       rlen += res;
	       if (size - rlen < BUF_SIZE) {
		  size *= 2;
		  b = driver_realloc_binary(b, size);
	       }
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
   tls_drv_finish,		/* F_PTR finish, called when unloaded */
   NULL,			/* handle */
   tls_drv_control,		/* F_PTR control, port_command callback */
   NULL,			/* F_PTR timeout, reserved */
   NULL,			/* F_PTR outputv, reserved */
  /* Added in Erlang/OTP R15B: */
  NULL,                 /* ready_async */
  NULL,                 /* flush */
  NULL,                 /* call */
  NULL,                 /* event */
  ERL_DRV_EXTENDED_MARKER,        /* extended_marker */
  ERL_DRV_EXTENDED_MAJOR_VERSION, /* major_version */
  ERL_DRV_EXTENDED_MINOR_VERSION, /* minor_version */
  0,                    /* driver_flags */
  NULL,                 /* handle2 */
  NULL,                 /* process_exit */
  NULL                  /* stop_select */
};

DRIVER_INIT(tls_drv) /* must match name in driver_entry */
{
   OpenSSL_add_ssl_algorithms();
   SSL_load_error_strings();
   init_hash_table();
   return &tls_driver_entry;
}


