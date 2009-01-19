/*
 * ejabberd, Copyright (C) 2002-2009   ProcessOne
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
#include <zlib.h>


#define BUF_SIZE 1024

typedef struct {
      ErlDrvPort port;
      z_stream *d_stream;
      z_stream *i_stream;
} ejabberd_zlib_data;


static ErlDrvData ejabberd_zlib_drv_start(ErlDrvPort port, char *buff)
{
   ejabberd_zlib_data *d =
      (ejabberd_zlib_data *)driver_alloc(sizeof(ejabberd_zlib_data));
   d->port = port;

   d->d_stream = (z_stream *)malloc(sizeof(z_stream));

   d->d_stream->zalloc = (alloc_func)0;
   d->d_stream->zfree = (free_func)0;
   d->d_stream->opaque = (voidpf)0;

   deflateInit(d->d_stream, Z_DEFAULT_COMPRESSION);

   d->i_stream = (z_stream *)malloc(sizeof(z_stream));

   d->i_stream->zalloc = (alloc_func)0;
   d->i_stream->zfree = (free_func)0;
   d->i_stream->opaque = (voidpf)0;

   inflateInit(d->i_stream);

   set_port_control_flags(port, PORT_CONTROL_FLAG_BINARY);

   return (ErlDrvData)d;
}

static void ejabberd_zlib_drv_stop(ErlDrvData handle)
{
   ejabberd_zlib_data *d = (ejabberd_zlib_data *)handle;

   deflateEnd(d->d_stream);
   free(d->d_stream);

   inflateEnd(d->i_stream);
   free(d->i_stream);

   driver_free((char *)handle);
}


#define DEFLATE 1
#define INFLATE 2

#define die_unless(cond, errstr)				\
	 if (!(cond))						\
	 {							\
	    rlen = strlen(errstr) + 1;				\
	    b = driver_realloc_binary(b, rlen);			\
	    b->orig_bytes[0] = 1;				\
	    strncpy(b->orig_bytes + 1, errstr, rlen - 1);	\
	    *rbuf = (char *)b;					\
	    return rlen;					\
	 }


static int ejabberd_zlib_drv_control(ErlDrvData handle,
				     unsigned int command,
				     char *buf, int len,
				     char **rbuf, int rlen)
{
   ejabberd_zlib_data *d = (ejabberd_zlib_data *)handle;
   int err;
   int size;
   ErlDrvBinary *b;

   switch (command)
   {
      case DEFLATE:
	 size = BUF_SIZE + 1;
	 rlen = 1;
	 b = driver_alloc_binary(size);
	 b->orig_bytes[0] = 0;

	 d->d_stream->next_in = (unsigned char *)buf;
	 d->d_stream->avail_in = len;
	 d->d_stream->avail_out = 0;
	 err = Z_OK;

	 while (err == Z_OK && d->d_stream->avail_out == 0)
	 {
	    d->d_stream->next_out = (unsigned char *)b->orig_bytes + rlen;
	    d->d_stream->avail_out = BUF_SIZE;

	    err = deflate(d->d_stream, Z_SYNC_FLUSH);
	    die_unless((err == Z_OK) || (err == Z_STREAM_END),
		       "Deflate error");

	    rlen += (BUF_SIZE - d->d_stream->avail_out);
	    size += (BUF_SIZE - d->d_stream->avail_out);
	    b = driver_realloc_binary(b, size);
	 }
	 b = driver_realloc_binary(b, rlen);
	 *rbuf = (char *)b;
	 return rlen;
      case INFLATE:
	 size = BUF_SIZE + 1;
	 rlen = 1;
	 b = driver_alloc_binary(size);
	 b->orig_bytes[0] = 0;

	 if (len > 0) {
	    d->i_stream->next_in = (unsigned char *)buf;
	    d->i_stream->avail_in = len;
	    d->i_stream->avail_out = 0;
	    err = Z_OK;

	    while (err == Z_OK && d->i_stream->avail_out == 0)
	    {
	       d->i_stream->next_out = (unsigned char *)b->orig_bytes + rlen;
	       d->i_stream->avail_out = BUF_SIZE;

	       err = inflate(d->i_stream, Z_SYNC_FLUSH);
	       die_unless((err == Z_OK) || (err == Z_STREAM_END),
			  "Inflate error");

	       rlen += (BUF_SIZE - d->i_stream->avail_out);
	       size += (BUF_SIZE - d->i_stream->avail_out);
	       b = driver_realloc_binary(b, size);
	    }
	 }
	 b = driver_realloc_binary(b, rlen);
	 *rbuf = (char *)b;
	 return rlen;
   }

   b = driver_alloc_binary(1);
   b->orig_bytes[0] = 0;
   *rbuf = (char *)b;
   return 1;
}


ErlDrvEntry ejabberd_zlib_driver_entry = {
   NULL,			/* F_PTR init, N/A */
   ejabberd_zlib_drv_start,	/* L_PTR start, called when port is opened */
   ejabberd_zlib_drv_stop,	/* F_PTR stop, called when port is closed */
   NULL,			/* F_PTR output, called when erlang has sent */
   NULL,			/* F_PTR ready_input, called when input descriptor ready */
   NULL,			/* F_PTR ready_output, called when output descriptor ready */
   "ejabberd_zlib_drv",		/* char *driver_name, the argument to open_port */
   NULL,			/* F_PTR finish, called when unloaded */
   NULL,			/* handle */
   ejabberd_zlib_drv_control,   /* F_PTR control, port_command callback */
   NULL,			/* F_PTR timeout, reserved */
   NULL				/* F_PTR outputv, reserved */
};

DRIVER_INIT(ejabberd_zlib_drv) /* must match name in driver_entry */
{
   return &ejabberd_zlib_driver_entry;
}


