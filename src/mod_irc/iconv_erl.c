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
#include <ei.h>
#include <iconv.h>

/*
 * R15B changed several driver callbacks to use ErlDrvSizeT and
 * ErlDrvSSizeT typedefs instead of int.
 * This provides missing typedefs on older OTP versions.
 */
#if ERL_DRV_EXTENDED_MAJOR_VERSION < 2
typedef int ErlDrvSizeT;
typedef int ErlDrvSSizeT;
#endif

typedef struct {
      ErlDrvPort port;
      iconv_t cd;
} iconv_data;


static ErlDrvData iconv_erl_start(ErlDrvPort port, char *buff)
{
   iconv_data* d = (iconv_data*)driver_alloc(sizeof(iconv_data));
   d->port = port;
   d->cd = NULL;

   set_port_control_flags(port, PORT_CONTROL_FLAG_BINARY);
   
   return (ErlDrvData)d;
}

static void iconv_erl_stop(ErlDrvData handle)
{
   driver_free((char*)handle);
}

static ErlDrvSSizeT iconv_erl_control(ErlDrvData drv_data,
			     unsigned int command,
			     char *buf, ErlDrvSizeT len,
			     char **rbuf, ErlDrvSizeT rlen)
{
   int i;
   int size;
   int index = 0;
   int avail;
   size_t inleft, outleft;
   ErlDrvBinary *b;
   char *from, *to, *string, *stmp, *rstring, *rtmp;
   iconv_t cd;
   int invalid_utf8_as_latin1 = 0;

   ei_decode_version(buf, &index, &i);
   ei_decode_tuple_header(buf, &index, &i);
   ei_get_type(buf, &index, &i, &size);
   from = driver_alloc(size + 1); 
   ei_decode_string(buf, &index, from);

   ei_get_type(buf, &index, &i, &size);
   to = driver_alloc(size + 1); 
   ei_decode_string(buf, &index, to);
  
   ei_get_type(buf, &index, &i, &size);
   stmp = string = driver_alloc(size + 1); 
   ei_decode_string(buf, &index, string);

   /* Special mode: parse as UTF-8 if possible; otherwise assume it's
      Latin-1.  Makes no difference when encoding. */
   if (strcmp(from, "utf-8+latin-1") == 0) {
      from[5] = '\0';
      invalid_utf8_as_latin1 = 1;
   }
   if (strcmp(to, "utf-8+latin-1") == 0) {
      to[5] = '\0';
   }
   cd = iconv_open(to, from);

   if (cd == (iconv_t) -1) {
      cd = iconv_open("ascii", "ascii");
      if (cd == (iconv_t) -1) {
	 *rbuf = (char*)(b = driver_alloc_binary(size));
	 memcpy(b->orig_bytes, string, size);

	 driver_free(from);
	 driver_free(to);
	 driver_free(string);

	 return size;
      }
   }
   
   outleft = avail = 4*size;
   inleft = size;
   rtmp = rstring = driver_alloc(avail);
   while (inleft > 0) {
      if (iconv(cd, &stmp, &inleft, &rtmp, &outleft) == (size_t) -1) {
	 if (invalid_utf8_as_latin1 && (*stmp & 0x80) && outleft >= 2) {
	    /* Encode one byte of (assumed) Latin-1 into two bytes of UTF-8 */
	    *rtmp++ = 0xc0 | ((*stmp & 0xc0) >> 6);
	    *rtmp++ = 0x80 | (*stmp & 0x3f);
	    outleft -= 2;
	 }
	 stmp++;
	 inleft--;
      }
   }
   
   size = rtmp - rstring;

   *rbuf = (char*)(b = driver_alloc_binary(size));
   memcpy(b->orig_bytes, rstring, size);

   driver_free(from);
   driver_free(to);
   driver_free(string);
   driver_free(rstring);
   iconv_close(cd);
   
   return size;
}



ErlDrvEntry iconv_driver_entry = {
   NULL,                       /* F_PTR init, N/A */
   iconv_erl_start,          /* L_PTR start, called when port is opened */
   iconv_erl_stop,           /* F_PTR stop, called when port is closed */
   NULL,         /* F_PTR output, called when erlang has sent */
   NULL,                       /* F_PTR ready_input, called when input descriptor ready */
   NULL,                       /* F_PTR ready_output, called when output descriptor ready */
   "iconv_erl",              /* char *driver_name, the argument to open_port */
   NULL,                       /* F_PTR finish, called when unloaded */
   NULL,                       /* handle */
   iconv_erl_control,          /* F_PTR control, port_command callback */
   NULL,                       /* F_PTR timeout, reserved */
   NULL,                       /* F_PTR outputv, reserved */
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

DRIVER_INIT(iconv_erl) /* must match name in driver_entry */
{
    return &iconv_driver_entry;
}


