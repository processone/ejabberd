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
#include <ei.h>
#include <iconv.h>

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

static int iconv_erl_control(ErlDrvData drv_data,
			     unsigned int command,
			     char *buf, int len,
			     char **rbuf, int rlen)
{
   int i;
   int size;
   int index = 0;
   int avail;
   size_t inleft, outleft;
   ErlDrvBinary *b;
   char *from, *to, *string, *stmp, *rstring, *rtmp;
   iconv_t cd;

   ei_decode_version(buf, &index, &i);
   ei_decode_tuple_header(buf, &index, &i);
   ei_get_type(buf, &index, &i, &size);
   from = malloc(size + 1); 
   ei_decode_string(buf, &index, from);

   ei_get_type(buf, &index, &i, &size);
   to = malloc(size + 1); 
   ei_decode_string(buf, &index, to);
  
   ei_get_type(buf, &index, &i, &size);
   stmp = string = malloc(size + 1); 
   ei_decode_string(buf, &index, string);

   cd = iconv_open(to, from);

   if (cd == (iconv_t) -1) {
      cd = iconv_open("ascii", "ascii");
      if (cd == (iconv_t) -1) {
	 *rbuf = (char*)(b = driver_alloc_binary(size));
	 memcpy(b->orig_bytes, string, size);

	 free(from);
	 free(to);
	 free(string);

	 return size;
      }
   }
   
   outleft = avail = 4*size;
   inleft = size;
   rtmp = rstring = malloc(avail);
   while (inleft > 0) {
      if (iconv(cd, &stmp, &inleft, &rtmp, &outleft) == (size_t) -1) {
	 stmp++;
	 inleft--;
      }
   }
   
   size = rtmp - rstring;

   *rbuf = (char*)(b = driver_alloc_binary(size));
   memcpy(b->orig_bytes, rstring, size);

   free(from);
   free(to);
   free(string);
   free(rstring);
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
   NULL                        /* F_PTR outputv, reserved */
};

DRIVER_INIT(iconv_erl) /* must match name in driver_entry */
{
    return &iconv_driver_entry;
}


