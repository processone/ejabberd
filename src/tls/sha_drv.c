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

#include <erl_driver.h>
#include <openssl/sha.h>
#ifdef HAVE_MD2
#include <openssl/md2.h>
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

static ErlDrvData sha_drv_start(ErlDrvPort port, char *buf)
{
  set_port_control_flags(port, PORT_CONTROL_FLAG_BINARY);
  return NULL;
}

static ErlDrvSSizeT sha_drv_control(ErlDrvData handle,
			   unsigned int command,
			   char *buf, ErlDrvSizeT len,
			   char **rbuf, ErlDrvSizeT rlen)
{
  ErlDrvBinary *b = NULL;

  switch (command) {
#ifdef HAVE_MD2
  case 2:
    rlen = MD2_DIGEST_LENGTH;
    b = driver_alloc_binary(rlen);
    if (b) MD2((unsigned char*)buf, len, (unsigned char*)b->orig_bytes);
    break;
#endif
  case 224:
    rlen = SHA224_DIGEST_LENGTH;
    b = driver_alloc_binary(rlen);
    if (b) SHA224((unsigned char*)buf, len, (unsigned char*)b->orig_bytes);
    break;
  case 256:
    rlen = SHA256_DIGEST_LENGTH;
    b = driver_alloc_binary(rlen);
    if (b) SHA256((unsigned char*)buf, len, (unsigned char*)b->orig_bytes);
    break;
  case 384:
    rlen = SHA384_DIGEST_LENGTH;
    b = driver_alloc_binary(rlen);
    if (b) SHA384((unsigned char*)buf, len, (unsigned char*)b->orig_bytes);
    break;
  case 512:
    rlen = SHA512_DIGEST_LENGTH;
    b = driver_alloc_binary(rlen);
    if (b) SHA512((unsigned char*)buf, len, (unsigned char*)b->orig_bytes);
    break;
  };
  
  if (b) {
    *rbuf = (char *)b;
  } else {
    *rbuf = NULL;
    rlen = 0;
  };

  return rlen;
}

ErlDrvEntry sha_driver_entry = {
  NULL,			/* F_PTR init, N/A */
  sha_drv_start,        /* L_PTR start, called when port is opened */
  NULL,                 /* F_PTR stop, called when port is closed */
  NULL,			/* F_PTR output, called when erlang has sent */
  NULL,			/* F_PTR ready_input, called when input descriptor ready */
  NULL,			/* F_PTR ready_output, called when output descriptor ready */
  "sha_drv",		/* char *driver_name, the argument to open_port */
  NULL,                 /* F_PTR finish, called when unloaded */
  NULL,			/* handle */
  sha_drv_control,	/* F_PTR control, port_command callback */
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

DRIVER_INIT(sha_drv) /* must match name in driver_entry */
{
  return &sha_driver_entry;
}
