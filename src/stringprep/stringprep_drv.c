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

#include "uni_data.c"
#include "uni_norm.c"

#define NAMEPREP_COMMAND 1
#define NODEPREP_COMMAND 2
#define RESOURCEPREP_COMMAND 3

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
} stringprep_data;


static ErlDrvData stringprep_erl_start(ErlDrvPort port, char *buff)
{
   stringprep_data* d = (stringprep_data*)driver_alloc(sizeof(stringprep_data));
   d->port = port;

   //set_port_control_flags(port, PORT_CONTROL_FLAG_BINARY);
   
   return (ErlDrvData)d;
}

static void stringprep_erl_stop(ErlDrvData handle)
{
   driver_free((char*)handle);
}


/* Hangul constants */
#define SBase 0xAC00
#define LBase 0x1100
#define VBase 0x1161
#define TBase 0x11A7
#define LCount 19
#define VCount 21
#define TCount 28
#define NCount (VCount * TCount)
#define SCount (LCount * NCount)

/*
 * "canonical_ordering" and "compose" functions are based on nfkc.c from Gnome
 * library
 */

static void canonical_ordering(int *str, int len)
{
   int i, j, t;
   int last, next;

   last = GetUniCharCClass(str[0]);
   for (i = 0; i < len - 1; i++)
   {
      next = GetUniCharCClass(str[i + 1]);
      if (next != 0 && last > next)
      {
	 for (j = i; j >= 0; j--)
	 {
	    if (GetUniCharCClass(str[j]) <= next)
	       break;
	    t = str[j + 1];
	    str[j + 1] = str[j];
	    str[j] = t;
	 }
	 next = last;
      }
      last = next;
   }
}


static int compose(int ch1, int ch2)
{
   int info1, info2;

   if (LBase <= ch1 && ch1 < LBase + LCount &&
       VBase <= ch2 && ch2 < VBase + VCount) {
      return SBase + ((ch1 - LBase) * VCount + (ch2 - VBase)) * TCount;
   }

   if (SBase <= ch1 && ch1 < SBase + SCount && ((ch1 - SBase) % TCount) == 0 &&
       TBase <= ch2 && ch2 < TBase + TCount) {
      return ch1 + ch2 - TBase;
   }

   info1 = GetUniCharCompInfo(ch1);
   if (info1 != -1 && info1 & CompSingleMask) {
      if (!(info1 & CompSecondMask) &&
	  ch2 == compFirstList[info1 & CompMask][0]) {
	 return compFirstList[info1 & CompMask][1];
      } else
	 return 0;
   }

   info2 = GetUniCharCompInfo(ch2);
   if (info2 != -1 && info2 & CompSingleMask) {
      if ((info2 & CompSecondMask) &&
	  ch1 == compSecondList[info2 & CompMask][0]) {
	 return compSecondList[info2 & CompMask][1];
      } else
	 return 0;
   }

   if (info1 != -1 && info2 != -1 &&
       !(info1 & CompSecondMask) && (info2 & CompSecondMask))
      return compBothList[info1][info2 & CompMask];
   else
      return 0;
}


#define ADD_UCHAR(ruc)							\
	 if (ruc <= 0x7F) {						\
	    if (pos >= size) {						\
	       size = 2*size + 1;					\
	       rstring = driver_realloc(rstring, size);			\
	    }								\
	    rstring[pos] = (char) ruc;					\
	    pos++;							\
	 } else if (ruc <= 0x7FF) {					\
	    if (pos + 1 >= size) {					\
	       size = 2*size + 2;					\
	       rstring = driver_realloc(rstring, size);			\
	    }								\
	    rstring[pos] = (char) ((ruc >> 6) | 0xC0);			\
	    rstring[pos+1] = (char) ((ruc | 0x80) & 0xBF);		\
	    pos += 2;							\
	 } else if (ruc <= 0xFFFF) {					\
	    if (pos + 2 >= size) {					\
	       size = 2*size + 3;					\
	       rstring = driver_realloc(rstring, size);			\
	    }								\
	    rstring[pos] = (char) ((ruc >> 12) | 0xE0);			\
	    rstring[pos+1] = (char) (((ruc >> 6) | 0x80) & 0xBF);	\
	    rstring[pos+2] = (char) ((ruc | 0x80) & 0xBF);		\
	    pos += 3;							\
	 } else if (ruc <= 0x1FFFFF) {					\
	    if (pos + 3 >= size) {					\
	       size = 2*size + 4;					\
	       rstring = driver_realloc(rstring, size);			\
	    }								\
	    rstring[pos] = (char) ((ruc >> 18) | 0xF0);			\
	    rstring[pos+1] = (char) (((ruc >> 12) | 0x80) & 0xBF);	\
	    rstring[pos+2] = (char) (((ruc >> 6) | 0x80) & 0xBF);	\
	    rstring[pos+3] = (char) ((ruc | 0x80) & 0xBF);		\
	    pos += 4;							\
	 }

#define ADD_UCHAR32(str, pos, len, ch)				\
	    if (pos >= len) {					\
	       len = 2*len + 1;					\
	       str = driver_realloc(str, len * sizeof(int));	\
	    }							\
	    str[pos] = ch;					\
	    pos++;


#define ADD_DECOMP(ruc)						\
	       info = GetUniCharDecompInfo(ruc);		\
	       if (info >= 0) {					\
		  decomp_len = GetDecompLen(info);		\
		  decomp_shift = GetDecompShift(info);		\
		  for (j = 0; j < decomp_len; j++) {		\
	             ADD_UCHAR32(str32, str32pos, str32len,	\
				 decompList[decomp_shift + j]);	\
		  }						\
	       } else {						\
		  ADD_UCHAR32(str32, str32pos, str32len, ruc);	\
	       }



static ErlDrvSSizeT stringprep_erl_control(ErlDrvData drv_data,
				  unsigned int command,
				  char *buf, ErlDrvSizeT len,
				  char **rbuf, ErlDrvSizeT rlen)
{
   int i, j, pos=1;
   unsigned char c;
   int bad = 0;
   int uc = 0, ruc;
   int size;
   int info;
   int prohibit = 0, tolower = 0;
   char *rstring;
   int *mc;
   int *str32;
   int str32len, str32pos = 0;
   int decomp_len, decomp_shift;
   int comp_pos, comp_starter_pos;
   int cclass_prev, cclass2;
   int ch1, ch2;
   int first_ral, last_ral, have_ral, have_l;

   size = len + 1;

   rstring = driver_alloc(size);
   rstring[0] = 0;

   str32len = len + 1;

   str32 = driver_alloc(str32len * sizeof(int));

   switch (command)
   {
      case 0:
	 prohibit = ACMask;
	 tolower = 1;
	 break;

      case NAMEPREP_COMMAND:
	 prohibit = ACMask;
	 tolower = 1;
	 break;

      case NODEPREP_COMMAND:
	 prohibit = ACMask | C11Mask | C21Mask | XNPMask;
	 tolower = 1;
	 break;

      case RESOURCEPREP_COMMAND:
	 prohibit = ACMask | C21Mask;
	 tolower = 0;
	 break;
   }

   for (i = 0; i < len; i++)
   {
      c = buf[i];
      if (c < 0x80) {
	 uc = c;
      } else if (c < 0xC0) {
	 bad = 1;
      } else if (c < 0xE0) {
	 if (i+1 < len && (buf[i+1] & 0xC0) == 0x80) {
	    uc = ((c & 0x1F) << 6) | (buf[i+1] & 0x3F);
	    i++;
	 } else {
	    bad = 1;
	 }
      } else if (c < 0xF0) {
	 if (i+2 < len && (buf[i+1] & 0xC0) == 0x80 &&
	     (buf[i+2] & 0xC0) == 0x80) {
	    uc = ((c & 0x0F) << 12)
	       | ((buf[i+1] & 0x3F) << 6)
	       | (buf[i+2] & 0x3F);
	    i += 2;
	 } else {
	    bad = 1;
	 }
      } else if (c < 0xF8) {
	 if (i+3 < len &&
	     (buf[i+1] & 0xC0) == 0x80 &&
	     (buf[i+2] & 0xC0) == 0x80 &&
	     (buf[i+3] & 0xC0) == 0x80) {
	    uc = ((c & 0x07) << 18)
	       | ((buf[i+1] & 0x3F) << 12)
	       | ((buf[i+2] & 0x3F) << 6)
	       | (buf[i+3] & 0x3F);
	    i += 3;
	    if (uc > 0x10FFFF)
	       bad = 1;
	 } else {
	    bad = 1;
	 }
      } else {
	 bad = 1;
      }

      if (bad) {
	 *rbuf = rstring;
	 driver_free(str32);
	 return 1;
      }
      
      info = GetUniCharInfo(uc);

      if (!(info & B1Mask)) 
      {
	 if (tolower) {
	    if (!(info & MCMask)) 
	    {
	       ruc = uc + GetDelta(info);
	       ADD_DECOMP(ruc);
	    } else {
	       mc = GetMC(info);
	       for (j = 1; j <= mc[0]; j++) {
		  ruc = mc[j];
		  ADD_DECOMP(ruc);
	       }
	    }
	 } else {
	    ruc = uc;
	    ADD_DECOMP(ruc);
	 }
      }
   }

   if (str32pos == 0) {
      rstring[0] = 1;
      *rbuf = rstring;
      driver_free(str32);
      return 1;
   }

   canonical_ordering(str32, str32pos);

   comp_pos = 1;
   comp_starter_pos = 0;
   ch1 = str32[0];
   cclass_prev = GetUniCharCClass(ch1);
   for (i = 1; i < str32pos; i++)
   {
      ch2 = str32[i];
      cclass2 = GetUniCharCClass(ch2);
      if ((cclass_prev == 0 || cclass2 > cclass_prev) &&
	  (ruc = compose(ch1, ch2))) {
	 ch1 = ruc;
      } else {
	 if (cclass2 == 0) {
	    str32[comp_starter_pos] = ch1;
	    comp_starter_pos = comp_pos++;
	    ch1 = ch2;
	    cclass_prev = 0;
	 } else {
	    str32[comp_pos++] = ch2;
	    cclass_prev = cclass2;
	 }
      }
   }
   str32[comp_starter_pos] = ch1;
   str32pos = comp_pos;
   
   last_ral = have_ral = have_l = 0;
   info = GetUniCharInfo(str32[0]);
   first_ral = info & D1Mask;
   for (i = 0; i < str32pos; i++)
   {
      ruc = str32[i];
      info = GetUniCharInfo(ruc);
      if (info & prohibit) {
	 *rbuf = rstring;
	 driver_free(str32);
	 return 1;
      }
      last_ral = info & D1Mask;
      have_ral = have_ral || last_ral;
      have_l |= info & D2Mask;
      ADD_UCHAR(ruc);
   }

   if (have_ral && (!first_ral || !last_ral || have_l)) {
      *rbuf = rstring;
      driver_free(str32);
      return 1;
   }

   rstring[0] = 1;
   *rbuf = rstring;
   driver_free(str32);
   
   return pos;
}



ErlDrvEntry stringprep_driver_entry = {
   NULL,			/* F_PTR init, N/A */
   stringprep_erl_start,	/* L_PTR start, called when port is opened */
   stringprep_erl_stop,		/* F_PTR stop, called when port is closed */
   NULL,			/* F_PTR output, called when erlang has sent */
   NULL,			/* F_PTR ready_input, called when input descriptor ready */
   NULL,			/* F_PTR ready_output, called when output descriptor ready */
   "stringprep_drv",		/* char *driver_name, the argument to open_port */
   NULL,			/* F_PTR finish, called when unloaded */
   NULL,			/* handle */
   stringprep_erl_control,	/* F_PTR control, port_command callback */
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

DRIVER_INIT(stringprep_erl) /* must match name in driver_entry */
{
    return &stringprep_driver_entry;
}

