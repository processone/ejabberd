/* $Id$ */

#include <stdio.h>
#include <erl_driver.h>
#include <ei.h>
#include <iconv.h>

#include "uni_data.c"

#define NAMEPREP_COMMAND 1
#define NODEPREP_COMMAND 2
#define RESOURCEPREP_COMMAND 3

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

static int stringprep_erl_control(ErlDrvData drv_data,
				  unsigned int command,
				  char *buf, int len,
				  char **rbuf, int rlen)
{
   int i, j=1;
   unsigned char c;
   int bad = 0;
   int uc, ruc;
   int size;
   int info;
   int prohibit, tolower;
   char *rstring;

   size = len + 1;

   rstring = driver_alloc(size);
   rstring[0] = 0;

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

   for(i=0; i < len; i++)
   {
      c = buf[i];
      if(c < 0x80) {
	 uc = c;
      } else if(c < 0xC0) {
	 bad = 1;
      } else if(c < 0xE0) {
	 if(i+1 < len && (buf[i+1] & 0xC0) == 0x80) {
	    uc = ((c & 0x1F) << 6) | (buf[i+1] & 0x3F);
	    i++;
	 } else {
	    bad = 1;
	 }
      } else if(c < 0xF0) {
	 if(i+2 < len && (buf[i+1] & 0xC0) == 0x80 &&
	    (buf[i+2] & 0xC0) == 0x80) {
	    uc = ((c & 0x1F) << 12) | ((buf[i+1] & 0x1F) << 6)
	       | (buf[i+2] & 0x3F);
	    i += 2;
	 } else {
	    bad = 1;
	 }
      } else {
	 // TODO
	 bad = 1;
      }

      if(bad) {
	 *rbuf = rstring;
	 return 1;
      }
      
      info = GetUniCharInfo(uc);
      if(info & prohibit) {
	 *rbuf = rstring;
	 return 1;
      }


      if(!(info & B1Mask)) 
      {
	 if(tolower) {
	    ruc = uc + GetDelta(info);
	 } else {
	    ruc = uc;
	 }

	 if(ruc < 0x80) {
	    if(j >= size) {
	       size = 2*size + 1;
	       rstring = driver_realloc(rstring, size);
	    }
	    rstring[j] = (char) ruc;
	    j++;
	 } else if(ruc < 0x7FF) {
	    if(j + 1 >= size) {
	       size = 2*size + 2;
	       rstring = driver_realloc(rstring, size);
	    }
	    rstring[j] = (char) ((ruc >> 6) | 0xC0);
	    rstring[j+1] = (char) ((ruc | 0x80) & 0xBF);
	    j += 2;
	 } else if(ruc < 0xFFFF) {
	    if(j + 2 >= size) {
	       size = 2*size + 3;
	       rstring = driver_realloc(rstring, size);
	    }
	    rstring[j] = (char) ((ruc >> 12) | 0xE0);
	    rstring[j+1] = (char) (((ruc >> 6) | 0x80) & 0xBF);
	    rstring[j+2] = (char) ((ruc | 0x80) & 0xBF);
	    j += 3;
	 }
      }
   }
   
   rstring[0] = 1;
   *rbuf = rstring;
   
   return j;
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
   NULL				/* F_PTR outputv, reserved */
};

#ifdef WIN32
__declspec(dllexport)
#endif
DRIVER_INIT(stringprep_erl) /* must match name in driver_entry */
{
   return &stringprep_driver_entry;
}

