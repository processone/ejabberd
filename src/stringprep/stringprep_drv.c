/* $Id$ */

#include <stdio.h>
#include <erl_driver.h>
#include <ei.h>
#include <iconv.h>

#include "uni_data.c"
#include "uni_norm.c"

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


static int combine(int ch1, int ch2)
{
   int info1, info2;

   info1 = GetUniCharCompInfo(ch1);
   if(info1 != -1) {
      if(info1 & CompSingleMask) {
	 if (ch2 == compFirstList[info1 & CompMask][0]) {
	    return compFirstList[info1 & CompMask][1];
	 } else
	    return 0;
      }
   } else
      return 0;
   
   info2 = GetUniCharCompInfo(ch2);
   if(info2 != -1) {
      if (info2 & CompSingleMask) {
	 if (ch1 == compSecondList[info2 & CompMask][0]) {
	    return compSecondList[info2 & CompMask][1];
	 } else
	    return 0;
      }
   } else
      return 0;

   return compBothList[info1][info2];
}


#define ADD_UCHAR(ruc)							\
	 if(ruc < 0x80) {						\
	    if(pos >= size) {						\
	       size = 2*size + 1;					\
	       rstring = driver_realloc(rstring, size);			\
	    }								\
	    rstring[pos] = (char) ruc;					\
	    pos++;							\
	 } else if(ruc < 0x7FF) {					\
	    if(pos + 1 >= size) {					\
	       size = 2*size + 2;					\
	       rstring = driver_realloc(rstring, size);			\
	    }								\
	    rstring[pos] = (char) ((ruc >> 6) | 0xC0);			\
	    rstring[pos+1] = (char) ((ruc | 0x80) & 0xBF);		\
	    pos += 2;							\
	 } else if(ruc < 0xFFFF) {					\
	    if(pos + 2 >= size) {					\
	       size = 2*size + 3;					\
	       rstring = driver_realloc(rstring, size);			\
	    }								\
	    rstring[pos] = (char) ((ruc >> 12) | 0xE0);			\
	    rstring[pos+1] = (char) (((ruc >> 6) | 0x80) & 0xBF);	\
	    rstring[pos+2] = (char) ((ruc | 0x80) & 0xBF);		\
	    pos += 3;							\
	 }


static int stringprep_erl_control(ErlDrvData drv_data,
				  unsigned int command,
				  char *buf, int len,
				  char **rbuf, int rlen)
{
   int i, j, pos=1;
   unsigned char c;
   int bad = 0;
   int uc, ruc;
   int size;
   int info;
   int prohibit, tolower;
   char *rstring;
   int *mc;
   
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
	    if(!(info & MCMask)) 
	    {
	       ruc = uc + GetDelta(info);

	       //info = GetUniCharDecompInfo(ruc);
	       //if(info >= 0) {
	       //   printf("Decomposition %x: ", ruc);
	       //   for(j = 0; j < GetDecompLen(info); j++) {
	       //      printf("%x ", decompList[GetDecompShift(info) + j]);
	       //   }
	       //   printf("\r\n");
	       //}
	       
	       ADD_UCHAR(ruc);
	    } else {
	       mc = GetMC(info);
	       for(j = 1; j <= mc[0]; j++) {
		  ruc = mc[j];
		  //printf("Char %x cclass %d\r\n", ruc, GetUniCharCClass(ruc));
		  ADD_UCHAR(ruc);
	       }
	    }
	 } else {
	    ruc = uc;
	    ADD_UCHAR(ruc);
	 }
      }
   }

   //printf("Combine: %x\r\n", combine(0x438, 0x301));

   rstring[0] = 1;
   *rbuf = rstring;
   
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
   NULL				/* F_PTR outputv, reserved */
};

#ifdef WIN32
__declspec(dllexport)
#endif
DRIVER_INIT(stringprep_erl) /* must match name in driver_entry */
{
   return &stringprep_driver_entry;
}

