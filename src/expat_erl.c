/* $Id$ */

#include <stdio.h>
#include <erl_driver.h>
#include <ei.h>
#include <expat.h>

typedef struct {
      ErlDrvPort port;
      XML_Parser parser;
} expat_data;


void *erlXML_StartElementHandler(expat_data *d,
				 const XML_Char *name,
				 const XML_Char **atts)
{
   int i;
   ei_x_buff buf;
   
   ei_x_new_with_version(&buf);
   ei_x_encode_tuple_header(&buf, 2);
   ei_x_encode_atom(&buf, "xmlstart");
   ei_x_encode_tuple_header(&buf, 2);
   ei_x_encode_string(&buf, name);
   
   for (i = 0; atts[i]; i += 2) {}

   ei_x_encode_list_header(&buf, i/2);

   for (i = 0; atts[i]; i += 2)
   {
      ei_x_encode_tuple_header(&buf, 2);
      ei_x_encode_string(&buf, atts[i]);
      ei_x_encode_string(&buf, atts[i+1]);
   }
   
   ei_x_encode_empty_list(&buf);
   
   driver_output(d->port, buf.buff, buf.index);
   ei_x_free(&buf);
   return NULL;
}

void *erlXML_EndElementHandler(expat_data *d,
			       const XML_Char *name)
{
   ei_x_buff buf;
   
   ei_x_new_with_version(&buf);
   ei_x_encode_tuple_header(&buf, 2);
   ei_x_encode_atom(&buf, "xmlend");
   ei_x_encode_string(&buf, name);

   driver_output(d->port, buf.buff, buf.index);
   ei_x_free(&buf);
   return NULL;
}

void *erlXML_CharacterDataHandler(expat_data *d,
				  const XML_Char *s,
				  int len)
{
   ei_x_buff buf;
   
   ei_x_new_with_version(&buf);
   ei_x_encode_tuple_header(&buf, 2);
   ei_x_encode_atom(&buf, "xmlcdata");
   ei_x_encode_string_len(&buf, s, len);

   driver_output(d->port, buf.buff, buf.index);
   ei_x_free(&buf);
   return NULL;
}


static ErlDrvData expat_erl_start(ErlDrvPort port, char *buff)
{
   expat_data* d = (expat_data*)driver_alloc(sizeof(expat_data));
   d->port = port;
   d->parser = XML_ParserCreate("UTF-8");
   XML_SetUserData(d->parser, d);

   XML_SetStartElementHandler(
      d->parser, (XML_StartElementHandler)erlXML_StartElementHandler);
   XML_SetEndElementHandler(
      d->parser, (XML_EndElementHandler)erlXML_EndElementHandler);
   XML_SetCharacterDataHandler(
      d->parser, (XML_CharacterDataHandler)erlXML_CharacterDataHandler);


   return (ErlDrvData)d;
}

static void expat_erl_stop(ErlDrvData handle)
{
   /* TODO: free parser */
   driver_free((char*)handle);
}

static void expat_erl_output(ErlDrvData handle, char *buff, int bufflen)
{
   expat_data* d = (expat_data*)handle;
   int res, errcode;
   char *errstring;
   ei_x_buff buf;

   /*buff[bufflen] = 0;
   
   fprintf(stderr, "RCVD: '%s'\n", buff);
   fflush(stderr);*/

   res = XML_Parse(d->parser, buff, bufflen, 0);

   if(!res)
   {
      errcode = XML_GetErrorCode(d->parser);
      errstring = (char *)XML_ErrorString(errcode);

      ei_x_new_with_version(&buf);
      ei_x_encode_tuple_header(&buf, 2);
      ei_x_encode_atom(&buf, "xmlerror");
      ei_x_encode_tuple_header(&buf, 2);
      ei_x_encode_long(&buf, errcode);
      ei_x_encode_string(&buf, errstring);

      driver_output(d->port, buf.buff, buf.index);
      ei_x_free(&buf);
   }
   
   //driver_output(d->port, &res, 1);
}



ErlDrvEntry expat_driver_entry = {
   NULL,                       /* F_PTR init, N/A */
   expat_erl_start,          /* L_PTR start, called when port is opened */
   expat_erl_stop,           /* F_PTR stop, called when port is closed */
   expat_erl_output,         /* F_PTR output, called when erlang has sent */
   NULL,                       /* F_PTR ready_input, called when input descriptor ready */
   NULL,                       /* F_PTR ready_output, called when output descriptor ready */
   "expat_erl",              /* char *driver_name, the argument to open_port */
   NULL,                       /* F_PTR finish, called when unloaded */
   NULL,                       /* F_PTR control, port_command callback */
   NULL,                       /* F_PTR timeout, reserved */
   NULL                        /* F_PTR outputv, reserved */
};

DRIVER_INIT(expat_erl) /* must match name in driver_entry */
{
   return &expat_driver_entry;
}


