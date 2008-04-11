/* $Id$ */

#include <stdio.h>
#include <string.h>
#include <erl_driver.h>
#include <ei.h>
#include <expat.h>


#define XML_START 0
#define XML_END   1
#define XML_CDATA 2
#define XML_ERROR 3

#define PARSE_COMMAND 0
#define PARSE_FINAL_COMMAND 1

ei_x_buff event_buf;

typedef struct {
      ErlDrvPort port;
      XML_Parser parser;
} expat_data;

void *erlXML_StartElementHandler(expat_data *d,
				 const XML_Char *name,
				 const XML_Char **atts)
{
   int i;

   ei_x_encode_list_header(&event_buf, 1);
   ei_x_encode_tuple_header(&event_buf, 2);
   ei_x_encode_long(&event_buf, XML_START);
   ei_x_encode_tuple_header(&event_buf, 2);
   ei_x_encode_string(&event_buf, name);

   for (i = 0; atts[i]; i += 2) {}

   if (i > 0)
   {
      ei_x_encode_list_header(&event_buf, i/2);

      for (i = 0; atts[i]; i += 2)
      {
	 ei_x_encode_tuple_header(&event_buf, 2);
	 ei_x_encode_string(&event_buf, atts[i]);
	 ei_x_encode_string(&event_buf, atts[i+1]);
      }
   }

   ei_x_encode_empty_list(&event_buf);

   return NULL;
}

void *erlXML_EndElementHandler(expat_data *d,
			       const XML_Char *name)
{
   ei_x_encode_list_header(&event_buf, 1);
   ei_x_encode_tuple_header(&event_buf, 2);
   ei_x_encode_long(&event_buf, XML_END);
   ei_x_encode_string(&event_buf, name);
   return NULL;
}

void *erlXML_CharacterDataHandler(expat_data *d,
				  const XML_Char *s,
				  int len)
{
   ei_x_encode_list_header(&event_buf, 1);
   ei_x_encode_tuple_header(&event_buf, 2);
   ei_x_encode_long(&event_buf, XML_CDATA);
   ei_x_encode_binary(&event_buf, s, len);
   return NULL;
}


static ErlDrvData expat_erl_start(ErlDrvPort port, char *buff)
{
   expat_data* d = (expat_data*)driver_alloc(sizeof(expat_data));
   d->port = port;
   d->parser = XML_ParserCreate("UTF-8");
   XML_SetUserData(d->parser, d);

   set_port_control_flags(port, PORT_CONTROL_FLAG_BINARY);

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
   XML_ParserFree(((expat_data *)handle)->parser);
   driver_free((char*)handle);
}

static int expat_erl_control(ErlDrvData drv_data,
			     unsigned int command,
			     char *buf, int len,
			     char **rbuf, int rlen)
{
   expat_data* d = (expat_data*)drv_data;
   int res, errcode;
   char *errstring;
   ErlDrvBinary *b;
   size_t size;

   switch (command)
   {
      case PARSE_COMMAND:
      case PARSE_FINAL_COMMAND:
	 ei_x_new_with_version(&event_buf);
	 res = XML_Parse(d->parser, buf, len, command == PARSE_FINAL_COMMAND);

	 if(!res)
	 {
	    errcode = XML_GetErrorCode(d->parser);
	    errstring = (char *)XML_ErrorString(errcode);

	    ei_x_encode_list_header(&event_buf, 1);
	    ei_x_encode_tuple_header(&event_buf, 2);
	    ei_x_encode_long(&event_buf, XML_ERROR);
	    ei_x_encode_tuple_header(&event_buf, 2);
	    ei_x_encode_long(&event_buf, errcode);
	    ei_x_encode_string(&event_buf, errstring);
	 }

	 ei_x_encode_empty_list(&event_buf);

	 size = event_buf.index;

	 b = driver_alloc_binary(size);
	 memcpy(b->orig_bytes, event_buf.buff, size);

	 ei_x_free(&event_buf);
 
	 *rbuf = (char *)b;
	 return size;
      default:
	 return 0;
   }
}

ErlDrvEntry expat_driver_entry = {
   NULL,			/* F_PTR init, N/A */
   expat_erl_start,		/* L_PTR start, called when port is opened */
   expat_erl_stop,		/* F_PTR stop, called when port is closed */
   NULL,			/* F_PTR output, called when erlang has sent */
   NULL,			/* F_PTR ready_input, called when input descriptor ready */
   NULL,			/* F_PTR ready_output, called when output descriptor ready */
   "expat_erl",			/* char *driver_name, the argument to open_port */
   NULL,			/* F_PTR finish, called when unloaded */
   NULL,			/* handle */
   expat_erl_control,		/* F_PTR control, port_command callback */
   NULL,			/* F_PTR timeout, reserved */
   NULL				/* F_PTR outputv, reserved */
};

DRIVER_INIT(expat_erl) /* must match name in driver_entry */
{
    return &expat_driver_entry;
}


