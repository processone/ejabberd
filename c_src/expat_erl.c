/*
 * ejabberd, Copyright (C) 2002-2013   ProcessOne
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
#include <expat.h>


#define XML_START 0
#define XML_END   1
#define XML_CDATA 2
#define XML_ERROR 3

#define PARSE_COMMAND 0
#define PARSE_FINAL_COMMAND 1

/*
 * R15B changed several driver callbacks to use ErlDrvSizeT and
 * ErlDrvSSizeT typedefs instead of int.
 * This provides missing typedefs on older OTP versions.
 */
#if ERL_DRV_EXTENDED_MAJOR_VERSION < 2
typedef int ErlDrvSizeT;
typedef int ErlDrvSSizeT;
#endif

ei_x_buff event_buf;
ei_x_buff xmlns_buf;

typedef struct {
      ErlDrvPort port;
      XML_Parser parser;
} expat_data;

static XML_Memory_Handling_Suite ms;

void encode_name(const XML_Char *name)
{
  char *name_start;
  char *prefix_start;
  char *buf;
  int name_len, prefix_len, buf_len;

  if ((name_start = strchr(name, '\n'))) {
    if ((prefix_start = strchr(name_start+1, '\n'))) {
      name_len = prefix_start - name_start;
      prefix_len = strlen(prefix_start+1);
      buf_len = prefix_len + name_len;
      buf = driver_alloc(buf_len);
      memcpy(buf, prefix_start+1, prefix_len);
      memcpy(buf+prefix_len, name_start, name_len);
      buf[prefix_len] = ':';
      ei_x_encode_binary(&event_buf, buf, buf_len);
      driver_free(buf);
    } else {
       ei_x_encode_binary(&event_buf, name_start+1, strlen(name_start+1));
    };
  } else {
     ei_x_encode_binary(&event_buf, name, strlen(name));
  }
}

void *erlXML_StartElementHandler(expat_data *d,
				 const XML_Char *name,
				 const XML_Char **atts)
{
   int i;

   ei_x_encode_list_header(&event_buf, 1);
   ei_x_encode_tuple_header(&event_buf, 2);
   ei_x_encode_long(&event_buf, XML_START);
   ei_x_encode_tuple_header(&event_buf, 2);
   encode_name(name);
   ei_x_append(&event_buf, &xmlns_buf);
   ei_x_free(&xmlns_buf);
   ei_x_new(&xmlns_buf);

   for (i = 0; atts[i]; i += 2) {}

   if (i > 0)
   {
      ei_x_encode_list_header(&event_buf, i/2);

      for (i = 0; atts[i]; i += 2)
      {
	 ei_x_encode_tuple_header(&event_buf, 2);
	 encode_name(atts[i]);
	 ei_x_encode_binary(&event_buf, atts[i+1], strlen(atts[i+1]));
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
   encode_name(name);
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

void *erlXML_StartNamespaceDeclHandler(expat_data *d,
				       const XML_Char *prefix,
				       const XML_Char *uri)
{
  int prefix_len;
  char *buf;

  /* From the expat documentation:
     "For a default namespace declaration (xmlns='...'),
     the prefix will be null ...
     ... The URI will be null for the case where
     the default namespace is being unset."

     FIXME: I'm not quite sure what all that means */
  if (uri == NULL)
      return NULL;

  ei_x_encode_list_header(&xmlns_buf, 1);
  ei_x_encode_tuple_header(&xmlns_buf, 2);
  if (prefix) {
    prefix_len = strlen(prefix);
    buf = driver_alloc(7 + prefix_len);
    strcpy(buf, "xmlns:");
    strcpy(buf+6, prefix);
    ei_x_encode_binary(&xmlns_buf, buf, strlen(buf));
    driver_free(buf);
  } else {
     ei_x_encode_binary(&xmlns_buf, "xmlns", strlen("xmlns"));
  };
  ei_x_encode_binary(&xmlns_buf, uri, strlen(uri));

  return NULL;
}

static ErlDrvData expat_erl_start(ErlDrvPort port, char *buff)
{
   expat_data* d = (expat_data*)driver_alloc(sizeof(expat_data));
   d->port = port;
   d->parser = XML_ParserCreate_MM("UTF-8", &ms, "\n");
   XML_SetUserData(d->parser, d);

   set_port_control_flags(port, PORT_CONTROL_FLAG_BINARY);

   XML_SetStartElementHandler(
      d->parser, (XML_StartElementHandler)erlXML_StartElementHandler);
   XML_SetEndElementHandler(
      d->parser, (XML_EndElementHandler)erlXML_EndElementHandler);
   XML_SetCharacterDataHandler(
      d->parser, (XML_CharacterDataHandler)erlXML_CharacterDataHandler);

   XML_SetStartNamespaceDeclHandler(
      d->parser, (XML_StartNamespaceDeclHandler) erlXML_StartNamespaceDeclHandler);
   XML_SetReturnNSTriplet(d->parser, 1);

   XML_SetDefaultHandler(d->parser, NULL);

   return (ErlDrvData)d;
}

static void expat_erl_stop(ErlDrvData handle)
{
   XML_ParserFree(((expat_data *)handle)->parser);
   driver_free((char*)handle);
}

static ErlDrvSSizeT expat_erl_control(ErlDrvData drv_data,
			     unsigned int command,
			     char *buf, ErlDrvSizeT len,
			     char **rbuf, ErlDrvSizeT rlen)
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
	 ei_x_new(&xmlns_buf);
#ifdef ENABLE_FLASH_HACK
        /* Flash hack - Flash clients send a null byte after the stanza.  Remove that... */
        {
           int i;
           int found_null = 0;

           /* Maybe the Flash client sent many stanzas in one packet.
              If so, there is a null byte between every stanza. */
           for (i = 0; i < len; i++) {
              if (buf[i] == '\0') {
                 buf[i] = ' ';
                 found_null = 1;
              }
           }

           /* And also remove the closing slash if this is a
              flash:stream element.  Assume that flash:stream is the
              last element in the packet, and entirely contained in
              it.  This requires that a null byte has been found. */
           if (found_null && strstr(buf, "<flash:stream"))
              /* buf[len - 1] is an erased null byte.
                 buf[len - 2] is >
                 buf[len - 3] is / (maybe)
              */
              if (buf[len - 3] == '/')
                 buf[len - 3] = ' ';
        }
#endif /* ENABLE_FLASH_HACK */

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
	    ei_x_encode_binary(&event_buf, errstring, strlen(errstring));
	 }

	 ei_x_encode_empty_list(&event_buf);

	 size = event_buf.index;

	 b = driver_alloc_binary(size);
	 memcpy(b->orig_bytes, event_buf.buff, size);

	 ei_x_free(&event_buf);
	 ei_x_free(&xmlns_buf);
 
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

DRIVER_INIT(expat_erl) /* must match name in driver_entry */
{
    ms.malloc_fcn = driver_alloc;
    ms.realloc_fcn = driver_realloc;
    ms.free_fcn = driver_free;
    return &expat_driver_entry;
}


