#include <erl_nif.h>
#include <string.h>
#include <stdio.h>

struct buf {
  int limit;
  int len;
  unsigned char *b;
};

static int make_element(ErlNifEnv* env, struct buf *rbuf, ERL_NIF_TERM el);

static struct buf *init_buf(ErlNifEnv* env)
{
  struct buf *rbuf = enif_alloc(env, sizeof(struct buf));
  rbuf->limit = 1024;
  rbuf->len = 0;
  rbuf->b = enif_alloc(env, rbuf->limit);
  return rbuf;
}

static void destroy_buf(ErlNifEnv* env, struct buf *rbuf)
{
  if (rbuf) {
    if (rbuf->b) {
      enif_free(env, rbuf->b);
    };
    enif_free(env, rbuf);
  };
}

inline void resize_buf(ErlNifEnv* env, struct buf *rbuf, int len_to_add)
{
  int new_len = rbuf->len + len_to_add;
  
  if (new_len >= rbuf->limit) {
    rbuf->limit = ((new_len / 1024) + 1) * 1024;
    rbuf->b = enif_realloc(env, rbuf->b, rbuf->limit);
  };
}

static void buf_add_char(ErlNifEnv* env, struct buf *rbuf, unsigned char c)
{
  resize_buf(env, rbuf, 1);
  (rbuf->b)[rbuf->len] = c;
  rbuf->len += 1;
}

static void buf_add_str(ErlNifEnv* env, struct buf *rbuf, char *data, int len)
{
  resize_buf(env, rbuf, len);
  memcpy(rbuf->b + rbuf->len, data, len);
  rbuf->len += len;
}

inline void crypt(ErlNifEnv* env, struct buf *rbuf, unsigned char *data, int len)
{
  int i;

  for (i = 0; i < len; i++) {
    switch (data[i]) {
    case '&':
      buf_add_str(env, rbuf, "&amp;", 5);
      break;
    case '<':
      buf_add_str(env, rbuf, "&lt;", 4);
      break;
    case '>':
      buf_add_str(env, rbuf, "&gt;", 4);
      break;
    case '"':
      buf_add_str(env, rbuf, "&quot;", 6);
      break;
    case '\'':
      buf_add_str(env, rbuf, "&apos;", 6);
      break;
    default:
      buf_add_char(env, rbuf, data[i]);
      break;
    };
  };
}

static int make_elements(ErlNifEnv* env, struct buf *rbuf, ERL_NIF_TERM els)
{
  ERL_NIF_TERM head, tail;
  int ret = 0;

  while (enif_get_list_cell(env, els, &head, &tail)) {
    ret = make_element(env, rbuf, head);
    if (ret) {
      els = tail;
    } else {
      break;
    };
  };

  return ret;
}

static int make_attrs(ErlNifEnv* env, struct buf *rbuf, ERL_NIF_TERM attrs)
{
  ErlNifBinary name, data;
  ERL_NIF_TERM head, tail;
  const ERL_NIF_TERM *tuple;
  int arity, ret = 1;
  
  while (enif_get_list_cell(env, attrs, &head, &tail)) {
    if (enif_get_tuple(env, head, &arity, &tuple)) {
      if (arity == 2) {
	if (enif_inspect_iolist_as_binary(env, tuple[0], &name) &&
	    enif_inspect_iolist_as_binary(env, tuple[1], &data)) {
	  buf_add_char(env, rbuf, ' ');
	  buf_add_str(env, rbuf, (char *)name.data, name.size);
	  buf_add_str(env, rbuf, "='", 2);
	  crypt(env, rbuf, data.data, data.size);
	  buf_add_char(env, rbuf, '\'');
	  attrs = tail;
	} else {
	  ret = 0;
	  break;
	};
      } else {
	ret = 0;
	break;
      };
    } else {
      ret = 0;
      break;
    };
  };
  
  return ret;
}

static int make_element(ErlNifEnv* env, struct buf *rbuf, ERL_NIF_TERM el)
{
  ErlNifBinary cdata, name;
  const ERL_NIF_TERM *tuple;
  int arity, ret = 0;

  if (enif_get_tuple(env, el, &arity, &tuple)) {
    if (arity == 2) {
      if (!enif_compare(env, tuple[0], enif_make_atom(env, "xmlcdata"))) {
	if (enif_inspect_iolist_as_binary(env, tuple[1], &cdata)) {
	  crypt(env, rbuf, cdata.data, cdata.size);
	  ret = 1;
	};
      };
    };
    if (arity == 4) {
      if (!enif_compare(env, tuple[0], enif_make_atom(env, "xmlelement"))) {
	if (enif_inspect_iolist_as_binary(env, tuple[1], &name)) {
	  buf_add_char(env, rbuf, '<');
	  buf_add_str(env, rbuf, (char *)name.data, name.size);
	  ret = make_attrs(env, rbuf, tuple[2]);
	  if (ret) {
	    if (enif_is_empty_list(env, tuple[3])) {
	      buf_add_str(env, rbuf, "/>", 2);
	    } else {
	      buf_add_char(env, rbuf, '>');
	      ret = make_elements(env, rbuf, tuple[3]);
	      if (ret) {
		buf_add_str(env, rbuf, "</", 2);
		buf_add_str(env, rbuf, (char*)name.data, name.size);
		buf_add_char(env, rbuf, '>');
	      };
	    };
	  };
	};
      };
    };
  };
  
  return ret;
}

static ERL_NIF_TERM element_to(ErlNifEnv* env, int argc,
			       const ERL_NIF_TERM argv[],
			       int as_string)
{
  ErlNifBinary output;
  ERL_NIF_TERM result;
  struct buf *rbuf;

  if (argc == 1) {
    rbuf = init_buf(env);
    if (make_element(env, rbuf, argv[0])) {
      if (as_string) {
	(rbuf->b)[rbuf->len] = 0;
	result = enif_make_string(env, (char *) rbuf->b, ERL_NIF_LATIN1);
	destroy_buf(env, rbuf);
	return result;
      }	else {
	if (enif_alloc_binary(env, rbuf->len, &output)) {
	  memcpy(output.data, rbuf->b, rbuf->len);
	  result = enif_make_binary(env, &output);
	  destroy_buf(env, rbuf);
	  return result;
	};
      };
    };
    destroy_buf(env, rbuf);
  };
  
  return enif_make_badarg(env);
}

/* static ERL_NIF_TERM element_to_string(ErlNifEnv* env, int argc, */
/* 				      const ERL_NIF_TERM argv[]) */
/* { */
/*   return element_to(env, argc, argv, 1); */
/* } */

static ERL_NIF_TERM element_to_binary(ErlNifEnv* env, int argc,
				      const ERL_NIF_TERM argv[])
{
  return element_to(env, argc, argv, 0);
}

static ErlNifFunc nif_funcs[] =
  {
    /* Stupid Erlang bug with enif_make_string() is fixed
       in R14A only (OTP-8685), so we can't use
       element_to_string yet.*/
    /* {"element_to_string", 1, element_to_string}, */
    {"element_to_binary", 1, element_to_binary}
  };

ERL_NIF_INIT(xml, nif_funcs, NULL, NULL, NULL, NULL)
