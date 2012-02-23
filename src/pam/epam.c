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

#include <security/pam_appl.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <erl_interface.h>
#include <ei.h>
#include <unistd.h>

#define dec_int16(s) ((((unsigned char*)  (s))[0] << 8) | \
                      (((unsigned char*)  (s))[1]))

#define enc_int16(i, s) {((unsigned char*)(s))[0] = ((i) >> 8) & 0xff; \
                        ((unsigned char*)(s))[1] = (i)         & 0xff;}

#define BUFSIZE (1 << 16)
#define CMD_AUTH 0
#define CMD_ACCT 1

typedef unsigned char byte;

#ifdef PAM_FAIL_DELAY
static void delay_fn(int retval, unsigned usec_delay, void *appdata_ptr)
{
  /* No delay. However, looks like some PAM modules ignore this */
}
#endif

static int misc_conv(int num_msg,
		     const struct pam_message **msg,
		     struct pam_response **resp,
		     void *password)
{
  int msg_style;
  if (num_msg != 1)
    return PAM_CONV_ERR;
  msg_style = msg[0]->msg_style;
  if ((msg_style != PAM_PROMPT_ECHO_OFF) && (msg_style != PAM_PROMPT_ECHO_ON))
    return PAM_CONV_ERR;
  *resp = malloc(sizeof(struct pam_response));
  (*resp)[0].resp_retcode = 0;
  (*resp)[0].resp = strdup(password);
  return PAM_SUCCESS;
}

static int auth(char *service, char *user, char *password)
{
  struct pam_conv conv = {misc_conv, password};
  int retval;
  pam_handle_t *pamh = NULL;
  retval = pam_start(service, user, &conv, &pamh);
  if (retval == PAM_SUCCESS)
    retval = pam_set_item(pamh, PAM_RUSER, user);
#ifdef PAM_FAIL_DELAY
  if (retval == PAM_SUCCESS)
    retval = pam_set_item(pamh, PAM_FAIL_DELAY, (void *)delay_fn);
#endif
  if (retval == PAM_SUCCESS)
    retval = pam_authenticate(pamh, 0);
  if (retval == PAM_SUCCESS)
    retval = pam_acct_mgmt(pamh, 0);
  pam_end(pamh, retval);
  return retval;
}

static int acct_mgmt(char *service, char *user)
{
  struct pam_conv conv = {misc_conv, NULL};
  int retval;
  pam_handle_t *pamh = NULL;
  retval = pam_start(service, user, &conv, &pamh);
  if (retval == PAM_SUCCESS)
    retval = pam_set_item(pamh, PAM_RUSER, user);
#ifdef PAM_FAIL_DELAY
  if (retval == PAM_SUCCESS)
    retval = pam_set_item(pamh, PAM_FAIL_DELAY, (void *)delay_fn);
#endif
  if (retval == PAM_SUCCESS)
    retval = pam_acct_mgmt(pamh, 0);
  pam_end(pamh, retval);
  return retval;
}

static int read_buf(int fd, byte *buf, int len)
{
  int i, got = 0;
  do {
    if ((i = read(fd, buf+got, len-got)) <= 0) {
      if (i == 0) return got;
      if (errno != EINTR)
	return got;
      i = 0;
    }
    got += i;
  } while (got < len);
  return (len);
}

static int read_cmd(byte *buf)
{
  int len;
  if (read_buf(0, buf, 2) != 2)
    return 0;
  len = dec_int16(buf);
  if (read_buf(0, buf, len) != len)
    return 0;
  return 1;
}

static int write_buf(int fd, char *buf, int len)
{
  int i, done = 0; 
  do {
    if ((i = write(fd, buf+done, len-done)) < 0) {
      if (errno != EINTR)
	return (i);
      i = 0;
    }
    done += i;
  } while (done < len);
  return (len);
}

static int write_cmd(char *buf, int len)
{
  byte hd[2];
  enc_int16(len, hd);
  if (write_buf(1, (char *)hd, 2) != 2)
    return 0;
  if (write_buf(1, buf, len) != len)
    return 0;
  return 1;
}

static int process_reply(ETERM *pid, int cmd, int res)
{
  ETERM *result;
  int len, retval;
  const char *errtxt;
  byte *buf;
  if (res == PAM_SUCCESS)
    result = erl_format("{~i, ~w, true}", cmd, pid);
  else
    {
      errtxt = pam_strerror(NULL, res);
      result = erl_format("{~i, ~w, {false, ~s}}", cmd, pid, errtxt);
    }
  len = erl_term_len(result);
  buf = erl_malloc(len);
  erl_encode(result, buf);
  retval = write_cmd((char *)buf, len);
  erl_free_term(result);
  erl_free(buf);
  return retval;
}

static int process_acct(ETERM *pid, ETERM *data)
{
  int retval = 0;
  ETERM *pattern, *srv, *user;
  char *service, *username;
  pattern = erl_format("{Srv, User}");
  if (erl_match(pattern, data))
    {
      srv = erl_var_content(pattern, "Srv");
      service = erl_iolist_to_string(srv);
      user = erl_var_content(pattern, "User");
      username = erl_iolist_to_string(user);
      retval = process_reply(pid, CMD_ACCT, acct_mgmt(service, username));
      erl_free_term(srv);
      erl_free_term(user);
      erl_free(service);
      erl_free(username);
    }
  erl_free_term(pattern);
  return retval;
}

static int process_auth(ETERM *pid, ETERM *data)
{
  int retval = 0;
  ETERM *pattern, *srv, *user, *pass;
  char *service, *username, *password;
  pattern = erl_format("{Srv, User, Pass}");
  if (erl_match(pattern, data))
    {
      srv = erl_var_content(pattern, "Srv");
      service = erl_iolist_to_string(srv);
      user = erl_var_content(pattern, "User");
      username = erl_iolist_to_string(user);
      pass = erl_var_content(pattern, "Pass");
      password = erl_iolist_to_string(pass);
      retval = process_reply(pid, CMD_AUTH, auth(service, username, password));
      erl_free_term(srv);
      erl_free_term(user);
      erl_free_term(pass);
      erl_free(service);
      erl_free(username);
      erl_free(password);
    };
  erl_free_term(pattern);
  return retval;
}

static int process_command(byte *buf)
{
  int retval = 0;
  ETERM *pattern, *tuple, *cmd, *port, *data;
  pattern = erl_format("{Cmd, Port, Data}");
  tuple = erl_decode(buf);
  if (erl_match(pattern, tuple))
    {
      cmd = erl_var_content(pattern, "Cmd");
      port = erl_var_content(pattern, "Port");
      data = erl_var_content(pattern, "Data");
      switch (ERL_INT_VALUE(cmd))
	{
	case CMD_AUTH:
	  retval = process_auth(port, data);
	  break;
	case CMD_ACCT:
	  retval = process_acct(port, data);
	  break;
	};
      erl_free_term(cmd);
      erl_free_term(port);
      erl_free_term(data);
    }
  erl_free_term(pattern);
  erl_free_term(tuple);
  return retval;
}

static void loop(void)
{
  byte buf[BUFSIZE];
  int retval = 0;
  do {
    if (read_cmd(buf) > 0)
      retval = process_command(buf);
    else
      retval = 0;
  } while (retval);
}

int main(int argc, char *argv[])
{
  erl_init(NULL, 0);
  loop();
  return 0;
}
