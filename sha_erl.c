/* 
 * The contents of this file are subject to the Mozilla Public
 * License Version 1.1 (the "License"); you may not use this file
 * except in compliance with the License. You may obtain a copy of
 * the License at http://www.mozilla.org/MPL/
 * 
 * Software distributed under the License is distributed on an "AS
 * IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or
 * implied. See the License for the specific language governing
 * rights and limitations under the License.
 * 
 * The Original Code is SHA 180-1 Reference Implementation (Compact version)
 * 
 * The Initial Developer of the Original Code is Paul Kocher of
 * Cryptography Research.  Portions created by Paul Kocher are 
 * Copyright (C) 1995-9 by Cryptography Research, Inc.  All
 * Rights Reserved.
 * 
 * Contributor(s):
 *
 */

#include <stdio.h>
#include <erl_driver.h>
#include <ei.h>

typedef struct {
      ErlDrvPort port;
} sha_data;

typedef struct {
  unsigned long H[5];
  unsigned long W[80];
  int lenW;
  unsigned long sizeHi,sizeLo;
} j_SHA_CTX;

static void shaHashBlock(j_SHA_CTX *ctx);

void shaInit(j_SHA_CTX *ctx) {
  int i;

  ctx->lenW = 0;
  ctx->sizeHi = ctx->sizeLo = 0;

  /* Initialize H with the magic constants (see FIPS180 for constants)
   */
  ctx->H[0] = 0x67452301L;
  ctx->H[1] = 0xefcdab89L;
  ctx->H[2] = 0x98badcfeL;
  ctx->H[3] = 0x10325476L;
  ctx->H[4] = 0xc3d2e1f0L;

  for (i = 0; i < 80; i++)
    ctx->W[i] = 0;
}


void shaUpdate(j_SHA_CTX *ctx, unsigned char *dataIn, int len) {
  int i;

  /* Read the data into W and process blocks as they get full
   */
  for (i = 0; i < len; i++) {
    ctx->W[ctx->lenW / 4] <<= 8;
    ctx->W[ctx->lenW / 4] |= (unsigned long)dataIn[i];
    if ((++ctx->lenW) % 64 == 0) {
      shaHashBlock(ctx);
      ctx->lenW = 0;
    }
    ctx->sizeLo += 8;
    ctx->sizeHi += (ctx->sizeLo < 8);
  }
}


void shaFinal(j_SHA_CTX *ctx, unsigned char hashout[20]) {
  unsigned char pad0x80 = 0x80;
  unsigned char pad0x00 = 0x00;
  unsigned char padlen[8];
  int i;

  /* Pad with a binary 1 (e.g. 0x80), then zeroes, then length
   */
  padlen[0] = (unsigned char)((ctx->sizeHi >> 24) & 255);
  padlen[1] = (unsigned char)((ctx->sizeHi >> 16) & 255);
  padlen[2] = (unsigned char)((ctx->sizeHi >> 8) & 255);
  padlen[3] = (unsigned char)((ctx->sizeHi >> 0) & 255);
  padlen[4] = (unsigned char)((ctx->sizeLo >> 24) & 255);
  padlen[5] = (unsigned char)((ctx->sizeLo >> 16) & 255);
  padlen[6] = (unsigned char)((ctx->sizeLo >> 8) & 255);
  padlen[7] = (unsigned char)((ctx->sizeLo >> 0) & 255);
  shaUpdate(ctx, &pad0x80, 1);
  while (ctx->lenW != 56)
    shaUpdate(ctx, &pad0x00, 1);
  shaUpdate(ctx, padlen, 8);

  /* Output hash
   */
  for (i = 0; i < 20; i++) {
    hashout[i] = (unsigned char)(ctx->H[i / 4] >> 24);
    ctx->H[i / 4] <<= 8;
  }

  /*
   *  Re-initialize the context (also zeroizes contents)
   */
  shaInit(ctx); 
}


void shaBlock(unsigned char *dataIn, int len, unsigned char hashout[20]) {
  j_SHA_CTX ctx;

  shaInit(&ctx);
  shaUpdate(&ctx, dataIn, len);
  shaFinal(&ctx, hashout);
}


#define SHA_ROTL(X,n) ((((X) << (n)) | ((X) >> (32-(n)))) & 0xffffffffL)

static void shaHashBlock(j_SHA_CTX *ctx) {
  int t;
  unsigned long A,B,C,D,E,TEMP;

  for (t = 16; t <= 79; t++)
    ctx->W[t] =
      SHA_ROTL(ctx->W[t-3] ^ ctx->W[t-8] ^ ctx->W[t-14] ^ ctx->W[t-16], 1);

  A = ctx->H[0];
  B = ctx->H[1];
  C = ctx->H[2];
  D = ctx->H[3];
  E = ctx->H[4];

  for (t = 0; t <= 19; t++) {
    TEMP = (SHA_ROTL(A,5) + (((C^D)&B)^D)     + E + ctx->W[t] + 0x5a827999L) & 0xffffffffL;
    E = D; D = C; C = SHA_ROTL(B, 30); B = A; A = TEMP;
  }
  for (t = 20; t <= 39; t++) {
    TEMP = (SHA_ROTL(A,5) + (B^C^D)           + E + ctx->W[t] + 0x6ed9eba1L) & 0xffffffffL;
    E = D; D = C; C = SHA_ROTL(B, 30); B = A; A = TEMP;
  }
  for (t = 40; t <= 59; t++) {
    TEMP = (SHA_ROTL(A,5) + ((B&C)|(D&(B|C))) + E + ctx->W[t] + 0x8f1bbcdcL) & 0xffffffffL;
    E = D; D = C; C = SHA_ROTL(B, 30); B = A; A = TEMP;
  }
  for (t = 60; t <= 79; t++) {
    TEMP = (SHA_ROTL(A,5) + (B^C^D)           + E + ctx->W[t] + 0xca62c1d6L) & 0xffffffffL;
    E = D; D = C; C = SHA_ROTL(B, 30); B = A; A = TEMP;
  }

  ctx->H[0] += A;
  ctx->H[1] += B;
  ctx->H[2] += C;
  ctx->H[3] += D;
  ctx->H[4] += E;
}

/*----------------------------------------------------------------------------
 *
 * This code added by Thomas "temas" Muldowney for Jabber compatability
 *
 *---------------------------------------------------------------------------*/
char *shahash(char *str)
{
    static char final[41];
    char *pos;
    unsigned char hashval[20];
    int x;

    if(!str || strlen(str) == 0)
        return NULL;

    shaBlock((unsigned char *)str, strlen(str), hashval);

    pos = final;
    for(x=0;x<20;x++)
    {
        snprintf(pos, 3, "%02x", hashval[x]);
        pos += 2;
    }
    return (char *)final;
}

void shahash_r(const char* str, char hashbuf[41])
{
    int x;
    char *pos;
    unsigned char hashval[20];
    
    if(!str || strlen(str) == 0)
        return;

    shaBlock((unsigned char *)str, strlen(str), hashval);

    pos = hashbuf;
    for(x=0;x<20;x++)
    {
        snprintf(pos, 3, "%02x", hashval[x]);
        pos += 2;
    }

    return;
}

static ErlDrvData sha_erl_start(ErlDrvPort port, char *buff)
{
   sha_data* d = (sha_data*)driver_alloc(sizeof(sha_data));
   d->port = port;

   return (ErlDrvData)d;
}

static void sha_erl_stop(ErlDrvData handle)
{
   driver_free((char*)handle);
}

static void sha_erl_output(ErlDrvData handle, char *buff, int bufflen)
{
   sha_data* d = (sha_data*)handle;
   ei_x_buff buf;

   static char final[41];
   char *pos;
   unsigned char hashval[20];
   int x;

   if(bufflen == 0)
      final[0] = '\000';
   else
   {
      shaBlock(buff, bufflen, hashval);

      pos = final;
      for(x=0;x<20;x++)
      {
	 snprintf(pos, 3, "%02x", hashval[x]);
	 pos += 2;
      }
   }
    
   ei_x_new_with_version(&buf);
   ei_x_encode_string(&buf, final);
    
   driver_output(d->port, buf.buff, buf.index);
   ei_x_free(&buf);
}


ErlDrvEntry sha_driver_entry = {
   NULL,                       /* F_PTR init, N/A */
   sha_erl_start,          /* L_PTR start, called when port is opened */
   sha_erl_stop,           /* F_PTR stop, called when port is closed */
   sha_erl_output,         /* F_PTR output, called when erlang has sent */
   NULL,                       /* F_PTR ready_input, called when input descriptor ready */
   NULL,                       /* F_PTR ready_output, called when output descriptor ready */
   "sha_erl",              /* char *driver_name, the argument to open_port */
   NULL,                       /* F_PTR finish, called when unloaded */
   NULL,                       /* F_PTR control, port_command callback */
   NULL,                       /* F_PTR timeout, reserved */
   NULL                        /* F_PTR outputv, reserved */
};

DRIVER_INIT(sha_erl) /* must match name in driver_entry */
{
   return &sha_driver_entry;
}

