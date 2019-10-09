/*
 * A simple zlib module for Sicstus Prolog
 *
 * Copyright (c) 2009 Daniel de Kok
 *
 * This software is provided 'as-is', without any express or implied
 * warranty.  In no event will the authors be held liable for any damages
 * arising from the use of this software.
 *
 * Permission is granted to anyone to use this software for any purpose,
 * including commercial applications, and to alter it and redistribute it
 * freely, subject to the following restrictions:
 *
 * 1. The origin of this software must not be misrepresented; you must not
 *    claim that you wrote the original software. If you use this software
 *    in a product, an acknowledgment in the product documentation would be
 *    appreciated but is not required.
 * 2. Altered source versions must be plainly marked as such, and must not be
 *    misrepresented as being the original software.
 * 3. This notice may not be removed or altered from any source distribution.
 *
 */

#include <math.h>
#include <stdlib.h>
#include <string.h>
#include <zlib.h>

#include <stdio.h>

#include <sicstus/sicstus.h>

SP_term_ref zlib_compress(SP_term_ref data, long level);

/* Raise a Sicstus exception. */
void zlib_raise_exception(char const *message)
{
  SP_term_ref m = SP_new_term_ref();
  SP_put_string(m, message);
  SP_raise_exception(m);
}

/* Get the length of a Prolog list. Returns 0 if the provided term is not
 * a list. */
int list_length(SP_term_ref list, size_t *len)
{
  if (!SP_is_list(list))
      return 0;

  SP_term_ref head = SP_new_term_ref();
  SP_term_ref tail = SP_new_term_ref();
  SP_term_ref tmp = list;

  /* Chop off heads until the tail is empty */
  *len = 0;
  while (SP_get_list(tmp,head,tail) == SP_SUCCESS) {
    ++(*len);
    tmp = tail;
  }

  return 1;
}

/* Put the first n bytes of a Sicstus list in a buffer. Returns a
 * pointer to the array if the conversion succeeded, otherwise returns
 * NULL. The array is dynamically allocated, and should be freed by
 * the caller. */
unsigned char *list_to_buf(SP_term_ref data, size_t n)
{
  unsigned char *cData = malloc(n);
  size_t w;
  SP_term_ref tail = SP_new_term_ref();
  SP_get_list_n_bytes(data, tail, n, &w, cData);

  if (w != n) {
    free(cData);
    cData = NULL;
  }

  return cData;
}

// Buffer to byte list
SP_term_ref buf_to_list(unsigned char *buf, size_t len)
{
  SP_term_ref bList = SP_new_term_ref();
  SP_term_ref tail = SP_new_term_ref();
  int r = SP_put_list_n_bytes(bList, tail, len, buf);
  if (!r)
    zlib_raise_exception("Error in buf_to_list: could not convert buffer!");

  return bList;
}

SP_term_ref zlib_compress_default(SP_term_ref data)
{
  zlib_compress(data, Z_DEFAULT_COMPRESSION);
}

unsigned char *zlib_buf_compress(unsigned char *cData, size_t dataLen,
				 int level, size_t *zSize)
{
  /* Prepare destination buffer */
  *zSize = ceill((dataLen + 1) * 1.001) + 12;
  unsigned char *zcData = (unsigned char *) malloc(*zSize);

  /* Compress */
  int r = compress2(zcData, zSize, cData, dataLen, level);
  if (r != Z_OK) {
    switch (r) {
    case Z_MEM_ERROR:
      zlib_raise_exception("Error in zlib_compress: not enough memory!");
      break;
    case Z_BUF_ERROR:
      zlib_raise_exception("Error in zlib_compress: output buffer is too small!");
      break;
    case Z_STREAM_ERROR:
      zlib_raise_exception("Error in zlib_compress: invalid level!");
      break;
    default:
      zlib_raise_exception("Error in zlib_compress: unknown error!");
    }

    free(zcData);
    return NULL;
  }

  return zcData;
}

unsigned char *zlib_uncompress_buf(unsigned char *zcData, size_t zDataLen,
				   size_t dataLen) {
  /* Output buffer */
  unsigned char *cData = (unsigned char *) malloc(dataLen);

  /* Uncompress */
  int r = uncompress(cData, &dataLen, zcData, zDataLen);
  if (r != Z_OK) {
    switch(r) {
    case Z_MEM_ERROR:
      zlib_raise_exception("Error in zlib_uncompress: not enough memory!");
      break;
    case Z_BUF_ERROR:
      zlib_raise_exception("Error in zlib_uncompress: output buffer is too small!");
      break;
    case Z_DATA_ERROR:
      zlib_raise_exception("Error in zlib_uncompress: corrupted data!");
      break;
    default:
      zlib_raise_exception("Error in zlib_uncompress: unknown error!");
    }

    free(cData);
    return NULL;
  }

  return cData;
}

SP_term_ref zlib_compress_buf_list(unsigned char *cData, size_t dataLen,
				   int level)
{
  /* Compress buffer */
  size_t zSize = 0;
  unsigned char *zcData = zlib_buf_compress(cData, dataLen, level, &zSize);
  if (zcData == NULL) {
    free(zcData);
    return SP_new_term_ref();
  }

  /* Convert compressed buffer to a byte list */
  SP_term_ref zData = buf_to_list(zcData, zSize);

  free(zcData);

  return zData;
}

unsigned char *zlib_uncompress_list_buf(SP_term_ref zData, size_t dataLen)
{
  /* Get the list length */
  size_t zDataLen;
  int r = list_length(zData, &zDataLen);
  if (!r) {
    zlib_raise_exception("Error in zlib_uncompress_list_buf: argument is not a valid list!");
    return NULL;
  }

  /* Convert byte list to a buffer */
  unsigned char *zcData = list_to_buf(zData, zDataLen);
  if (zcData == NULL) {
    zlib_raise_exception("Error in zlib_uncompress_list_buf: could not convert list!");
    return NULL;
  }

  /* Uncompress buffer */
  unsigned char *cData = zlib_uncompress_buf(zcData, zDataLen, dataLen);
  free(zcData);
  if (cData == NULL)
    return NULL;

  /* Copy to a Sicstus-allocated buffer for garbage collection. */
  unsigned char *spCData = (unsigned char *) SP_malloc(dataLen);
  memcpy(spCData, cData, dataLen);
  free(cData);

  return spCData;
}

/* Compress data in a byte list. */
SP_term_ref zlib_compress(SP_term_ref data, long level)
{
  SP_term_ref zData = SP_new_term_ref();

  /* Get the list length */
  size_t dataLen;
  int r = list_length(data, &dataLen);
  if (!r) {
    zlib_raise_exception("Error in zlib_compress: argument is not a valid list!");
    return zData;
  }

  /* Convert byte list to a buffer */
  unsigned char *cData = list_to_buf(data, dataLen);
  if (cData == NULL) {
    zlib_raise_exception("Error in zlib_compress: could not convert list!");
    return zData;
  }

  /* Compress buffer */
  size_t zSize = 0;
  unsigned char *zcData = zlib_buf_compress(cData, dataLen, level, &zSize);
  if (zcData == NULL) {
    free(zcData);
    return SP_new_term_ref();
  }

  /* Convert compressed buffer to a byte list */
  zData = buf_to_list(zcData, zSize);

  free(zcData);

  return zData;
}

/* Uncompress data in a byte list. */
SP_term_ref zlib_uncompress(SP_term_ref zData, long dataLen)
{
  SP_term_ref data = SP_new_term_ref();

  /* Get the list length */
  size_t zDataLen;
  int r = list_length(zData, &zDataLen);
  if (!r) {
    zlib_raise_exception("Error in zlib_uncompress: argument is not a valid list!");
    return data;
  }

  /* Convert byte list to a buffer */
  unsigned char *zcData = list_to_buf(zData, zDataLen);
  if (zcData == NULL) {
    zlib_raise_exception("Error in zlib_uncompress: could not convert list!");
    return data;
  }

  /* Uncompress buffer */
  unsigned char *cData = zlib_uncompress_buf(zcData, zDataLen, dataLen);
  free(zcData);
  if (cData == NULL)
    return data;

  data = buf_to_list(cData, dataLen);

  free(cData);

  return data;
}

void zlib_free_buffer(unsigned char *buf)
{
  SP_free(buf);
}
