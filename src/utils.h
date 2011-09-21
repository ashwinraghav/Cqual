/* This file is part of cqual.
   Copyright (C) 2000-2001 The Regents of the University of California.

cqual is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

cqual is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with cqual; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA. */

#ifndef UTILS_H
#define UTILS_H

#include <assert.h>
#include <stdarg.h>
#include <stdlib.h>
#include "bool.h"
#include "compiler.h"
#include "regions.h"
#include "utils.h"
#include "linkage.h"

EXTERN_C_BEGIN

void *xmalloc(size_t size);
void *xrealloc(void *p, size_t newsize);

unsigned long align_to(unsigned long n, unsigned long alignment);

/* least common multiple */
unsigned long lcm(unsigned long x, unsigned long y);

#define MAX(a, b) ((a) > (b) ? (a) : (b))
#define MIN(a, b) ((a) < (b) ? (a) : (b))

#ifdef HAVE_VARIADIC_MACROS
#define fail(args...) __fail(__FILE__, __LINE__, __FUNCTION__, args)
#else
void fail(const char *fmt, ...);
#endif

void __fail(const char *file, unsigned int line,
	    const char *func, const char *fmt, ...) __attribute__ ((__noreturn__));


/* insist(action) is like assert(action), but action may have
   side-effects */
#ifdef NDEBUG
# define insist(action)  (action)
#else
# define insist assert
#endif

#ifdef NDEBUG
# define insistnot(action)  (action)
#else
# define insistnot(action) assert(!(action))
#endif

/* Concatenate 2 strings, allocating space in r for the result */
char *rstrcat(region, const char *, const char *);

/* Concatenate n strings, allocating space in r for the result.  The
   last argument should be a null pointer. */
char *rstrscat(region, ...);

/* Convert an integer to a string, storing the result in r */
const char *inttostr(region r, int);

/* sprintf a string, allocating space in r for the result */
char *rsprintf(region r, const char *fmt, ...);
char *rvsprintf(region r, const char *fmt, va_list args);

/* Convert a pointer to an ascii string with leading 0x.  Re-uses
   internal buffer. */
char *ptr_to_ascii(void *ptr);

/* Convert a pointer to an integer */
int ptr_hash(void *ptr);

/* Return TRUE iff ptr1 == ptr2 */
bool ptr_eq(void *ptr1, void *ptr2);

/* Hash a string */
int string_hash(const char *str);

/* Return TRUE iff s1 == s2 */
bool string_eq(const char *s1, const char *s2);

/* A total ordering on pointers.  Returns 0 if ptr1 = ptr2, a value <0
   if ptr1 < ptr2, or a value >0 if ptr1 > ptr2. */
int ptr_cmp(void *ptr1, void *ptr2);

EXTERN_C_END

#endif
