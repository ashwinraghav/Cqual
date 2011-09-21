/* This file is part of cqual.
   Copyright (C) 2000-2002 The Regents of the University of California.

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

#ifndef TIMER_H
#define TIMER_H

#include <sys/time.h>
#include "linkage.h"

EXTERN_C_BEGIN

#ifdef TIMER_USERTIME
typedef struct timer
{
  unsigned int start;   /* When this timer was started */
  unsigned int elapsed; /* Current elapsed time */
} timer;

void reset_timer(timer *t);
#else
typedef struct timer
{
  struct timeval start;   /* When this timer was started */
  struct timeval elapsed; /* Current elapsed time */
} timer;
#endif

/* #define ZERO_TIMER ((timer) {{0, 0}, {0, 0}}) */

/* Begin timing */
void start_timer(timer *t);

/* End timing, adding difference between start time and current time
   to elapsed time */
void end_timer(timer *t);

/* Convert elapsed time of t into a string */
char *timer_to_ascii(timer *t);

EXTERN_C_END

#endif
