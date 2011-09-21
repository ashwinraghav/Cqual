/* This file is part of cqual.
   Copyright (C) 2000-2002 The Regents of the University of California.

   Portions from the GNU C Library, copyright (C)
   1991-1994,96,97,98,99,2000,2001 Free Software Foudation, Inc.

cqual is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

cqual is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

The GNU C Library is free software; you can redistribute it and/or
modify it under the terms of the GNU Lesser General Public
License as published by the Free Software Foundation; either
version 2.1 of the License, or (at your option) any later version.

The GNU C Library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Lesser General Public License for more details.

You should have received a copy of the GNU General Public License
along with cqual; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA. */

#include <assert.h>
#include <stdio.h>
#if TIME_WITH_SYS_TIME
# include <sys/time.h>
# include <time.h>
#else
# if HAVE_SYS_TIME_H
#  include <sys/time.h>
# else
#  include <time.h>
# endif
#endif
#include <unistd.h>
#include "utils.h"
#include "timer.h"

#define BUFSIZE 80   /* Size of the timer_to_ascii buffer */

#ifdef TIMER_USERTIME
void reset_timer(timer *t)
{
  t->elapsed = 0;
}

/* Begin timing */
void start_timer(timer *t)
{
  struct tms begin;

  times(&begin);
  t->start = begin.tms_utime;
}

/* End timing, adding difference between start time and current time
   to elapsed time */
void end_timer(timer *t)
{
  struct tms end;
  
  times(&end);
  t->elapsed += end.tms_utime - t->start;
}

/* Convert elapsed time of t into a string */
char *timer_to_ascii(timer *t)
{
  static char buf[BUFSIZE];
  double secs;

  secs = ((double)t->elapsed)/sysconf(_SC_CLK_TCK);
  snprintf(buf, BUFSIZE, "%.3f", secs);
  buf[BUFSIZE-1] = '\0';
  return buf;
}
#else

/* Macros from the GNU C Library, for systems that don't have them */
#ifndef timerisset
# define timerisset(tvp)	((tvp)->tv_sec || (tvp)->tv_usec)
#endif
#ifndef timerclear
# define timerclear(tvp)	((tvp)->tv_sec = (tvp)->tv_usec = 0)
#endif
#ifndef timeradd
# define timeradd(a, b, result)						      \
  do {									      \
    (result)->tv_sec = (a)->tv_sec + (b)->tv_sec;			      \
    (result)->tv_usec = (a)->tv_usec + (b)->tv_usec;			      \
    if ((result)->tv_usec >= 1000000)					      \
      {									      \
	++(result)->tv_sec;						      \
	(result)->tv_usec -= 1000000;					      \
      }									      \
  } while (0)
#endif
#ifndef timersub
# define timersub(a, b, result)						      \
  do {									      \
    (result)->tv_sec = (a)->tv_sec - (b)->tv_sec;			      \
    (result)->tv_usec = (a)->tv_usec - (b)->tv_usec;			      \
    if ((result)->tv_usec < 0) {					      \
      --(result)->tv_sec;						      \
      (result)->tv_usec += 1000000;					      \
    }									      \
  } while (0)
#endif

/* Begin timing */
void start_timer(timer *t)
{
  assert(!timerisset(&t->start));
  insistnot(gettimeofday(&t->start, NULL));
}

/* End timing, adding difference between start time and current time
   to elapsed time */
void end_timer(timer *t)
{
  struct timeval end, diff;

  insistnot(gettimeofday(&end, NULL));
  assert(timerisset(&t->start));

  timersub(&end, &t->start, &diff);
  timeradd(&t->elapsed, &diff, &t->elapsed);
  timerclear(&t->start);
}

/* Convert elapsed time of t into a string */
char *timer_to_ascii(timer *t)
{
  static char buf[BUFSIZE];
  double secs, usecs;

  usecs = ((double) t->elapsed.tv_usec) / 1000000;
  secs = ((double) t->elapsed.tv_sec) + usecs;
  snprintf(buf, BUFSIZE, "%.3f", secs);
  buf[BUFSIZE-1] = '\0';
  return buf;
}
#endif
