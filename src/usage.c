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

#include <assert.h>
#include <sys/types.h>
#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "usage.h"

#define PROC_LINE_LENGTH 4096

/* Return the memory used by the current process, in bytes */
long get_memusage(void)
{
  long vmsize;
  pid_t pid;
  char filename[64];
  char buf[PROC_LINE_LENGTH];
  FILE *stat_file;
  int i, field;

  vmsize = 0;
  pid = getpid();
  snprintf(filename, 64, "/proc/%d/stat", pid);
  filename[63] = '\0';
  stat_file = fopen(filename, "r");
  if (!stat_file)
    {
      perror(filename);
      exit(EXIT_FAILURE);
    }

  /* Read in /proc/<pid>/stat */
  if (!fread(buf, sizeof(char), PROC_LINE_LENGTH, stat_file))
    {
      perror(filename);
      exit(EXIT_FAILURE);
    }

  /* Scan over fields; assume separated by spaces */
  field = 0;
  i = 0;
  while (buf[i])
    {
      if (field == 22)
	{
	  /* memory size, in bytes */
	  for (; '0' <= buf[i] && buf[i] <= '9'; i++)
	    vmsize = 10*vmsize + buf[i] - '0';

	}

      /* Skip ahead to next field */
      while (buf[i] && buf[i] != ' ')
	i++;
      i++; /* Skip over space */
      field++;
    }

  fclose(stat_file);
  return vmsize;
}
