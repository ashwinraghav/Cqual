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

#ifndef LOCATION_H
#define LOCATION_H

#include "config.h"
#include "bool.h"

typedef struct Location
{ 
  char *filename;
  unsigned long lineno;
  unsigned long filepos; /* Offset in file */
  bool in_system_header;
} *location;

int location_hash(location loc);
bool location_eq(location loc1, location loc2);
int location_cmp(location loc1, location loc2);

#endif
