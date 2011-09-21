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

#include <string.h>
#include "location.h"
#include "utils.h"

int location_hash(location loc)
{
  int h;

  h = 0;
  h = loc->lineno;
  h = 33*h + 720 + loc->filepos;
  h = 33*h + 720 + string_hash(loc->filename);
  return h;
}

bool location_eq(location loc1, location loc2)
{
  return (loc1->lineno == loc2->lineno &&
	  loc1->filepos == loc2->filepos &&
	  loc1->in_system_header == loc2->in_system_header &&
	  !strcmp(loc1->filename, loc2->filename));
}

int location_cmp(location loc1, location loc2)
{
  int result;

  result = strcmp(loc1->filename, loc2->filename);
  if (!result)
    result = loc1->filepos - loc2->filepos;
  return result;
}
