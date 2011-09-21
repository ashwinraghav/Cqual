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

#ifndef COMPILER_H
#define COMPILER_H

# if defined (__STDC_VERSION__) && __STDC_VERSION__ >= 199901L
#  define HAVE_C99
# endif

# if defined(__GNUC__) || defined(HAVE_C99)
#  define HAVE_VARIADIC_MACROS
# endif

# if defined(__GNUC__) || defined(HAVE_C99) || defined(__cplusplus)
#  define HAVE_INLINE
# endif

# if !defined(__GNUC__) && !defined(__attribute__)
#  define __attribute__(attributes)
# endif

#endif /* !COMPILER_H */
