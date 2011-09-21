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

#ifndef COLOR_H
#define COLOR_H

#include "linkage.h"

#define pam_color_1          "pam-color-1"
#define pam_color_2          "pam-color-2"
#define pam_color_3          "pam-color-3"
#define pam_color_4          "pam-color-4"
#define pam_color_5          "pam-color-5"
#define pam_color_6          "pam-color-6"
#define pam_color_7	     "pam-color-7"
#define pam_color_8	     "pam-color-8"
#define pam_color_mouse      "pam-color-mouse"
#define pam_color_multiqual  "pam-color-5"
#define pam_color_noqual     "pam-color-1"
#define pam_color_error      "pam-color-8"
#define pam_color_warning    "pam-color-6"
#define pam_color_info       "pam-color-7"

/* Returns color we should use for lub of colors 1 and 2 */
EXTERN_C const char *combine_colors_pam(const char *color1, const char *color2);

#endif
