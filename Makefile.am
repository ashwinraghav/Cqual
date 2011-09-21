## Process this file with automake to produce Makefile.in
## -*- makefile -*-
#    This file is part of cqual.
#    Copyright (C) 2000-2002 The Regents of the University of California.
# 
# cqual is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2, or (at your option)
# any later version.
# 
# cqual is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with cqual; see the file COPYING.  If not, write to
# the Free Software Foundation, 59 Temple Place - Suite 330,
# Boston, MA 02111-1307, USA.

AUTOMAKE_OPTIONS = foreign
SUBDIRS = src doc PAM-3
EXTRA_DIST = 			\
	bin/gccpreproc		\
	bin/remblanks		\
	bin/remquals		\
	examples/README		\
	examples/taint0.c	\
	examples/taint1.c	\
	examples/taint2.c	\
	examples/taint-cast.c	\
	examples/taint-const-subtyping.c  \
	examples/taint-poly.c	\
	examples/taint-varargs.c\
	examples/lock.c		\
	examples/lock2.c	\
	examples/lock3.c	\
	examples/linux-lock.c	\
	examples/linux-lock.i	\
	examples/linux-lock2.c	\
	examples/linux-lock2.i	\
	examples/y2k1.c		\
	examples/y2k2.c		\
	examples/y2k3.c		\
	examples/y2k4.c		\
	examples/rcs1.c		\
	examples/rcs2.c		\
	examples/rcs3.c		\
	config/lattice 		\
	config/prelude.i	\
	inst/install-sh		\
	inst/missing		\
	inst/mkinstalldirs	\
	inst/texinfo.tex	\
	inst/ylwrap		\
	aclocal.m4		\
	COPYING			\
	COPYRIGHT		\
	README			\
	setup-pam

all-local: personal

.PHONY:  personal

personal:
	perl $(srcdir)/setup-pam
