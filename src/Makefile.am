## Process this file with automake to produce Makefile.in
## -*- makefile -*-

#  This file is part of cqual.
#  Copyright (C) 2000-2002 The Regents of the University of California.
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

regiondir = libcompat
regionflags = -Ddeletes= -Dtraditional= -Dsameregion= -Dparentptr=
CFLAGS = -O3
cflags =

SUBDIRS = $(regiondir)

INCLUDES = $(regionflags) $(cflags) -I$(srcdir)/$(regiondir)

libregions = $(regiondir)/libregions.a

bin_PROGRAMS = cqual iquals

parser_core =			\
	AST.h			\
	AST.c			\
	AST_types.h		\
	AST_defs.h		\
	AST_utils.h		\
	AST_utils.c		\
	array.c			\
	array.h			\
	bool.h			\
	buffer.h 		\
	buffer.c 		\
	builtins.h 		\
	builtins.c		\
	c-lex.h			\
	c-lex.c			\
	c-parse.h		\
	c-parse.tab.h		\
	c-parse.tab.c 		\
	c-gperf.h 		\
	compiler.h		\
	config.h 		\
	constants.h 		\
	constants.c		\
	containers.h 		\
	cqual.h 		\
	cstring.h 		\
	cval.h 			\
	cval.c 			\
	dd_list.h		\
	dd_list.c 		\
	decls.h 		\
	env.h 			\
	env.c			\
	errors.h		\
	errors.c 		\
	expr.h			\
	expr.c 			\
	flags.h 		\
	hash.h 			\
	hash.c			\
	input.h 		\
	input.c 		\
	linkage.h 		\
	location.h 		\
	location.c		\
	mbchar.h		\
	mbchar.c		\
	parser.h		\
	qualifiers.h		\
	semantics.h		\
	semantics.c		\
	stmt.h			\
	stmt.c			\
	types.h			\
	types.c			\
	unparse.h		\
	unparse.c		\
	utils.h			\
	utils.c

cqual_SOURCES = 		\
	$(parser_core)		\
	analyze.h		\
	analyze.c		\
	bitset.h		\
	bitset.c		\
	color.h			\
	effect.h 		\
	effect.c 		\
	eq_expressions.c	\
	flow.h 			\
	flow.c 			\
	lattice-lex.c		\
	lattice-parse.tab.h	\
	lattice-parse.tab.c	\
	main.c 			\
	pam.h 			\
	pam.c			\
	qerror.h		\
	qerror.c		\
	qtype.h			\
	qtype.c			\
	quals.h			\
	quals.c			\
	set.h			\
	set.c			\
	timer.h			\
	timer.c			\
	usage.h			\
	usage.c

iquals_SOURCES = 		\
	bitset.h		\
	bitset.c		\
	buffer.h		\
	buffer.c		\
	dd_list.h		\
	dd_list.c		\
	hash.h			\
	hash.c			\
	iquals-lex.c		\
	iquals.tab.c		\
	iquals.tab.h		\
	lattice-lex.c		\
	lattice-parse.tab.h	\
	lattice-parse.tab.c	\
	linkage.h		\
	quals.h			\
	quals.c			\
	qerror.h		\
	qerror.c		\
	set.h			\
	set.c			\
	utils.h			\
	utils.c

LDADD = $(libregions) @ALLOCA@

BUILT_SOURCES = 		\
	AST_defs.c		\
	AST_defs.h		\
	AST_parent.c		\
	AST_print.c		\
	AST_types.h		\
	c-gperf.h		\
	c-parse.tab.h		\
	c-parse.tab.c		\
	containers.h		\
	lattice-parse.tab.c	\
	lattice-parse.tab.h	\
	lattice-lex.c		\
	iquals-lex.c		\
	iquals.tab.c		\
	iquals.tab.h

EXTRA_DIST =			\
	build-basics.el 	\
	build-parent.el		\
	build-print.el		\
	build-types.el		\
	build-containers.el	\
	c-parse.gperf		\
	c-parse.y		\
	containers.def		\
	iquals.l		\
	iquals.y		\
	lattice-parse.y		\
	lattice-parse.l		\
	nodetypes.def

$(srcdir)/c-parse.tab.c $(srcdir)/c-parse.tab.h: c-parse.y
	bison -d $<

$(srcdir)/AST_defs.c $(srcdir)/AST_defs.h $(srcdir)/AST_types.h: nodetypes.def build-types.el build-basics.el
	cd $(srcdir) && emacs -batch -load build-types.el

$(srcdir)/AST_parent.c: nodetypes.def build-parent.el build-basics.el
	cd $(srcdir) && emacs -batch -load build-parent.el

$(srcdir)/AST_print.c: nodetypes.def build-print.el build-basics.el
	cd $(srcdir) && emacs -batch -load build-print.el

$(srcdir)/containers.h: containers.def build-containers.el
	cd $(srcdir) && emacs -batch -load build-containers.el

$(srcdir)/lattice-parse.tab.c $(srcdir)/lattice-parse.tab.h: lattice-parse.y
	bison -d -plyy lattice-parse.y

$(srcdir)/lattice-lex.c: lattice-parse.tab.h lattice-parse.l
	$(LEX) -olattice-lex.c -Plyy lattice-parse.l

$(srcdir)/iquals.tab.c $(srcdir)/iquals.tab.h: iquals.y
	bison -d iquals.y

$(srcdir)/iquals-lex.c: $(srcdir)/iquals.tab.h iquals.l
	$(LEX) -oiquals-lex.c iquals.l

$(srcdir)/c-gperf.h: c-parse.gperf qualifiers.h
	cd $(srcdir) && m4 c-parse.gperf | grep -v '^$$' | \
	gperf -p -j1 -i 1 -g -o -t -G -N is_reserved_word -k1,3,$$ >$@.
	mv $@. $@
