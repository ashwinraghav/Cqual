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
EXTRA_DIST = 			\
	user-guide.ps		\
	user-guide.pdf

# A standard Makefile for latex stuff; this can be used as a template. 

ALL = user-guide.ps user-guide.pdf
PAPERTYPE = letter

all-local: $(ALL)

%.ps: %.dvi
	dvips -P cmz -t $(PAPERTYPE) $< -o $@

%.pdf: %.ps
	pdflatex $(@:.pdf=.tex)

%.dvi: %.tex
	echo "\newcommand{\version}{$(VERSION)}" > .version; \
	./mylatex $<

clean-local:
	-rm -f *.{aux,dvi,log,bbl,blg,ps,pdf,toc,lof,lot} .cache .cache.new
