## Process this file with automake to produce Makefile.in
## -*- makefile -*-
# Copyright (c) 1999-2002 The Regents of the University of California.
#
# PAM is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2, or (at your option)
# any later version.
#
# PAM is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with PAM; see the file COPYING.  If not, write to
# the Free Software Foundation, 59 Temple Place - Suite 330,
# Boston, MA 02111-1307, USA.

SUBDIRS = elib

info_TEXINFOS = pam-mode.texi

elfiles =			\
	pam-3.el		\
	pam-vars.el		\
	pam-util.el		\
	pam-overlays.el 	\
	pam-window.el		\
	pam-executions.el	\
	pam-hyper.el		\
	pam-blob.el		\
	pam-faces.el		\
	pam-copyright.el

EXTRA_DIST =			\
	COPYING			\
	COPYRIGHT		\
	emacs-functions		\
	pam-3.sig		\
	pam-3.sml		\
	README			\
	sources.cm		\
	simple/dictionary	\
	simple/pamint.sml	\
	simple/simple.sml	\
	simple/sources.cm	\
	simple/text.sml		\
	lpath.el		\
	$(elfiles)

%.elc: %.el
	emacs -batch -q -l $(srcdir)/lpath.el -f batch-byte-compile $<

all-local: $(elfiles:.el=.elc)

clean-local:
	rm -f *.elc

%.info: %.texi
	makeinfo -o $@ $<

$(srcdir)/pam-vars.elc: $(srcdir)/pam-copyright.elc
$(srcdir)/pam-util.elc: $(srcdir)/pam-vars.elc $(srcdir)/pam-faces.elc $(srcdir)/pam-copyright.elc
$(srcdir)/pam-overlays.elc: $(srcdir)/pam-vars.elc $(srcdir)/pam-util.elc $(srcdir)/pam-copyright.elc
$(srcdir)/pam-window.elc: $(srcdir)/pam-vars.elc $(srcdir)/pam-util.elc $(srcdir)/pam-overlays.elc $(srcdir)/pam-copyright.elc
$(srcdir)/pam-executions.elc: $(srcdir)/pam-vars.elc $(srcdir)/pam-util.elc $(srcdir)/pam-window.elc $(srcdir)/pam-copyright.elc
$(srcdir)/pam-hyper.elc: $(srcdir)/pam-vars.elc $(srcdir)/pam-util.elc $(srcdir)/pam-window.elc $(srcdir)/pam-overlays.elc $(srcdir)/pam-copyright.elc
$(srcdir)/pam-blob.elc: $(srcdir)/pam-vars.elc $(srcdir)/pam-util.elc $(srcdir)/pam-window.elc $(srcdir)/pam-overlays.elc $(srcdir)/pam-hyper.elc $(srcdir)/pam-copyright.elc
$(srcdir)/pam-3.elc: $(srcdir)/pam-vars.elc $(srcdir)/pam-util.elc $(srcdir)/pam-overlays.elc $(srcdir)/pam-window.elc $(srcdir)/pam-executions.elc $(srcdir)/pam-hyper.elc $(srcdir)/pam-blob.elc $(srcdir)/pam-copyright.elc
