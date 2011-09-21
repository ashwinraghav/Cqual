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

%token<str> QUALIFIER STRING
%token COLOR FLOW_SENSITIVE EQ LEVEL NEG NONPROP ORDER PARTIAL POS
%token REF SIGN VALUE

%{
  #include <stdio.h>
  #include <stdlib.h>
  #include <string.h>
  #include "quals.h"
  int lyylex(void);
  void lyyerror(char *);
  extern char *lyytext;
  int lattice_line = 0;
  char *lattice_file;
  qual current_qual = NULL;
%}

%union {
  char *str;
}

%start po

%%

po:     /* empty */
        | PARTIAL ORDER { begin_po_qual(); }
	    po_attr_opt '{' descr '}' { end_po_qual(); } po

po_attr_opt:  /* empty */
	    | '[' po_attrs ']'

po_attrs: po_attr
        | po_attrs ',' po_attr

po_attr: FLOW_SENSITIVE { set_po_flow_sensitive(); }
       | NONPROP { set_po_nonprop(); }

descr:	  /* empty */
	| qual_attr descr
	| qual_order descr

qual_attr:
	  QUALIFIER
	  { add_qual($1); }
	| QUALIFIER { current_qual = add_qual($1); } '[' attributes ']'
	  { current_qual = NULL; }

attributes:
	  attribute
	| attributes ',' attribute

attribute:
	  COLOR '=' STRING
	  { add_color_qual(current_qual, $3); }
	| level_attribute
	| sign_attribute

level_attribute:
	  LEVEL '=' REF
	  { add_level_qual(current_qual, level_ref); }
	| LEVEL '=' VALUE
	  { add_level_qual(current_qual, level_value); }

sign_attribute:
	  SIGN '=' POS
	  { add_sign_qual(current_qual, sign_pos); }
	| SIGN '=' NEG
	  { add_sign_qual(current_qual, sign_neg); }
	| SIGN '=' EQ
	  { add_sign_qual(current_qual, sign_eq); }

qual_order:
	  QUALIFIER '<' QUALIFIER
	  { qual q1, q2;

	    q1 = add_qual($1);
	    q2 = add_qual($3);
	    add_qual_lt(q1, q2);
	  }

%%

void lyyerror(char *string)
{
  fprintf(stderr, "%s:%d: %s\n", lattice_file, lattice_line, string);
}

void load_config_file_quals(const char *filename)
{
  extern void lyyrestart(FILE *);
  FILE *file;

  file = fopen(filename, "r");
  if (!file)
    {
      fprintf(stderr, "Unable to open ``%s'': ", filename);
      perror("");
      exit(EXIT_FAILURE);
    }
  lattice_file = strdup(filename);
  lattice_line = 1;
  lyyrestart(file);
  lyyparse();
  fclose(file);
  free(lattice_file);
  lattice_file = NULL;
}
