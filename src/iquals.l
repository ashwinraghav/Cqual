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

%option noyywrap nounput

%{
  #include <string.h>
  #include "quals.h"
  #include "iquals.tab.h"
  extern int line;
%}

constant \$[0-9a-zA-Z_]*
variable [0-9a-zA-Z_]+

%%

[\t ]		/* Skip */
\n 		{ line++; }
"const"         { yylval.str = strdup(yytext); return CONSTANT; }
"volatile"      { yylval.str = strdup(yytext); return CONSTANT; }
{constant}      { yylval.str = strdup(yytext); return CONSTANT; }
{variable}      { yylval.str = strdup(yytext); return VARIABLE; }
"<="            { return LEQ; }
"==>"           { return IMPLIES; }
"="             { return EQ; }
"=="            { return UNIFY; }
.               { return ERROR; }

%%
