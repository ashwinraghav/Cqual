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

%option noyywrap nounput
%x COMMENT

%{
  #include <string.h>
  #include "lattice-parse.tab.h"
  extern int lattice_line;
%}

qualifier \$[0-9a-zA-Z_]*
string \"[^""\n]*\"

%%

[\t ]      /* Skip */
\n          { lattice_line++; }
"/*"        { BEGIN(COMMENT); }
"const"     { lyylval.str = strdup(lyytext); return QUALIFIER; }
"volatile"  { lyylval.str = strdup(lyytext); return QUALIFIER; }
"restrict"  { lyylval.str = strdup(lyytext); return QUALIFIER; }
{qualifier} { lyylval.str = strdup(lyytext); return QUALIFIER; }
"color"     { return COLOR; }
"flow-sensitive"   { return FLOW_SENSITIVE; }
"nonprop"   { return NONPROP; }
"ref"       { return REF; }
"value"     { return VALUE; }
"level"     { return LEVEL; }
"sign"      { return SIGN; }
"pos"       { return POS; }
"neg"       { return NEG; }
"eq"        { return EQ; }
[Pp][Aa][Rr][Tt][Ii][Aa][Ll] { return PARTIAL; }
[Oo][Rr][Dd][Ee][Rr] { return ORDER; }
\{          { return '{'; }
\}          { return '}'; }
{string}    { lyylval.str = strdup(lyytext+1);
              lyylval.str[strlen(lyylval.str)-1] = '\0';
              return STRING; }
\<          { return '<'; }
\=          { return '='; }
\[          { return '['; }
\]          { return ']'; }
,           { return ','; }

    /* Taken from flex man page */
<COMMENT>{
[^*\n]*        /* eat anything that's not a '*' */
"*"+[^*/\n]*   /* eat up '*'s not followed by '/'s */
\n             { lattice_line++; }
"*"+"/"        BEGIN(INITIAL);
}

%%
