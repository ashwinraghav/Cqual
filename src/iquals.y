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

%token<str> CONSTANT VARIABLE
%token EQ IMPLIES LEQ UNIFY ERROR
%type <qual> qualifier

%{
  #include <string.h>
  #include <stdlib.h>
  #include <stdarg.h>
  #include <stdio.h>
  #include "regions.h"
  #include "quals.h"
  #include "qerror.h"
  int yylex(void);
  void yyerror(char *, ...);
  extern char *yytext;
  int line = 0;
  char *filename = NULL;

  qual lookup_add_var(char *name);
%}

%union {
  char *str;
  qual qual;
}

%left IMPLIES

%%

system: /* Empty */
      | constraint system

constraint: qualifier LEQ qualifier
            {
	      if (mkleq_qual(NULL, $1, $3))
		{
		  yyerror("Unsatisfiable constraint");
		  YYABORT;
		}
	    }
          | qualifier EQ qualifier
            {
	      if (mkeq_qual(NULL, $1, $3))
		{
		  yyerror("Unsatisfiable constraint");
		  YYABORT;
		}
	    }
          | qualifier UNIFY qualifier
            {
	      if (unify_qual(NULL, $1, $3))
		{
		  yyerror("Unsatisfiable constraint");
		  YYABORT;
		}
	    }
          | qualifier LEQ qualifier IMPLIES qualifier LEQ qualifier
            {
	      if (cond_mkleq_qual(NULL, $1, $3, $5, $7))
		{
		  yyerror("Unsatisfiable constraint");
		  YYABORT;
		}
	    }

qualifier: CONSTANT { $$ = find_qual($1);
                      if (!$$)
			{
			  yyerror("Unknown qualifier %s", $1);
			  YYABORT;
			}
                    }
         | VARIABLE { $$ = lookup_add_var($1); }

%%

void yyerror(char *format, ...)
{
  va_list args;

  fprintf(stderr, "%s:%d: ", filename, line);
  va_start(args, format);
  vfprintf(stderr, format, args);
  va_end(args);
  fprintf(stderr, "\n");
}

void fatal(const char *format, ...)
{
  va_list args;
  va_start(args, format);
  vfprintf(stderr, format, args);
  va_end(args);
  fprintf(stderr, "\n");
  exit(EXIT_FAILURE);
}

/* HACK:  We should really put a better interface in quals.c */
struct Store {};
struct Aloc {};
void propagate_store_cell_forward(store s, aloc al) {}
void propagate_store_cell_backward(store s, aloc al) {}
void report_qerror(location loc, severity sev, const char *format, ...) {}
int location_cmp(location loc1, location loc2) { return 1; }
void pam_add_overlay_anchor(location loc) {}
int flag_ugly = 0;

region iquals_region;
location dummy_location = NULL;

typedef struct var_list {
  char *name;
  qual qual;
  struct var_list *next;
} *var_list;

var_list vars = NULL;

/* If name is already in vars, returns its qual, otherwise make a new
   qual q, add name->q to vars, and return q. */
qual lookup_add_var(char *name)
{
  var_list cur = vars;
  var_list new;

  while (cur)
    {
      if (!strcmp(cur->name, name))
	return cur->qual;
      cur = cur->next;
    }
  /* Not found */
  new = ralloc(iquals_region, struct var_list);
  new->name = name;
  new->qual = make_qvar(name, dummy_location, FALSE);
  new->next = vars;
  vars = new;
  return new->qual;
}

void print_results(void)
{
  var_list cur;

  for (cur = vars; cur; cur = cur->next)
    {
      printf("%s: ", cur->name);
      print_qual(printf, cur->qual);
      printf("\n");
    }
}

int main(int argc, char **argv)
{
  extern void yyrestart(FILE *);
  char *config_file = NULL;
  FILE *file;
  int i, err;

  region_init();
  iquals_region = newregion();

  for (i = 1; i < argc; i++)
    {
      if (!strcmp(argv[i], "-config"))
	{
	  if (i < argc - 1)
	    {
	      i++;
	      config_file = argv[i];
	    }
	  else
	    fatal("Missing -config file");
	}
      else if (!strcmp(argv[i], "-g"))
	flag_print_quals_graph = TRUE;
      else
	{
	  if (!filename)
	    filename = argv[i];
	  else
	    fatal("Multiple input filenames");
	}
    }

  if (!filename)
    {
      printf("Usage: iquals [-config <file>|-g] <filename>\n");
      printf("File format:\n\n");
      printf("\tq1 <= q2                inequality\n");
      printf("\tq1  = q2                equality\n");
      printf("\tq1 == q2                unification\n");
      printf("\tq1 <= q2 ==> q3 <= q4   conditional inequality\n");
      exit(EXIT_SUCCESS);
    }

  init_quals();

  if (config_file)
    load_config_file_quals(config_file);

  end_define_pos();

  file = fopen(filename, "r");
  if (!file)
    {
      fprintf(stderr, "Unable to open ``%s'': ", filename);
      perror("");
      exit(EXIT_FAILURE);
    }

  yyrestart(file);
  line = 1;
  err = yyparse();
  fclose(file);
  finish_quals();
  print_results();
  if (err)
    exit(EXIT_FAILURE);
  else
    exit(EXIT_SUCCESS);
}
