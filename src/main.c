/* This file was modified in 2000-2002 by David Gay, Jeff Foster, and
   Tachio Terauchi for cqual.  The changes are Copyright (c) 2000-2002
   The Regents of the University of California.

   This file is distributed under the terms of the GNU General Public License
   (see below).  */

/* Top level of GNU C compiler
   Copyright (C) 1987, 88, 89, 92-7, 1998 Free Software Foundation, Inc.

This file is part of GNU CC.

GNU CC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU CC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU CC; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

#include <signal.h>
#include <sys/stat.h>
#include <glob.h>
#include <sys/time.h>
#include <unistd.h>
#include <limits.h>
#include "parser.h"
#include "input.h"
#include "c-parse.h"
#include "dd_list.h"
#include "unparse.h"
#include "semantics.h"
#include "cqual.h"
#include "analyze.h"
#include "flow.h"
#include "pam.h"
#include "quals.h"
#include "timer.h"
#include "utils.h"
#include "qtype.h"
#include "effect.h"
#include "usage.h"

/* Name of program invoked, sans directories.  */
char *progname;

/* Copy of arguments to main, without the filename.  */
int copy_argc;
char **copy_argv;

/* TRUE if we're analyzing a prelude (may space more than one actual
   file) */
bool in_prelude;

/* Number of hotspots to look for.  0 = look for no hotspots. */
extern int num_hotspots;

/* List of ASTs for the entire program */
dd_list parsed_files;

/* -f flags.  */

/* Nonzero means all references through pointers are volatile.  */
int flag_volatile;

/* Nonzero means treat all global and extern variables as global.  */
int flag_volatile_global;

/* Nonzero means just do syntax checking; don't output anything.  */
int flag_syntax_only;

/* Nonzero means change certain warnings into errors.
   Usually these are warnings about failure to conform to some standard.  */
int flag_pedantic_errors;

/* Tag all structures with __attribute__(packed) */
int flag_pack_struct;

/* Options controlling warnings */

/* Don't print warning messages.  -w.  */
int inhibit_warnings;

/* Print various extra warnings.  -W.  */
int extra_warnings;

/* Treat warnings as errors.  -Werror.  */
int warnings_are_errors;

/* Nonzero to warn about unused local variables.  */
int warn_unused;

/* Nonzero to warn about variables used before they are initialized.  */
int warn_uninitialized;

/* Nonzero means warn about all declarations which shadow others.   */
int warn_shadow;
int error_shadow; /* Make shadow an error */

/* Warn if a switch on an enum fails to have a case for every enum value.  */
int warn_switch;

/* Nonzero means warn about function definitions that default the return type
   or that use a null return and have a return-type other than void.  */
int warn_return_type;

/* Nonzero means warn about pointer casts that increase the required
   alignment of the target type (and might therefore lead to a crash
   due to a misaligned access).  */
int warn_cast_align;

/* Nonzero means warn about any identifiers that match in the first N
   characters.  The value N is in `id_clash_len'.  */
int warn_id_clash;
unsigned id_clash_len;

/* Nonzero means warn about any objects definitions whose size is larger
   than N bytes.  Also want about function definitions whose returned
   values are larger than N bytes. The value N is in `larger_than_size'.  */
int warn_larger_than;
unsigned larger_than_size;

/* Nonzero means warn if inline function is too large.  */
int warn_inline;

/* Warn if a function returns an aggregate,
   since there are often incompatible calling conventions for doing this.  */
int warn_aggregate_return;

/* Nonzero means `$' can be in an identifier.  */
int dollars_in_ident = 1; /* jfoster, default to on */

/* Nonzero means allow type mismatches in conditional expressions;
   just make their values `void'.   */
int flag_cond_mismatch;

/* Nonzero means don't recognize the keyword `asm'.  */
int flag_no_asm;

/* Nonzero means warn about implicit declarations.  */
int warn_implicit;

/* Nonzero means give string constants the type `const char *'
   to get extra warnings from them.  These warnings will be too numerous
   to be useful, except in thoroughly ANSIfied programs.  */
int warn_write_strings;

/* Nonzero means warn about sizeof (function) or addition/subtraction
   of function pointers.  */
int warn_pointer_arith;

/* Nonzero means warn for all old-style non-prototype function decls.  */
int warn_strict_prototypes;

/* Nonzero means warn about multiple (redundant) decls for the same single
   variable or function.  */
int warn_redundant_decls;

/* Nonzero means warn about extern declarations of objects not at
   file-scope level and about *all* declarations of functions (whether
   extern or static) not at file-scope level.  Note that we exclude
   implicit function declarations.  To get warnings about those, use
   -Wimplicit.  */
int warn_nested_externs;

/* Nonzero means warn about pointer casts that can drop a type qualifier
   from the pointer target type.  */
int warn_cast_qual;

/* Nonzero means warn when casting a function call to a type that does
   not match the return type (e.g. (float)sqrt() or (anything*)malloc()
   when there is no previous declaration of sqrt or malloc.  */
int warn_bad_function_cast;

/* Warn about traditional constructs whose meanings changed in ANSI C.  */
int warn_traditional;

/* Warn about *printf or *scanf format/argument anomalies. */
int warn_format;

/* Warn about a subscript that has type char.  */
int warn_char_subscripts;

/* Warn if a type conversion is done that might have confusing results.  */
int warn_conversion;

/* Warn if main is suspicious. */
int warn_main;

/* Nonzero means warn about use of multicharacter literals.  */
int warn_multichar = 1;

/* Nonzero means do some things the same way PCC does.  */
int flag_traditional;

/* Nonzero means to allow single precision math even if we're generally
   being traditional. */
int flag_allow_single_precision;

/* Nonzero means warn about suggesting putting in ()'s.  */
int warn_parentheses;

/* Warn if initializer is not completely bracketed.  */
int warn_missing_braces;

/* Warn about comparison of signed and unsigned values.  */
int warn_sign_compare;

/* Nonzero means message about use of implicit function declarations;
 1 means warning; 2 means error. */
int mesg_implicit_function_declaration;

bool pedantic; /* Report pedantic warnings if true */

/* Nonzero means warn about use of implicit int. */
int warn_implicit_int;

/* Nonzero means warn for any global function def
   without separate previous prototype decl.  */
int warn_missing_prototypes;

/* Nonzero means warn for any global function def
   without separate previous decl.  */
int warn_missing_declarations;

/* Don't print functions as they are compiled and don't print
   times taken by the various passes.  -quiet.  */
int quiet_flag;

/* Nonzero means don't run real cc1 afterwards */
int flag_parse_only;

/* Nonzero means `char' should be signed.  */
int flag_signed_char;

/* Nonzero means give an enum type only as many bytes as it needs.  */
int flag_short_enums;

/* Nonzero means to treat bitfields as signed unless they say `unsigned'.  */
int flag_signed_bitfields = 1;
int explicit_flag_signed_bitfields = 0;

/* Qualifier inference is being run as a PAM process */
int flag_pam_mode = 0;

/* PAM being run for web interface. */
int flag_pam_html = 0;

/* True if we should print out the qualifier constraint graph in dot
   form. */
int flag_strict_const = 0;

/* True if we should print out a summary of the results at the end. */
int flag_print_results = 0;

/* True if qualifiers should be preserved through casts. */
int flag_casts_preserve = 0;

/* True if const annotations imply we can use subtyping instead of
   equality under a const pointer. */
int flag_const_subtyping = 1;

/* True if we should do the flow-sensitive type qualifiers pass. */
int flag_flow_sensitive = 0;

/* Do the flow-sensitive analysis even if there are no flow-sensitive
   type qualifiers used */
int flag_force_flow_sensitive = 0;

/* True if we should print out the computed linearities; only valid when
   flag_flow_sensitive is set */
int flag_print_lin = 0;

/* True if we're analyzing a linux kernel driver, so the second and third
   pass should behave specially */
int flag_driver = 0;

/* True if we want cqual issuing warning when qualifiers flow into
   non-qualified casts */
int flag_casts_warn = 0;

/* True if we should display names in an ugly way so that you can tell
   different versions of the same name apart. */
int flag_ugly = 0;

/* True if we should print out some statistics about running time,
   memory usage, etc. */
int flag_statistics = 0;

/* True if we should display store contents when printing function types */
int flag_print_stores = 0;

/* Table of language-independent -f options.
   STRING is the option name.  VARIABLE is the address of the variable.
   ON_VALUE is the value to store in VARIABLE
    if `-fSTRING' is seen as an option.
   (If `-fno-STRING' is seen as an option, the opposite value is stored.)  */

struct { char *string; int *variable; int on_value;} f_options[] =
{
  {"volatile", &flag_volatile, 1},
  {"volatile-global", &flag_volatile_global, 1},
  {"syntax-only", &flag_syntax_only, 1},
  {"parse-only", &flag_parse_only, 1},
  {"pack-struct", &flag_pack_struct, 1},
  {"pam-mode", &flag_pam_mode, 1},
  {"pam-html", &flag_pam_html, 1},
  {"print-quals-graph", &flag_print_quals_graph, 1},
  {"strict-const", &flag_strict_const, 1},
  {"print-results", &flag_print_results, 1},
  {"casts-preserve", &flag_casts_preserve, 1},
  {"use-const-subtyping", &flag_const_subtyping, 1},
  {"flow-sensitive", &flag_flow_sensitive, 1},
  {"force-flow-sensitive", &flag_force_flow_sensitive, 1},
  {"print-lin", &flag_print_lin, 1},
  {"driver", &flag_driver, 1},
  {"casts-warn", &flag_casts_warn, 1},
  {"ugly", &flag_ugly, 1},
  {"stats", &flag_statistics, 1},
  {"print-stores", &flag_print_stores, 1},
};

/* Table of language-specific options.  */

char *lang_options[] =
{
  "-ansi",
  "-fallow-single-precision",

  "-fsigned-bitfields",
  "-funsigned-bitfields",
  "-fno-signed-bitfields",
  "-fno-unsigned-bitfields",
  "-fsigned-char",
  "-funsigned-char",
  "-fno-signed-char",
  "-fno-unsigned-char",

  "-ftraditional",
  "-traditional",
  "-fnotraditional",
  "-fno-traditional",

  "-fasm",
  "-fno-asm",
  "-fbuiltin",
  "-fno-builtin",
  "-fhosted",
  "-fno-hosted",
  "-ffreestanding",
  "-fno-freestanding",
  "-fcond-mismatch",
  "-fno-cond-mismatch",
  "-fdollars-in-identifiers",
  "-fno-dollars-in-identifiers",
  "-fident",
  "-fno-ident",
  "-fshort-double",
  "-fno-short-double",
  "-fshort-enums",
  "-fno-short-enums",

  "-Wall",
  "-Wbad-function-cast",
  "-Wno-bad-function-cast",
  "-Wcast-qual",
  "-Wno-cast-qual",
  "-Wchar-subscripts",
  "-Wno-char-subscripts",
  "-Wcomment",
  "-Wno-comment",
  "-Wcomments",
  "-Wno-comments",
  "-Wconversion",
  "-Wno-conversion",
  "-Wformat",
  "-Wno-format",
  "-Wimport",
  "-Wno-import",
  "-Wimplicit-function-declaration",
  "-Wno-implicit-function-declaration",
  "-Werror-implicit-function-declaration",
  "-Wimplicit-int",
  "-Wno-implicit-int",
  "-Wimplicit",
  "-Wno-implicit",
  "-Wmain",
  "-Wno-main",
  "-Wmissing-braces",
  "-Wno-missing-braces",
  "-Wmissing-declarations",
  "-Wno-missing-declarations",
  "-Wmissing-prototypes",
  "-Wno-missing-prototypes",
  "-Wnested-externs",
  "-Wno-nested-externs",
  "-Wparentheses",
  "-Wno-parentheses",
  "-Wpointer-arith",
  "-Wno-pointer-arith",
  "-Wredundant-decls",
  "-Wno-redundant-decls",
  "-Wsign-compare",
  "-Wno-sign-compare",
  "-Wstrict-prototypes",
  "-Wno-strict-prototypes",
  "-Wtraditional",
  "-Wno-traditional",
  "-Wtrigraphs",
  "-Wno-trigraphs",
  "-Wundef",
  "-Wno-undef",
  "-Wwrite-strings",
  "-Wno-write-strings",

  0
};


/* Likewise for -W.  */

struct { char *string; int *variable; int on_value;} W_options[] =
{
  {"unused", &warn_unused, 1},
  {"error", &warnings_are_errors, 1},
  {"shadow", &warn_shadow, 1},
  {"switch", &warn_switch, 1},
  {"aggregate-return", &warn_aggregate_return, 1},
  {"cast-align", &warn_cast_align, 1},
  {"uninitialized", &warn_uninitialized, 1},
  {"inline", &warn_inline, 1}
};

typedef struct parsed_file
{
  const char *name;
  declaration program;
} *parsed_file;

static timer total_time;
static long memusage;

/* Handler for SIGPIPE.  */

static void pipe_closed (int signo)
{
  fatal("output pipe has been closed");
}

/* Print a fatal error message.  NAME is the text.
   Also include a system error message based on `errno'.  */

void pfatal_with_name(const char *name)
{
  fprintf(stderr, "%s: ", progname);
  perror(name);
  exit(FATAL_EXIT_CODE);
}

RETSIGTYPE outofmemory(void)
{
  fprintf(stderr, "Out of memory - exiting\n");
  exit(FATAL_EXIT_CODE);
}

static void add_program_files(const char *file_list, dd_list files)
{
  FILE *list = fopen(file_list, "r");
  
  if(!list)
    {
      error("unable to open %s", file_list);
      return;
    }

  /* each line in */
  while(!ferror(list) && !feof(list))
    {
      char *newline = NULL;
      char name[PATH_MAX] = {0};
      
      if(!fgets(name, PATH_MAX, list))
	break;

      /* if there is a \n, replace it with \0 */
      newline = strchr(name, (int)'\n');
      if (newline)
	*newline = '\0';

      /* add the file to the files list */
      dd_add_last(parse_region, files, rstrdup(parse_region, name));
    }
}

/* Compile an entire file of output from cpp, named NAME.
   Write a file of assembly output and various debugging dumps.  */
static void compile_file(char *name)
{
  FILE *ifile;

#if !USE_CPPLIB
  /* Open input file.  */

  if (name == 0 || !strcmp (name, "-"))
    {
      ifile = stdin;
      name = "stdin";
    }
  else
    ifile = fopen (name, "r");
  if (ifile == 0)
    pfatal_with_name (name);

#ifdef IO_BUFFER_SIZE
  setvbuf(ifile, (char *) xmalloc (IO_BUFFER_SIZE), _IOFBF, IO_BUFFER_SIZE);
#endif
#endif /* !USE_CPPLIB */

  set_nomem_handler(outofmemory);

  set_input(ifile, name);
  init_lex(); /* needs to go before init_semantics, since inits
		 dummy_location */
  init_semantics();

  if (yyparse () != 0)
    {
      if (errorcount == 0)
	fprintf (stderr, "Errors detected in input file (your bison.simple is out of date)");
    }

  fclose (ifile);

  if (errorcount == 0)
    {
      /*if (flag_parse_only)
	{
	  fprintf(stderr, "\nDONE. Unparsing............\n\n");
	  unparse(stdout, the_program);
	}
      else
      exit(exec_cc1(the_program));*/

      if (the_program)
	{
	  parsed_file pf;

	  pf = ralloc(parse_region, struct parsed_file);
	  pf->name = name;
	  pf->program = the_program;
	  dd_add_last(parse_region, parsed_files, pf);

	  analyze(the_program);
	}
    }
}

/* Decode the string P as a language-specific option for C. */
void c_decode_option(char *p)
{
  if (!strcmp (p, "-ftraditional") || !strcmp (p, "-traditional"))
    {
      flag_traditional = 1;
    }
  else if (!strcmp (p, "-fallow-single-precision"))
    flag_allow_single_precision = 1;
  else if (!strcmp (p, "-fnotraditional") || !strcmp (p, "-fno-traditional"))
    {
      flag_traditional = 0;
    }
  else if (!strcmp (p, "-fdollars-in-identifiers"))
    dollars_in_ident = 1;
  else if (!strcmp (p, "-fno-dollars-in-identifiers"))
    dollars_in_ident = 0;
  else if (!strcmp (p, "-fsigned-char"))
    flag_signed_char = 1;
  else if (!strcmp (p, "-funsigned-char"))
    flag_signed_char = 0;
  else if (!strcmp (p, "-fno-signed-char"))
    flag_signed_char = 0;
  else if (!strcmp (p, "-fno-unsigned-char"))
    flag_signed_char = 1;
  else if (!strcmp (p, "-fsigned-bitfields")
	   || !strcmp (p, "-fno-unsigned-bitfields"))
    {
      flag_signed_bitfields = 1;
      explicit_flag_signed_bitfields = 1;
    }
  else if (!strcmp (p, "-funsigned-bitfields")
	   || !strcmp (p, "-fno-signed-bitfields"))
    {
      flag_signed_bitfields = 0;
      explicit_flag_signed_bitfields = 1;
    }
  else if (!strcmp (p, "-fshort-enums"))
    flag_short_enums = 1;
  else if (!strcmp (p, "-fno-short-enums"))
    flag_short_enums = 0;
  else if (!strcmp (p, "-fcond-mismatch"))
    flag_cond_mismatch = 1;
  else if (!strcmp (p, "-fno-cond-mismatch"))
    flag_cond_mismatch = 0;
  else if (!strcmp (p, "-fasm"))
    flag_no_asm = 0;
  else if (!strcmp (p, "-fno-asm"))
    flag_no_asm = 1;
  else if (!strcmp (p, "-ansi"))
    flag_no_asm = 1;
  else if (!strcmp (p, "-Werror-implicit-function-declaration"))
    mesg_implicit_function_declaration = 2;
  else if (!strcmp (p, "-Wimplicit-function-declaration"))
    mesg_implicit_function_declaration = 1;
  else if (!strcmp (p, "-Wno-implicit-function-declaration"))
    mesg_implicit_function_declaration = 0;
  else if (!strcmp (p, "-Wimplicit-int"))
    warn_implicit_int = 1;
  else if (!strcmp (p, "-Wno-implicit-int"))
    warn_implicit_int = 0;
  else if (!strcmp (p, "-Wimplicit"))
    {
      warn_implicit_int = 1;
      if (mesg_implicit_function_declaration != 2)
        mesg_implicit_function_declaration = 1;
    }
  else if (!strcmp (p, "-Wno-implicit"))
    warn_implicit_int = 0, mesg_implicit_function_declaration = 0;
  else if (!strcmp (p, "-Wwrite-strings"))
    warn_write_strings = 1;
  else if (!strcmp (p, "-Wno-write-strings"))
    warn_write_strings = 0;
  else if (!strcmp (p, "-Wcast-qual"))
    warn_cast_qual = 1;
  else if (!strcmp (p, "-Wno-cast-qual"))
    warn_cast_qual = 0;
  else if (!strcmp (p, "-Wbad-function-cast"))
    warn_bad_function_cast = 1;
  else if (!strcmp (p, "-Wno-bad-function-cast"))
    warn_bad_function_cast = 0;
  else if (!strcmp (p, "-Wpointer-arith"))
    warn_pointer_arith = 1;
  else if (!strcmp (p, "-Wno-pointer-arith"))
    warn_pointer_arith = 0;
  else if (!strcmp (p, "-Wstrict-prototypes"))
    warn_strict_prototypes = 1;
  else if (!strcmp (p, "-Wno-strict-prototypes"))
    warn_strict_prototypes = 0;
  else if (!strcmp (p, "-Wmissing-prototypes"))
    warn_missing_prototypes = 1;
  else if (!strcmp (p, "-Wno-missing-prototypes"))
    warn_missing_prototypes = 0;
  else if (!strcmp (p, "-Wmissing-declarations"))
    warn_missing_declarations = 1;
  else if (!strcmp (p, "-Wno-missing-declarations"))
    warn_missing_declarations = 0;
  else if (!strcmp (p, "-Wredundant-decls"))
    warn_redundant_decls = 1;
  else if (!strcmp (p, "-Wno-redundant-decls"))
    warn_redundant_decls = 0;
  else if (!strcmp (p, "-Wnested-externs"))
    warn_nested_externs = 1;
  else if (!strcmp (p, "-Wno-nested-externs"))
    warn_nested_externs = 0;
  else if (!strcmp (p, "-Wtraditional"))
    warn_traditional = 1;
  else if (!strcmp (p, "-Wno-traditional"))
    warn_traditional = 0;
  else if (!strcmp (p, "-Wformat"))
    warn_format = 1;
  else if (!strcmp (p, "-Wno-format"))
    warn_format = 0;
  else if (!strcmp (p, "-Wchar-subscripts"))
    warn_char_subscripts = 1;
  else if (!strcmp (p, "-Wno-char-subscripts"))
    warn_char_subscripts = 0;
  else if (!strcmp (p, "-Wconversion"))
    warn_conversion = 1;
  else if (!strcmp (p, "-Wno-conversion"))
    warn_conversion = 0;
  else if (!strcmp (p, "-Wparentheses"))
    warn_parentheses = 1;
  else if (!strcmp (p, "-Wno-parentheses"))
    warn_parentheses = 0;
  else if (!strcmp (p, "-Wreturn-type"))
    warn_return_type = 1;
  else if (!strcmp (p, "-Wno-return-type"))
    warn_return_type = 0;
  else if (!strcmp (p, "-Wmissing-braces"))
    warn_missing_braces = 1;
  else if (!strcmp (p, "-Wno-missing-braces"))
    warn_missing_braces = 0;
  else if (!strcmp (p, "-Wmain"))
    warn_main = 1;
  else if (!strcmp (p, "-Wno-main"))
    warn_main = 0;
  else if (!strcmp (p, "-Wsign-compare"))
    warn_sign_compare = 1;
  else if (!strcmp (p, "-Wno-sign-compare"))
    warn_sign_compare = 0;
  else if (!strcmp (p, "-Wmultichar"))
    warn_multichar = 1;
  else if (!strcmp (p, "-Wno-multichar"))
    warn_multichar = 0;
  else if (!strcmp (p, "-Wall"))
    {
      /* We save the value of warn_uninitialized, since if they put
	 -Wuninitialized on the command line, we need to generate a
	 warning about not using it without also specifying -O.  */
      if (warn_uninitialized != 1)
	warn_uninitialized = 2;
      warn_implicit_int = 1;
      mesg_implicit_function_declaration = 1;
      warn_return_type = 1;
      warn_unused = 1;
      warn_switch = 1;
      warn_format = 1;
      warn_char_subscripts = 1;
      warn_parentheses = 1;
      warn_missing_braces = 1;
      /* We set this to 2 here, but 1 in -Wmain, so -ffreestanding can turn
	 it off only if it's not explicit.  */
      warn_main = 2;
    }
}


/* Entry point of cc1/c++.  Decode command args, then call compile_file.
   Exit code is 35 if can't open files, 34 if fatal error,
   33 if had nonfatal errors, else success.  */
int main(int argc, char **argv)
{
  register int i;
  dd_list files, preludes;
  dd_list_pos cur;
  int version_flag = 0;
  char *p;
  char *config_file = NULL;

#ifdef TIMER_USERTIME
  reset_timer(&total_time);
#endif
  start_timer(&total_time);
  region_init();
  parse_region = newregion();
  in_prelude = FALSE;
  num_hotspots = 0;
  parsed_files = dd_new_list(parse_region);

  copy_argc = 0;
  copy_argv = xmalloc((argc + 1) * sizeof(*copy_argv));
  files = dd_new_list(parse_region);
  preludes = dd_new_list(parse_region);

  p = argv[0] + strlen (argv[0]);
  while (p != argv[0] && p[-1] != '/'
#ifdef DIR_SEPARATOR
	 && p[-1] != DIR_SEPARATOR
#endif
	 )
    --p;
  progname = p;

#ifdef SIGPIPE
  signal (SIGPIPE, pipe_closed);
#endif

  copy_argv[0] = argv[0];
  copy_argc = 1;
  for (i = 1; i < argc; i++)
    {
      int j;
      bool copy_arg = TRUE;

      /* If this is a language-specific option,
	 decode it in a language-specific way.  */
      for (j = 0; lang_options[j] != 0; j++)
	if (!strncmp (argv[i], lang_options[j],
		      strlen (lang_options[j])))
	  break;
      if (lang_options[j] != 0)
	/* If the option is valid for *some* language,
	   treat it as valid even if this language doesn't understand it.  */
	c_decode_option(argv[i]);
      else if (argv[i][0] == '-' && argv[i][1] != 0)
	{
	  register char *str = argv[i] + 1;
	  if (str[0] == 'Y')
	    str++;

	  if (!strcmp (str, "dumpbase"))
	    copy_argv[copy_argc++] = argv[i++];
	  else if (str[0] == 'f')
	    {
	      register char *p = &str[1];
	      int found = 0;

	      /* Some kind of -f option.
		 P's value is the option sans `-f'.
		 Search for it in the table of options.  */

	      for (j = 0;
		   !found && j < sizeof (f_options) / sizeof (f_options[0]);
		   j++)
		{
		  if (!strcmp (p, f_options[j].string))
		    {
		      *f_options[j].variable = f_options[j].on_value;
		      /* A goto here would be cleaner,
			 but breaks the vax pcc.  */
		      found = 1;
		    }
		  if (p[0] == 'n' && p[1] == 'o' && p[2] == '-'
		      && ! strcmp (p+3, f_options[j].string))
		    {
		      *f_options[j].variable = ! f_options[j].on_value;
		      found = 1;
		    }
		}
	    }
	  else if (!strcmp (str, "pedantic"))
	    pedantic = 1;
	  else if (!strcmp (str, "pedantic-errors"))
	    flag_pedantic_errors = pedantic = 1;
	  else if (!strcmp (str, "quiet"))
	    quiet_flag = 1;
	  else if (!strcmp (str, "version"))
	    version_flag = 1;
	  else if (!strcmp (str, "w"))
	    inhibit_warnings = 1;
	  else if (!strcmp (str, "W"))
	    {
	      extra_warnings = 1;
	      /* We save the value of warn_uninitialized, since if they put
		 -Wuninitialized on the command line, we need to generate a
		 warning about not using it without also specifying -O.  */
	      if (warn_uninitialized != 1)
		warn_uninitialized = 2;
	    }
	  else if (str[0] == 'W')
	    {
	      register char *p = &str[1];
	      int found = 0;

	      /* Some kind of -W option.
		 P's value is the option sans `-W'.
		 Search for it in the table of options.  */

	      for (j = 0;
		   !found && j < sizeof (W_options) / sizeof (W_options[0]);
		   j++)
		{
		  if (!strcmp (p, W_options[j].string))
		    {
		      *W_options[j].variable = W_options[j].on_value;
		      /* A goto here would be cleaner,
			 but breaks the vax pcc.  */
		      found = 1;
		    }
		  if (p[0] == 'n' && p[1] == 'o' && p[2] == '-'
		      && ! strcmp (p+3, W_options[j].string))
		    {
		      *W_options[j].variable = ! W_options[j].on_value;
		      found = 1;
		    }
		}

	      if (found)
		;
	      else if (!strncmp (p, "id-clash-", 9))
		{
		  char *endp = p + 9;

		  while (*endp)
		    {
		      if (*endp >= '0' && *endp <= '9')
			endp++;
		      else
			{
			  error ("Invalid option `%s'", argv[i]);
			  goto id_clash_lose;
			}
		    }
		  warn_id_clash = 1;
		  id_clash_len = atoi (str + 10);
		id_clash_lose: ;
		}
	      else if (!strncmp (p, "larger-than-", 12))
		{
		  char *endp = p + 12;

		  while (*endp)
		    {
		      if (*endp >= '0' && *endp <= '9')
			endp++;
		      else
			{
			  error ("Invalid option `%s'", argv[i]);
			  goto larger_than_lose;
			}
		    }
		  warn_larger_than = 1;
		  larger_than_size = atoi (str + 13);
		larger_than_lose: ;
		}
	    }
	  else if (!strcmp (str, "o"))
	    copy_argv[copy_argc++] = argv[i++];
	  else if (str[0] == 'G')
	    {
	      if (str[1] == '\0')
		copy_argv[copy_argc++] = argv[i++];
	    }
	  else if (!strncmp (str, "aux-info", 8))
	    {
	      if (str[8] == '\0')
		copy_argv[copy_argc++] = argv[i++];
	    }
	  else if (!strcmp(str, "config"))
	    {
	      if (i < argc - 1)
		{
		  i++;
		  config_file = strdup(argv[i]);
		}
	      else
		error ("Missing -config file");
	    }
	  else if (!strcmp(str, "prelude"))
	    {
	      if (i < argc - 1)
		{
		  i++;
		  dd_add_last(parse_region, preludes,
			      rstrdup(parse_region, argv[i]));
		}
	      else
		error("Missing -prelude file");
	    }
	  else if (!strcmp(str, "hotspots"))
	    {
	      if (i < argc - 1)
		{
		  i++;
		  num_hotspots = atoi(argv[i]);
		  if (num_hotspots < 0)
		    error("Negative value for -hotspots count");
		}
	      else
		error("Missing -hotspots count");
	    }
	  else if (!strcmp( str, "program-files"))
	    {
	      if (i < argc - 1)
		{
		  i++;
		  add_program_files(argv[i], files);
		}
	      else
		error("Missing -program-files file");
	    }
	}
      else if (argv[i][0] == '+')
	;
      else
	{
	  /* Allow wildcards, because PAM won't expand files */
	  glob_t globbuf;
	  char **cur;

	  if (glob(argv[i], 0, NULL, &globbuf))
	    {
	      /* glob returned non-zero error status; abort */
	      fprintf(stderr, "%s: file not found\n", argv[i]);
	      exit(FATAL_EXIT_CODE);
	    }
	  else
	    for (cur = globbuf.gl_pathv; *cur; cur++)
	      {
		/* Assume anything called prelude.i is a prelude file */
		if ( strlen(*cur) >= 9 &&
                    !strncmp("prelude.i", *cur + strlen(*cur) - 9, 9))
		  dd_add_last(parse_region, preludes,
			      rstrdup(parse_region, *cur));
		else
		  dd_add_last(parse_region, files,
			      rstrdup(parse_region, *cur));
	      }
	  copy_arg = FALSE;
	}

      if (copy_arg)
	copy_argv[copy_argc++] = argv[i];
    }
  copy_argv[copy_argc] = NULL;

  if (flag_casts_preserve && flag_flow_sensitive) {
    fprintf(stderr, "-fcasts-preserve currently not allowed with "
	    "-fflow_sensitive");
    exit(FATAL_EXIT_CODE);
  }

  /* Now analyze *all* of the files.  First, initialize all appropriate
     data structures. */
  init_types();
  cval_init();
  init_effects();
  init_qtype();
  init_quals();
  init_qerror();

  init_store();

  if (config_file)
    load_config_file_quals(config_file);

  /* Add const so that we can do -fconst-subtyping no matter what */
  if (!const_qual)
    {
      begin_po_qual();
      const_qual = add_qual("const");
      add_level_qual(const_qual, level_ref);
      set_po_nonprop();
      end_po_qual();
    }

  /* Add volatile so we can handle noreturn functions */
  if (!volatile_qual)
    {
      begin_po_qual();
      volatile_qual = add_qual("volatile");
      add_level_qual(volatile_qual, level_ref);
      add_sign_qual(volatile_qual, sign_eq);
      set_po_nonprop();
      end_po_qual();
    }

  /* Add noreturn, for non-returning functions */
  if (!noreturn_qual)
    {
      begin_po_qual();
      noreturn_qual = add_qual("noreturn");
      add_level_qual(noreturn_qual, level_value);
      add_sign_qual(noreturn_qual, sign_eq);
      set_po_nonprop();
      end_po_qual();
    }

  end_define_pos(); /* Allow cqual to run with no qualifiers */

  init_pam();
  init_analyze();
  found_fs_qual = FALSE; /* Force reset, since init_analyze may
			    look up some quals */

  /* Now analyze the prelude files */
  in_prelude = TRUE;
  dd_scan(cur, preludes)
    {
      char *file;
      
      file = DD_GET(char *, cur);
      fprintf(stderr, "Analyzing prelude %s\n", file);
      compile_file(file);
    }
  in_prelude = FALSE;
  
  /* Finally, analyze the source files */
  if (dd_is_empty(files))
    compile_file(0); /* If no files given, compile stdin */
  else
    dd_scan(cur, files)
    {
      char *file;
      
      file = DD_GET(char *, cur);
      fprintf(stderr, "Analyzing %s\n", file);
      compile_file(file);
    }

  if (errorcount)
    exit (FATAL_EXIT_CODE);
  finish_analyze();

  if (flag_flow_sensitive && (flag_force_flow_sensitive || found_fs_qual))
    {
      dd_list_pos cur;

      init_flow_sensitive();
      dd_scan(cur, parsed_files)
	{
	  parsed_file pf;

	  pf = DD_GET(parsed_file, cur);
	  fprintf(stderr, "Flow-sensitive analysis of %s\n", pf->name);
	  analyze_flow_sensitive(pf->program);
	}
      finish_flow_sensitive();
    }

  finish_quals();

  end_timer(&total_time);
  if (flag_statistics)
    memusage = get_memusage();

  if (flag_print_results)
    print_results();

  if (flag_pam_mode)
    enter_pam_mode();

  if (flag_statistics)
    {
      fflush(NULL);
      fprintf(stderr, "VMSize: %ldK\n", memusage / 1024);
      fprintf(stderr, "Elapsed time: %ss\n", timer_to_ascii(&total_time));
      fprintf(stderr, "Analysis complete\n");
    }

  fflush(NULL);
  exit (SUCCESS_EXIT_CODE);
  return 0;
}
