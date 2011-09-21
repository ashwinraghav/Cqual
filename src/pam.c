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

#include <ctype.h>
#include <stdio.h>
#include <string.h>
#include "analyze.h"
#include "buffer.h"
#include "color.h"
#include "containers.h"
#include "cqual.h"
#include "flow.h"
#include "pam.h"
#include "utils.h"
#include "qerror.h"
#include "qtype.h"
#include "hash.h"
#include "effect.h"

/* Size of buffer for receiving PAM commands */
#define BUF_MAX 1024

typedef void (*click_func)(const char *click, const char *file,
			   const char *name, void *arg);

struct Click
{
  click_func func;            /* Function to execute */
  void *arg;                  /* Extra argument to pass */
};

enum overlay_kind { overlay_qtype = 1,
		    overlay_fs_qtype = 2,
		    overlay_lin = 4,
		    overlay_anchor = 8 };

struct Overlay
{
  int kind;  /* bitmask of overlay_kinds */

  /* for everything */
  const char *name;

  /* for overlay_qtype */
  qtype qtype;

  /* for overlay_fs_qtype */
  qtype fs_qtype;
  store store;       /* also for overlay_lin */

  /* for overlay_lin */
  aloc aloc;

  /* nothing new for overlay_anchor */
};

struct File
{
  bool loaded;
  const char *link;
  pam_overlay_map overlays;
};

typedef struct Error
{
  const char *func;
  location loc;
  severity sev;
  const char *message;
  const char *detail;
  const char *link;
} *pam_error;

/**************************************************************************
 *                                                                        *
 * Global Variables and Prototypes                                        *
 *                                                                        *
 **************************************************************************/

static region pam_region = NULL;
static pam_buffer_map buffer_clicks;
static pam_file_map pam_files;
static dd_list pam_errors;       /* list : pam_error */
static unsigned int next_link = 0;
static growbuf pam_vprintf_scratch_buf = NULL;

/* Reset any side-effected stuff */
void init_pam(void)
{
  pam_vprintf_scratch_buf = NULL;
  if (pam_region)
    deleteregion(pam_region);
  pam_region = newregion();
  buffer_clicks = make_pam_buffer_map(pam_region, 7);
  pam_files = make_pam_file_map(pam_region, 37);
  pam_errors = dd_new_list(pam_region);
  next_link = 0;
  pam_vprintf_scratch_buf = growbuf_new(pam_region, 256);
}


/*
 * Some prototypes
 */

void pam_jump_file(const char *click_name, const char *file,
		   const char *name, location loc);
const char *get_next_link(void);

/**************************************************************************
 *                                                                        *
 * Low-level PAM communication                                            *
 *                                                                        *
 **************************************************************************/

void pam_clear_buffer(const char *);      /* Erase the buffer contents */
void pam_change_buffer(const char *);     /* Switch to the buffer */
void pam_set_lower_buffer(const char *);   /* ? */
void pam_set_upper_buffer(const char *);     /* ? */
void pam_close_buffer(const char *);      /* Close (delete?) the buffer */
void pam_split_buffer(const char *);      /* ? */
void pam_goto_char_buffer(const char *, unsigned long);
                                          /* Go to character pos in a buffer */
void pam_goto_anchor_buffer(const char *, const char *);
                                          /* Go to anchor in a buffer */

void pam_clear_file(const char *);        /* See above */
void pam_change_file(const char *);
void pam_set_lower_file(const char *);
void pam_set_upper_file(const char *);
void pam_close_file(const char *);
void pam_split_file(const char *);
void pam_goto_char_file(const char *, unsigned long);
void pam_goto_anchor_file(const char *, const char *);

int pam_waiting(void);                  /* Tell PAM we've finished dealing
					   with the last action and we're
					   waiting for a click */
void pam_debug(const char *, ...);        /* Print a debugging message */
int pam_message(const char *, ...);       /* Print a text message */
void pam_default_path(const char *);      /* Set the default path */

void pam_blobs_begin(void);               /* Begin printing marked-up text */
void pam_blobs_end(void);                 /* End printing marked-up text */
void pam_overlays_begin(void);            /* Begin marking-up file */
void pam_overlays_end(void);              /* End marking-up file */

void pam_markup_file(const char *f);      /* Start marking up file f */
void pam_markup_buffer(const char *b);    /* Start marking up buffer b */


/* Add some text to a mark-up */
int pam_add_text(const char *, ...);

/* Add a hyperlink to a markup.  ``text'' is displayed marked up with
   color.  ``name'' is the internal name of the hyperlink. */
void pam_add_link(const char *name, const char *color, const char *text);

/* Add some marked-up text -- no link */
void pam_add_marked_text(const char *color, const char *text);

/* Add a hyperlink on top of some text.  The text to add a link to
   ranges from positions start-end in the current markup buffer, and
   is marked up with color.  ``name'' is the internal name of the
   hyperlink. */
void pam_add_overlink(const char *name, const char *color, int start, int end);

void pam_add_overlink_anchor(const char *name, const char *anchor_name,
			     const char *color, int start, int end);

/* Add a color on top of some text -- no link */
void pam_add_overmark(const char *color, int start, int end);

/* Definitions */

#define pam_printf printf
#define pam_vprintf vprintf
#define pam_putc(c) putc(c, stdout)
#define pam_getc()  getc(stdin)

#define pam_window_buffer_msg "buffer"
#define pam_window_file_msg "file"
#define pam_click_msg "click"
#define pam_shift_click_msg "shift-click"

/* Print out a string, adding escapes where necessary */
int pam_print_escaped_string(const char *s)
{
  int count = 0;

  pam_putc('\"');
  while (*s)
  {
    switch (*s)
    {
    case '\\': pam_printf("\\\\"); break;
    case '\"': pam_printf("\\\""); break;
    case '\n': pam_printf("\\n"); break;
    default:
      if (isprint(*s))
	pam_putc(*s);
      else
	fail("Error: 0x%x not printable\n", *s);
    }
    s++;
    count++;
  }
  pam_putc('\"');
  return count;
}

int pam_vprintf_escaped(const char *fmt, va_list args)
{
  growbuf_reset(pam_vprintf_scratch_buf);
  gvprintf(pam_vprintf_scratch_buf, fmt, args);
  return pam_print_escaped_string(growbuf_contents(pam_vprintf_scratch_buf));
}

void pam_printf_escaped(const char *fmt, ...)
{
  va_list args;
  va_start(args, fmt);
  pam_vprintf_escaped(fmt, args);
}

typedef enum { pam_clear_window, pam_change_window, pam_set_lower_window,
	       pam_set_upper_window, pam_close_window, pam_split_window
} pam_op_kind;

void pam_window_op(pam_op_kind op, const char *wname, const char *kind)
{
  pam_putc('(');
  switch (op)
    {
    case pam_clear_window: pam_printf("pam-clear-window"); break;
    case pam_change_window: pam_printf("pam-change-window"); break;
    case pam_set_lower_window: pam_printf("pam-set-window-down"); break;
    case pam_set_upper_window: pam_printf("pam-set-window-up"); break;
    case pam_close_window: pam_printf("pam-close-window"); break;
    case pam_split_window: pam_printf("pam-split-window"); break;
    default:
      fail("Unexpected window op %d\n", op);
    }
  pam_printf(" (");
  pam_print_escaped_string(wname);
  pam_putc(' ');
  pam_print_escaped_string(kind);
  pam_printf("))\n");
}

#define mkpam_buffer_op(op) void pam_ ## op ## _buffer(const char *wname) \
    { pam_window_op(pam_ ## op ## _window, wname, pam_window_buffer_msg); }

#define mkpam_file_op(op) void pam_ ## op ## _file(const char *wname) \
    { pam_window_op(pam_ ## op ## _window, wname, pam_window_file_msg); }

mkpam_buffer_op(clear);
mkpam_buffer_op(change);
mkpam_buffer_op(set_lower);
mkpam_buffer_op(set_upper);
mkpam_buffer_op(close);
mkpam_buffer_op(split);
mkpam_file_op(clear);
mkpam_file_op(change);
mkpam_file_op(set_lower);
mkpam_file_op(set_upper);
mkpam_file_op(close);
mkpam_file_op(split);

void pam_goto_char_buffer(const char *buffer, unsigned long pos)
{
  pam_printf("(pam-goto-char (");
  pam_print_escaped_string(buffer);
  pam_putc(' ');
  pam_print_escaped_string(pam_window_buffer_msg);
  pam_printf(") %ld)\n", pos);
}

void pam_goto_char_file(const char *buffer, unsigned long pos)
{
  pam_printf("(pam-goto-char (");
  pam_print_escaped_string(buffer);
  pam_putc(' ');
  pam_print_escaped_string(pam_window_file_msg);
  pam_printf(") %ld)\n", pos);
}

void pam_goto_anchor_buffer(const char *buffer, const char *anchor)
{
  pam_printf("(pam-goto-anchor (");
  pam_print_escaped_string(buffer);
  pam_putc(' ');
  pam_print_escaped_string(pam_window_buffer_msg);
  pam_printf(") ");
  pam_print_escaped_string(anchor);
  pam_printf(")\n");
}

void pam_goto_anchor_file(const char *buffer, const char *anchor)
{
  pam_printf("(pam-goto-anchor (");
  pam_print_escaped_string(buffer);
  pam_putc(' ');
  pam_print_escaped_string(pam_window_file_msg);
  pam_printf(") ");
  pam_print_escaped_string(anchor);
  pam_printf(")\n");
}

int pam_waiting(void)
{
  return pam_printf("(pam-waiting)\n");
}

void pam_debug(const char *str, ...)
{
  va_list args;

  va_start(args, str);
  pam_printf("(pam-debug ");
  pam_vprintf_escaped(str, args);
  pam_printf(")\n");
}

int pam_message(const char *str, ...)
{
  va_list args;
  int count = 0;

  va_start(args, str);
  count += pam_printf("(pam-message ");
  count += pam_vprintf_escaped(str, args);
  count += pam_printf(")\n");
  return count;
}

void pam_default_path(const char *path)
{
  pam_printf("(pam-default-path ");
  pam_print_escaped_string(path);
  pam_printf(")\n");
}

void pam_blobs_begin(void) { pam_printf("(pam-blobs"); }
void pam_blobs_end(void)   { pam_printf(")\n"); }

void pam_overlays_begin(void) { pam_printf("(pam-overlays"); }
void pam_overlays_end(void)   { pam_printf(")\n"); }

void pam_markup_file(const char *name)
{
  pam_printf(" (file (");
  pam_print_escaped_string(name);
  pam_printf(" ");
  pam_print_escaped_string(pam_window_file_msg);
  pam_printf("))");
}

void pam_markup_buffer(const char *name)
{
  pam_printf(" (file (");
  pam_print_escaped_string(name);
  pam_printf(" ");
  pam_print_escaped_string(pam_window_buffer_msg);
  pam_printf("))");
}

int pam_add_text(const char *text, ...)
{
  int result;
  va_list args;

  va_start(args, text);
  pam_printf(" (text ");
  result = pam_vprintf_escaped(text, args);
  pam_printf(")");
  return result;
}

/*
void pam_print_color(pam_color c)
{
  switch (c)
    {
    case pam_color_1: pam_printf("(pam-color-1)"); break;
    case pam_color_2: pam_printf("(pam-color-2)"); break;
    case pam_color_3: pam_printf("(pam-color-3)"); break;
    case pam_color_4: pam_printf("(pam-color-4)"); break;
    case pam_color_5: pam_printf("(pam-color-5)"); break;
    case pam_color_6: pam_printf("(pam-color-6)"); break;
    case pam_color_7: pam_printf("(pam-color-7)"); break;
    case pam_color_8: pam_printf("(pam-color-8)"); break;
    case pam_color_mouse: pam_printf("(pam-color-mouse)"); break;
    default:
      fail("Unexpected markup color %d\n", c);
    }
}
*/

void pam_add_markup(int start, int end, const char *color, const char *text)
{
  pam_printf(" (markup (%d %d) (%s) ", start, end, color);
  pam_print_escaped_string(text);
  pam_printf(")");
}

void pam_add_hyper(const char *name, int start, int end, const char *color,
		   const char *text)
{
  pam_printf(" (hyper ");
  pam_print_escaped_string(name);
  pam_printf(" (%d %d) (%s) ", start, end, color);
  pam_print_escaped_string(text);
  pam_printf(")");
}

void pam_add_hyper_anchor(const char *name, const char *anchor_name,
			  int start, int end, const char *color,
			  const char *text)
{
  pam_printf(" (hyper-anchor ");
  pam_print_escaped_string(name);
  pam_printf(" ");
  pam_print_escaped_string(anchor_name);
  pam_printf(" (%d %d) (%s) ", start, end, color);
  pam_print_escaped_string(text);
  pam_printf(")");
}

void pam_add_anchor(const char *name, int start, int end) {
  pam_printf(" (anchor ");
  pam_print_escaped_string(name);
  pam_printf(" (%d %d))", start, end);
}

/* Add a hyperlink to a markup.  ``text'' is displayed marked up with
   color.  ``name'' is the internal name of the hyperlink. */
void pam_add_link(const char *name, const char *color, const char *text)
{
  pam_add_hyper(name, -1, -1, color, text);
}

/* Add some marked-up text -- no link */
void pam_add_marked_text(const char *color, const char *text)
{
  pam_add_markup(-1, -1, color, text);
}

/* Add a hyperlink on top of some text.  The text to add a link to
   ranges from positions start-end in the current markup buffer, and
   is marked up with color.  ``name'' is the internal name of the
   hyperlink. */
void pam_add_overlink(const char *name, const char *color, int start, int end)
{
  pam_add_hyper(name, start, end, color, "");
}

void pam_add_overlink_anchor(const char *name, const char *anchor_name,
			     const char *color, int start, int end)
{
  pam_add_hyper_anchor(name, anchor_name, start, end, color, "");
}

/* Add a color on top of some text -- no link */
void pam_add_overmark(const char *color, int start, int end)
{
  pam_add_markup(start, end, color, "");
}

/**************************************************************************
 *                                                                        *
 * PAM environments                                                       *
 *                                                                        *
 * Functions for remembering what to do when the user clicks on stuff.    *
 *                                                                        *
 **************************************************************************/

#define pam_any_click ""

/* Add a dispatch to the click dispatch table for buffer bname on
   click cname.  If the click was already there, override it
   with a new click. */
void pam_add_buffer_click(const char *bname, const char *cname,
			  click_func func, void *arg)
{
  pam_click_map clicks;
  pam_click c;

  if (!pam_buffer_map_lookup(buffer_clicks, bname, &clicks))
    {
      clicks = make_pam_click_map(pam_region, 37);
      insist(pam_buffer_map_insert(buffer_clicks, bname, clicks));
    }

  c = ralloc(pam_region, struct Click);
  c->func = func;
  c->arg = arg;

  pam_click_map_insert(clicks, cname, c);
}

/* Remove a dispatch from the click dispatch table for buffer bname on
   click cname.  Aborts if click wasn't already there. */
void pam_remove_buffer_click(const char *bname, const char *cname)
{
  pam_click_map clicks;

  insist(pam_buffer_map_lookup(buffer_clicks, bname, &clicks));
  insist(pam_click_map_remove(clicks, cname));
}

/* Look up the click for a particular buffer.  Return NULL if
   no click exists. */
pam_click pam_get_buffer_click(const char *bname, const char *cname)
{
  pam_click_map clicks;
  pam_click c;

  if (!pam_buffer_map_lookup(buffer_clicks, bname, &clicks))
    return NULL;
  if (!pam_click_map_lookup(clicks, cname, &c) &&
      !pam_click_map_lookup(clicks, pam_any_click, &c))
    return NULL;
  return c;
}

/**************************************************************************
 *                                                                        *
 * About                                                                  *
 *                                                                        *
 **************************************************************************/

#define pam_about_buffer "*About*"
#define pam_about_click "abt"
#define pam_home_click "home"

/* (Re)create the about buffer */
void pam_create_about_buffer(void)
{
  pam_clear_buffer(pam_about_buffer);

  pam_blobs_begin();
  pam_markup_buffer(pam_about_buffer);
  pam_add_text(
"cqual is Copyright (c) 2000 The Regents of the University of California.\n"
"All rights reserved.\n"
"\n"
"Permission to use, copy, modify, and distribute this software for any\n"
"purpose, without fee, and without written agreement is hereby granted,\n"
"provided that the above copyright notice and the following two\n"
"paragraphs appear in all copies of this software.\n"
"\n"
"IN NO EVENT SHALL THE UNIVERSITY OF CALIFORNIA BE LIABLE TO ANY PARTY FOR\n"
"DIRECT, INDIRECT, SPECIAL, INCIDENTAL, OR CONSEQUENTIAL DAMAGES ARISING OUT\n"
"OF THE USE OF THIS SOFTWARE AND ITS DOCUMENTATION, EVEN IF THE UNIVERSITY\n"
"OF CALIFORNIA HAS BEEN ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.\n"
"\n"
"THE UNIVERSITY OF CALIFORNIA SPECIFICALLY DISCLAIMS ANY WARRANTIES,\n"
"INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY\n"
"AND FITNESS FOR A PARTICULAR PURPOSE.  THE SOFTWARE PROVIDED HEREUNDER IS\n"
"ON AN \"AS IS\" BASIS, AND THE UNIVERSITY OF CALIFORNIA HAS NO OBLIGATION\n"
"TO PROVIDE MAINTENANCE, SUPPORT, UPDATES, ENHANCEMENTS, OR MODIFICATIONS\n\n");
  pam_add_link(pam_home_click, pam_color_6, "Back");
  pam_add_text("\n");
  pam_blobs_end();
}

/* Bring up an about window */
void pam_about_click_func(const char *click, const char *file,
			  const char *name, void *arg)
{
  pam_close_buffer(pam_about_buffer);
  pam_set_upper_buffer(pam_about_buffer);
  pam_change_buffer(pam_about_buffer);
  pam_create_about_buffer();
}

/**************************************************************************
 *                                                                        *
 * Hotspots                                                               *
 *                                                                        *
 **************************************************************************/

void pam_qual_click_func(const char *click, const char *file,
			 const char *name, void *arg);
int pam_add_qual_link(qual q, const char *link_name);

#define pam_hotspots_buffer "*Hotspots*"
#define pam_hotspots_click "hotspots"

void pam_create_hotspots_buffer(void)
{
  int i;

  pam_clear_buffer(pam_hotspots_buffer);
  pam_blobs_begin();
  pam_markup_buffer(pam_hotspots_buffer);
  pam_add_text("Potential qualifier variable hotspots:\n\n");
  for (i = 0; i < num_hotspots && hotspots[i] != NULL; i++)
    {
      qual q;

      q = hotspots[i];
      pam_add_qual_link(q, unique_name_qual(q));
      pam_add_text(" (%d)\n", error_count_qual(q));
    }
  pam_blobs_end();
  pam_add_buffer_click(pam_hotspots_buffer,
		       pam_any_click,
		       pam_qual_click_func,
		       NULL);
}

/* Bring up the hotspots window */
void pam_hotspots_click_func(const char *click, const char *file,
			     const char *name, void *arg)
{
  pam_close_buffer(pam_hotspots_buffer);
  pam_set_lower_buffer(pam_hotspots_buffer);
  pam_change_buffer(pam_hotspots_buffer);
  pam_create_hotspots_buffer();
}

/* Return TRUE iff there are hotspots */
bool exist_hotspots(void)
{
  return (hotspots && hotspots[0]);
}

/**************************************************************************
 *                                                                        *
 * Undefined/Unannotated Globals                                          *
 *                                                                        *
 * These functions are parameterized by whether to list undefined globals *
 * or unannotated varargs functions.                                      *
 *                                                                        *
 **************************************************************************/

#define pam_undefineds_buffer "*Undefined Globals*"
#define pam_undefineds_click "undef"
#define pam_varargs_buffer "*Non-Polymorphic Varargs Functions*"
#define pam_varargs_click "varargs"

/* Which kind of global to deal with */
typedef enum { undefined_globals, varargs_globals } kind_globals;

typedef bool(*ddecl_pred)(data_declaration ddecl, store s);

typedef struct pred_info {
  ddecl_pred pred;
  const char *buffer;
  store store;
} *pred_info;

/* Make a hyperlink to an undefined global name */
void pam_markup_global(data_declaration ddecl, void *arg)
{
  pred_info pi;

  pi = (pred_info) arg;
  if (pi->pred(ddecl, pi->store))
    {
      pam_add_link(ddecl->name, pam_color_1, ddecl->name);
      pam_add_text("\n");
      pam_add_buffer_click(pi->buffer,
			   ddecl->name,
			   (click_func) pam_jump_file,
			   location_declaration(ddecl->ast));
    }
}

/* (Re)create the buffer listing k-kinded globals */
void pam_create_globals_buffer(kind_globals k)
{
  struct pred_info pi;
  const char *title;

  switch (k)
    {
    case undefined_globals:
      title = "Undefined Globals";
      pi.pred = is_undefined_global;
      pi.buffer = pam_undefineds_buffer;
      pi.store = global_store;
      break;
    case varargs_globals:
      title = "Non-Polymorphic Varargs Functions";
      pi.pred = is_unannotated_global;
      pi.buffer = pam_varargs_buffer;
      pi.store = global_store;
      break;
    default:
      fail("Unexpected kind_globals %x\n", k);
    }
  pam_clear_buffer(pi.buffer);
  pam_blobs_begin();
  pam_markup_buffer(pi.buffer);
  pam_add_text("%s:\n\n", title);
  traverse_globals(pam_markup_global, &pi);
  pam_blobs_end();
}

/* Bring up the undefined globals window */
void pam_undefineds_click_func(const char *click, const char *file,
			       const char *name, void *arg)
{
  pam_close_buffer(pam_undefineds_buffer);
  pam_set_lower_buffer(pam_undefineds_buffer);
  pam_change_buffer(pam_undefineds_buffer);
  pam_create_globals_buffer(undefined_globals);
}
 
/* Bring up the unannotated varargs window */
void pam_varargs_click_func(const char *click, const char *file,
			    const char *name, void *arg)
{
  pam_close_buffer(pam_varargs_buffer);
  pam_set_lower_buffer(pam_varargs_buffer);
  pam_change_buffer(pam_varargs_buffer);
  pam_create_globals_buffer(varargs_globals);
}

typedef struct pred_info2 {
  ddecl_pred pred;
  bool flag;
  store store;
} *pred_info2;
 
/* arg is a pred_info.  Set arg->flag to TRUE if arg->pred is true of
   ddecl.  Otherwise leave arg->flag alone. */
void set_if_pred(data_declaration ddecl, void *arg)
{
  pred_info2 pi;

  pi = (pred_info2) arg;
  if (pi->pred(ddecl, pi->store))
    pi->flag = TRUE;
}

/* Return TRUE iff there is some global satisfying pred */
bool exist_globals(ddecl_pred pred)
{
  struct pred_info2 pi;

  pi.pred = pred;
  pi.flag = FALSE;
  pi.store = global_store;
  traverse_globals(set_if_pred, &pi);
  return pi.flag;
}

/* Return TRUE iff there is some undefined but used global */
bool exist_undefineds(void)
{
  return exist_globals(is_undefined_global);
}

/* Return TRUE iff there is some unannotated (global) varargs function */
bool exist_varargs(void)
{
  return exist_globals(is_unannotated_global);
}

/**************************************************************************
 *                                                                        *
 * Qualified Types                                                        *
 *                                                                        *
 **************************************************************************/

#define pam_results_buffer "*Qualifier Results*"
#define pam_type_buffer "*Types*"

void pam_qtype_click_func(const char *click, const char *file,
			  const char *name, void *arg);
void pam_qual_click_func(const char *click, const char *file,
			 const char *name, void *arg);

/* Add a hyperlink for a qualifier labeled with link_name. */
int pam_add_qual_link(qual q, const char *link_name)
{
  const char *name = name_qual(q);
  const char *color = color_qual(q);
  if (!color)
    color = pam_color_noqual;
  if (variable_qual(q))
    pam_add_link(link_name, color, name);
  else
    {
      assert(constant_qual(q));
      pam_add_marked_text(color, name);
    }
  return strlen(name);
}

/* Called indirectly by pam_click_dispatch to print out a qualifier.
   Prints the qualifier, and also adds the qual to our list of
   overlays. */
int pam_quals_printf(printf_func pf, qual q)
{
  assert(pf == pam_add_text);
  return pam_add_qual_link(q, unique_name_qual(q));
}

/* Show a qualified type */
void pam_qtype_click_func(const char *click, const char *file,
			  const char *name, void *arg)
{
  void *addr;
  /* pam_message("Generic click to ptr %s in file %s", name, file); */
  /* It's a dispatch in a file with overlays */
  
  if (sscanf(name, "%p", &addr) == 0)
    pam_message("Invalid click on ``%s'' in file ``%s''", name, file);
  else
    {
      pam_overlay overlay = (pam_overlay) addr;
      pam_close_buffer(pam_type_buffer);
      pam_set_lower_buffer(pam_type_buffer);
      pam_change_buffer(pam_type_buffer);
      pam_clear_buffer(pam_type_buffer);
      pam_add_buffer_click(pam_type_buffer, pam_any_click,
			   pam_qual_click_func, NULL);
      pam_blobs_begin();
      pam_markup_buffer(pam_type_buffer);

      if (overlay->kind & overlay_qtype)
	{
	  if (overlay->kind & overlay_fs_qtype)
	    pam_add_text("(standard) ");

	  pam_add_text("%s: ", overlay->name);
	  print_qtype_qf(pam_add_text, pam_quals_printf, overlay->qtype,
			 NULL, FALSE);
	  pam_add_text("\n");
	}

      if (overlay->kind & overlay_fs_qtype)
	{
	  pam_add_text("(flow-sensitive) %s: ", overlay->name);
	  print_qtype_qf(pam_add_text, pam_quals_printf, 
			 overlay->fs_qtype,
			 overlay->store, FALSE);
	}

      if (overlay->kind & overlay_lin)
	{
	  pam_add_text("\n");
	  pam_add_text("Linearity : ");
	  pam_add_text("%d", lin_from_store(overlay->store,
					    overlay->aloc));
	}

      pam_blobs_end();
    }
}

/**************************************************************************
 *                                                                        *
 * Qualifiers                                                             *
 *                                                                        *
 **************************************************************************/

static qual last_printed = NULL;
static int num_pos = 0, num_neg = 0, num_eq = 0;

void pam_print_const_qual(qual q, void *arg)
{
  assert(constant_qual(q));
  pam_add_text(" ");
  pam_add_qual_link(q, "");
  switch (sign_qual(q))
    {
    case sign_pos: num_pos++; break;
    case sign_neg: num_neg++; break;
    case sign_eq: num_eq++; break;
    default: fail("Unexpected qualifier sign %x\n", sign_qual(q));
    }
}

void pam_print_edge(qual left, qual right, location loc, void *arg)
{
  const char *s, *arrow;
  static int left_size;
  qual to_print;

  if (!last_printed)
    {
      qual init_qual;

      if (constant_qual(left))
	{
	  assert(!constant_qual(right));
	  init_qual = left;
	  to_print = right;
	  arrow = "<=";
	}
      else
	{
	  assert(!constant_qual(left));
	  init_qual = right;
	  to_print = left;
	  arrow = ">=";
	}

      s = unique_name_qual(init_qual);
      left_size = pam_add_qual_link(init_qual, s);
      if (flag_ugly)
	left_size += pam_add_text(" %s ", s);
      else
	left_size += pam_add_text(" ");
    }
  else
    {
      pam_add_text("%*s", left_size, "");

      if (eq_qual(last_printed, left))
	{
	  to_print = right;
	  arrow = "<=";
	}
      else
	{
	  assert(eq_qual(last_printed, right));
	  to_print = left;
	  arrow = ">=";
	}
    }

  if (loc)
    {
      const char *name;

      name = get_next_link();
      pam_add_link(name, pam_color_8, arrow);
      pam_add_buffer_click(pam_type_buffer,
			   name,
			   (click_func) pam_jump_file,
			   loc);
    }
  else
    pam_add_text(arrow);
  pam_add_text(" ");
  s = unique_name_qual(to_print);
  pam_add_qual_link(to_print, s);
  if (flag_ugly)
    pam_add_text(" %s\n", s);
  else
    pam_add_text("\n");
  last_printed = to_print;
}

/* Print path from c to q */
void pam_print_path_to_qual(qual c, qual q)
{
  last_printed = NULL;
  traverse_shortest_path_edges(q, c, pam_print_edge, NULL);
}

void pam_qual_mouse_2_click_func(const char *file, const char *name, void *arg)
{
  void *addr;
  qual q;

  assert(!arg);
  if (sscanf(name, "%p", &addr) == 0)
    {
      pam_message("Invalid click on ``%s'' in buffer ``%s''", name, file);
      return;
    }
  q = (qual) addr;
  /*  print_qual_graph(q, "qual.dot", TRUE);*/
  pam_close_buffer(pam_type_buffer);
  pam_set_lower_buffer(pam_type_buffer);
  pam_change_buffer(pam_type_buffer);
  pam_clear_buffer(pam_type_buffer);
  pam_blobs_begin();
  pam_markup_buffer(pam_type_buffer);

  /* Reset our state */
  num_pos = 0;
  num_neg = 0;
  num_eq = 0;

  /* Print the qualifier bounds */
  if (flag_ugly)
    pam_add_text("%s %s: ", name_qual(q), unique_name_qual(q));
  else
    pam_add_text("%s: ", name_qual(q));
  scan_qual_bounds(q, pam_print_const_qual, NULL);
  pam_add_text("\n\n");

  /* Print the path(s) to the bounds */
  /*pam_remove_buffer_clicks(pam_type_buffer);*/
  if (num_pos + num_neg + num_eq > 0)
    {
      if (num_neg <= 1 && num_pos <= 1 && num_eq == 0)
	{
	  /* If we've got at most one positive and one negative qualifier,
	     do the standard tainting-style edge printing */
	  last_printed = NULL;
	  traverse_shortest_path_edges(q, NULL, pam_print_edge, NULL);
	}
      else
	/* Otherwise, print paths for each qualifier separately */
	scan_qual_bounds(q, (qual_traverse_fn) pam_print_path_to_qual, q);
    }
  else
    pam_add_text("No qualifiers\n");

  pam_blobs_end();
}

/* Jump to the location defining a qualifier. */
void pam_qual_shift_mouse_2_click_func(const char *file, const char *name,
				       void *arg)
{
  void *addr;
  qual q;
  location loc;

  /* They clicked on a qualifier in a qtype */
  if (sscanf(name, "%p", &addr) == 0)
    {
      pam_message("Invalid click on ``%s'' in buffer ``%s''", name, file);
      return;
    }
  q = (qual) addr;
  loc = location_qual(q);
  if (loc)
    pam_jump_file(pam_click_msg, file, name, loc);
  else
    pam_message("No location for %s", name_qual(q));
}

/* Handle click on a qualifier. */
void pam_qual_click_func(const char *click, const char *file,
			 const char *name, void *arg)
{
  if (!strcmp(click, pam_click_msg))
    pam_qual_mouse_2_click_func(file, name, arg);
  else if (!strcmp(click, pam_shift_click_msg))
    pam_qual_shift_mouse_2_click_func(file, name, arg);
  else
    fail("Unexpected click ``%s''\n", click);
}

/**************************************************************************
 *                                                                        *
 * Files                                                                  *
 *                                                                        *
 **************************************************************************/

/* Load in file and mark it up. */
void pam_load_file(const char *click_name, const char *file_containing_link,
		   const char *name, char *filename)
{
  pam_file f;
  pam_overlay_map_scanner oms;
  pam_overlay_map_scanner_sorted omss;
  location loc;
  pam_overlay overlay;
  int numOverlays = 0;

  pam_close_file(filename);
  pam_set_upper_file(filename);
  pam_change_file(filename);

  if (!pam_file_map_lookup(pam_files, filename, &f))
    {
      pam_message("Asked to add overlays to unknown file ``%s''", filename);
      return;
    }

  if (f->loaded)
    /* Assume already loaded info */
    return;

  pam_message("Adding overlays to ``%s''", filename);

  pam_overlays_begin();
  pam_markup_file(filename);
  if (flag_pam_html)
    pam_overlay_map_scan_sorted(f->overlays, location_cmp, &omss);
  else
    pam_overlay_map_scan(f->overlays, &oms);

  while (flag_pam_html ?
	 pam_overlay_map_next_sorted(&omss, &loc, &overlay) :
	 pam_overlay_map_next(&oms, &loc, &overlay))
    {
      const char *color = NULL;
      int name_length = overlay->name ? strlen(overlay->name) : 1;

      if (overlay->kind & overlay_qtype)
	color = combine_colors_pam(color,
				   color_qtype(overlay->qtype, NULL));

      if (overlay->kind & overlay_fs_qtype)
	color = combine_colors_pam(color,
				   color_qtype(overlay->fs_qtype,
					       overlay->store));

      if (!color && (overlay->kind & (overlay_qtype | overlay_fs_qtype)))
	color = pam_color_noqual;

      if (overlay->kind & overlay_lin)
	{
	  if (lin_from_store(overlay->store, overlay->aloc) == 2)
	    color = pam_color_6;
	  else
	    color = pam_color_7;
	}

      if (color != NULL)
	{
	  if (overlay->kind & overlay_anchor)
	    pam_add_overlink_anchor(ptr_to_ascii(overlay),
				    ptr_to_ascii(overlay),
				    color,
				    loc->filepos + 1,
				    loc->filepos + 1 + name_length);
	  else
	    pam_add_overlink(ptr_to_ascii(overlay),
			     color,
			     loc->filepos + 1, /* emacs off by one */
			     loc->filepos + 1 + name_length);
	}
      else
	pam_add_anchor(ptr_to_ascii(overlay),
		       loc->filepos + 1,
		       loc->filepos + 1 + name_length);

      numOverlays++;
      if (numOverlays > 1000)
	{
	  pam_overlays_end();
	  pam_overlays_begin();
	  pam_markup_file(filename);
	  numOverlays = 0;
	}
    }
  pam_overlays_end();
  pam_message("done.");
  f->loaded = TRUE;
}

/* Load in file, mark it up, and jump to location */
void pam_jump_file(const char *click_name, const char *file,
		   const char *name, location loc)
{
  pam_file f;
  pam_overlay overlay;

  pam_load_file(pam_click_msg, file, name, loc->filename);

  if (flag_pam_html)
    {
      insist(pam_file_map_lookup(pam_files, loc->filename, &f));
      insist(pam_overlay_map_lookup(f->overlays, loc, &overlay));
      pam_goto_anchor_file(loc->filename, ptr_to_ascii(overlay));
    }
  else
    /* Emacs:  first character in the file is at position 1 */
    pam_goto_char_file(loc->filename, loc->filepos + 1);
}

/* Return the name of the next free link */
const char *get_next_link(void)
{
  return inttostr(pam_region, next_link++);
}

/* Add a file to the list of analyzed files, and add it to the
   buffer click table. */
void pam_add_file(const char *name)
{
  pam_file f;
  char *local_name;

  f = ralloc(pam_region, struct File);
  f->loaded = FALSE;
  f->link = get_next_link();
  f->overlays = make_pam_overlay_map(pam_region, 177);

  local_name = rstrdup(pam_region, name);
  insist(pam_file_map_insert(pam_files, local_name, f));
  pam_add_buffer_click(pam_results_buffer, f->link,
		       (click_func) pam_load_file, (void *) local_name);
}

/* Add an error message, and add it to the buffer click table */
void pam_add_error(const char *func, location loc, severity sev,
		   const char *fmt, va_list args)
{
  pam_error e = ralloc(pam_region, struct Error);

  e->func = func;
  e->loc = loc;
  e->sev = sev;
  e->message = rvsprintf(pam_region, fmt, args);
  e->link = get_next_link();
  if (growbuf_empty(qual_error_detail) && growbuf_empty(qtype_error_detail))
    e->detail = NULL;
  else if (growbuf_empty(qual_error_detail))
    e->detail = rstrdup(pam_region, growbuf_contents(qtype_error_detail));
  else
    e->detail = rstrdup(pam_region, growbuf_contents(qual_error_detail));

  dd_add_last(pam_region, pam_errors, e);
  pam_add_buffer_click(pam_results_buffer, e->link,
		       (click_func) pam_jump_file, (void *) e->loc);
  if (loc)
    pam_add_overlay_anchor(loc);
}

/* Add an overlay to a file.  Adds the file if it doesn't already
   exist. */
void pam_add_overlay_file(location loc, const char *idname, qtype qt)
{
  pam_file f;
  pam_overlay overlay;

  if (!pam_file_map_lookup(pam_files, loc->filename, &f))
    {
      pam_add_file(loc->filename);
      insist(pam_file_map_lookup(pam_files, loc->filename, &f));
    }

  /*  printf("Adding overlay %s to %s\n", idname, files->filename);*/
  if (pam_overlay_map_lookup(f->overlays, loc, &overlay))
    {
      overlay->kind |= overlay_qtype;
      if (!overlay->name)
	overlay->name = idname;
      else
	/* names must match */
	assert(!strcmp(overlay->name, idname));
      assert(!overlay->qtype);                   /* no prev qtype */
      overlay->qtype = qt;
    }
  else
    {
      overlay = ralloc(pam_region, struct Overlay);
      overlay->kind = overlay_qtype;
      overlay->name = idname;
      overlay->qtype = qt;
      overlay->fs_qtype = NULL;
      overlay->store = NULL;
      overlay->aloc = NULL;

      insist(pam_overlay_map_insert(f->overlays, loc, overlay));
    }
}

/* Add an anchor, i.e., something we might want to jump to */
void pam_add_overlay_anchor(location loc)
{
  pam_file f;
  pam_overlay overlay;

  if (!flag_pam_html)
    /* don't do this in standard PAM mode */
    return;

  if (!pam_file_map_lookup(pam_files, loc->filename, &f))
    {
      pam_add_file(loc->filename);
      insist(pam_file_map_lookup(pam_files, loc->filename, &f));
    }

  /*  printf("Adding overlay %s to %s\n", idname, files->filename);*/
  if (pam_overlay_map_lookup(f->overlays, loc, &overlay))
    {
      overlay->kind |= overlay_anchor;
    }
  else
    {
      overlay = ralloc(pam_region, struct Overlay);
      overlay->kind = overlay_anchor;
      overlay->name = NULL;
      overlay->qtype = NULL;
      overlay->fs_qtype = NULL;
      overlay->store = NULL;
      overlay->aloc = NULL;

      insist(pam_overlay_map_insert(f->overlays, loc, overlay));
    }
}

/* Add a linearity overlay to a file */
void pam_add_overlay_lin(location loc, store store, aloc al)
{
  pam_file f;
  pam_overlay overlay;

  if (!pam_file_map_lookup(pam_files, loc->filename, &f))
    {
      pam_add_file(loc->filename);
      insist(pam_file_map_lookup(pam_files, loc->filename, &f));
    }

  /*  printf("Adding overlay %s to %s\n", idname, files->filename);*/
  if (pam_overlay_map_lookup(f->overlays, loc, &overlay))
    {
      overlay->kind |= overlay_lin;
      if (!overlay->store)
	overlay->store = store;
      else
	assert(eq_store(overlay->store, store));
      assert(!overlay->aloc);
      overlay->aloc = al;
    }
  else
    {
      overlay = ralloc(pam_region, struct Overlay);
      overlay->kind = overlay_lin;
      overlay->name = "change_type";
      overlay->qtype = NULL;
      overlay->fs_qtype = NULL;
      overlay->store = store;
      overlay->aloc = al;

      insist(pam_overlay_map_insert(f->overlays, loc, overlay));
    }
}

/* Tell PAM that qt is the flow-sensitive qtype at location loc, and
   the store at that location is s. */
void pam_add_overlay_flow_sensitive(location loc, qtype qt, store s)
{
  pam_file f;
  pam_overlay overlay;

  insist(pam_file_map_lookup(pam_files, loc->filename, &f));
  insist(pam_overlay_map_lookup(f->overlays, loc, &overlay));
  assert(overlay->kind & overlay_qtype);
  assert(!(overlay->kind & overlay_fs_qtype));
  overlay->kind |= overlay_fs_qtype;
  assert(!overlay->fs_qtype);
  overlay->fs_qtype = qt;
  assert(!overlay->store || eq_store(overlay->store, s));
  overlay->store = s;
}

/**************************************************************************
 *                                                                        *
 * Initial (Summary) Window                                               *
 *                                                                        *
 **************************************************************************/

/* Print out a list of files.  Assumes we're already in a blob. */
void pam_print_files(void)
{
  pam_file_map_scanner_sorted htss;
  const char *name;
  pam_file f;

  if (pam_file_map_size(pam_files) == 0)
    return;

  pam_add_text("Analyzed files:\n\n");

  pam_file_map_scan_sorted(pam_files, strcmp, &htss);
  while (pam_file_map_next_sorted(&htss, &name, &f))
    {
      pam_add_text("        ");
      pam_add_link(f->link, pam_color_2, name);
      pam_add_text("\n");
    }
}

/* Print out a list of errors.  Assumes we're already in a blob. */
void pam_print_errors(void)
{
  region scratch_region;
  dd_list_pos p;
  char *s;

  if (dd_is_empty(pam_errors))
    return;

  scratch_region = newregion();
  pam_add_text("The following errors were detected:\n\n");
  dd_scan(p, pam_errors)
    {
      pam_error e;
      char *color;

      e = DD_GET(pam_error, p);
      switch (e->sev)
	{
	case sev_err:
	  color = pam_color_error;
	  break;
	case sev_warn:
	  color = pam_color_warning;
	  break;
	case sev_info:
	  color = pam_color_info;
	  break;
	default:
	  fail("Unexpected severity %d\n", e->sev);
	}

      if (e->loc)
	{
	  /* If there's a location for this error, add link */
	  s = rsprintf(scratch_region, "%s:%ld", e->loc->filename,
		       e->loc->lineno);
	  pam_add_link(e->link, color, s);
	  pam_add_text("\n");
	}
      pam_add_text("%s", e->message);
      if (e->detail)
	pam_add_text("\n%s", e->detail);
      pam_add_text("\n\n");
    }
  deleteregion(scratch_region);
}

void pam_create_initial_window(void)
{
  pam_clear_buffer(pam_results_buffer);
  pam_blobs_begin();
  pam_markup_buffer(pam_results_buffer);
  /* pam_add_text("\nQualifier inference\n\n"); */


  pam_add_link(pam_about_click, pam_color_6, "About");
  pam_add_text("                                 ");
  if (exist_undefineds())
    pam_add_link(pam_undefineds_click, pam_color_6, "Undefined Globals");
  else
    pam_add_text("Undefined Globals (none)");
  pam_add_text("\n");

  if (exist_hotspots())
    {
      pam_add_link(pam_hotspots_click, pam_color_6,
		   "Hotspots");
      pam_add_text("                              ");
    }
  else
    pam_add_text("Hotspots (none)                       ");
  if (exist_varargs())
    pam_add_link(pam_varargs_click, pam_color_6,
		 "Non-Polymorphic Varargs Functions");
  else
    pam_add_text("Non-Polymorphic Varargs Functions (none)");

  pam_add_text("\n");
  if (qerrors)
    pam_add_text("\nQualifier inference failed.\n\n");
  else
    pam_add_text("\nQualifier inference succesful.\n\n");
  pam_print_files();
  pam_add_text("\n");
  pam_print_errors();
  pam_blobs_end();
}

/**************************************************************************
 *                                                                        *
 * Top-level PAM loop                                                     *
 *                                                                        *
 * Enter the PAM browsing mode, handling clicks, looping until PAM exits  *
 *                                                                        *
 **************************************************************************/

/* Bring up the first page again */
void pam_home_click_func(const char *click, const char *file,
			 const char *name, void *arg)
{
  pam_close_buffer(pam_results_buffer);
  pam_set_upper_buffer(pam_results_buffer);
  pam_change_buffer(pam_results_buffer);
}

/* Add customized functions for clicks */
void pam_install_generic_buffer_clicks(void)
{
  pam_add_buffer_click(pam_results_buffer, pam_about_click,
		       pam_about_click_func, NULL);
  pam_add_buffer_click(pam_results_buffer, pam_undefineds_click,
		       pam_undefineds_click_func, NULL);
  pam_add_buffer_click(pam_results_buffer, pam_varargs_click,
		       pam_varargs_click_func, NULL);
  pam_add_buffer_click(pam_results_buffer, pam_hotspots_click,
		       pam_hotspots_click_func, NULL);
  pam_add_buffer_click(pam_about_buffer, pam_home_click,
		       pam_home_click_func, NULL);
  pam_add_buffer_click(pam_type_buffer, pam_any_click, pam_qual_click_func,
		       NULL);
}

/* Handle ``click'' in PAM window ``file,'' on type ``type,'' on
   hyperlink ``name'' */
void pam_click_dispatch(char *click_name, char *name, char *file, char *type)
{
  if (!strcmp(type, pam_window_buffer_msg))
    {
      /* It's a dispatch in a custom window.  Search through our table. */
      pam_click c;

      c = pam_get_buffer_click(file, name);
      if (!c)
	pam_message("Don't know what to do for buffer click on ``%s'' in "
		    "``%s''",
		    name, file);
      else
	c->func(click_name, file, name, c->arg);
    }
  else if (!strcmp(type, pam_window_file_msg))
    {
      pam_message("Unexpected PAM window type ``%s''", type);
    }
  else
      pam_qtype_click_func(click_name, file, name, NULL);
}

/* Reads at most size-1 characters from the standard input, storing
   the result in buf.  Stops reading characters after detecting an \n
   or an EOF (not included in buf), or when the buffer is full.
   Always null-terminates buf.  Returns the number of characters
   stored in the buffer, including \0. */
int get_line(char *buf, int size)
{
  int n = 0;
  /*  static FILE *foo = 0;*/

  assert(size >= 1);   /* Must be space for \0 */
  while (n < size - 1) /* Leave space for terminating \0 */
    {
      char c = pam_getc();
      if (c == EOF || c == '\n')
	break;
      buf[n++] = c;
    }
  buf[n++] = '\0';
  /* if (!foo)
       foo = fopen("clicks", "w");
     fprintf(foo, "%s\n", buf);
     fflush(foo);*/
  return n;
}

/* Enter PAM mode, and loop on PAM events */
void enter_pam_mode(void)
{
  /* Disable buffering on stdout to make cygwin happy */
#ifdef SETVBUF_REVERSED
  setvbuf(stdout, _IONBF, NULL, 0);
#else
  setvbuf(stdout, NULL, _IONBF, 0);
#endif
  pam_install_generic_buffer_clicks();

  /* Set up the results buffer */
  pam_printf("\n\n///\n");  /* Mysterious message to indicate analysis is
			       over */
  pam_close_buffer(pam_results_buffer);
  pam_change_buffer(pam_results_buffer);
  pam_create_initial_window();
  if (qerrors)
    pam_message("Qualifier inference failed.");
  else
    pam_message("Qualifier inference successful.");

  /* Go into a loop, accepting commands from PAM */
  while (1)
    {
      static char buf[BUF_MAX];
      int bufSize;
      char *cmd;

      pam_waiting(); /* Tell PAM we're waiting for input */
      bufSize = 0; /* Number of characters actually in buffer */
      cmd = buf + bufSize;
      if ((bufSize += get_line(cmd, BUF_MAX-bufSize)) == BUF_MAX)
	fail("PAM sent a command that was too big\n");
      if (!strcmp(buf, pam_click_msg) || !strcmp(buf, pam_shift_click_msg))
	{
	  char *name, *file, *type;

	  /* Read name */
	  name = buf + bufSize;;
	  if ((bufSize += get_line(name, BUF_MAX-bufSize)) == BUF_MAX)
	    fail("PAM sent a command that was too big\n");

	  /* Read file */
	  file = buf + bufSize;
	  if ((bufSize += get_line(file, BUF_MAX-bufSize)) == BUF_MAX)
	    fail("PAM sent a command that was too big\n");

	  /* Read type */
	  type = buf + bufSize;
	  if ((bufSize += get_line(type, BUF_MAX-bufSize)) == BUF_MAX)
	    fail("PAM sent a command that was too big\n");

	  /* pam_message("Click! name=%s, file=%s, type=%s", name, file,
	     type); */
	  pam_click_dispatch(cmd, name, file, type);
	}
      else if (!strcmp(buf, "quit"))
	{
	  pam_message("Analysis program exiting.");
	  break;
	}
      else if (!strcmp(buf, "file"))
	fail("Unimplemented: PAM file\n");
      else
	pam_message("Unrecognized command ``%s'' returned from PAM", buf);
    }
}

/*
 * Extra stuff
 *
 * Print summary of analysis results
 */

void print_results(void)
{
  pam_file_map_scanner_sorted htss;
  const char *name;
  pam_file f;

  if (hotspots)
    {
      int i;

      printf("Potential hotspots:\n\n");
      for (i = 0; i < num_hotspots && hotspots[i]; i++)
	printf("%s (%d)\n", name_qual(hotspots[i]),
	       error_count_qual(hotspots[i]));
      printf("\n");
    }

  pam_file_map_scan_sorted(pam_files, strcmp, &htss);
  while (pam_file_map_next_sorted(&htss, &name, &f))
    {
      pam_overlay_map_scanner_sorted omss;
      pam_overlay overlay;
      location loc;

      printf("File %s:\n", name);
      pam_overlay_map_scan_sorted(f->overlays, location_cmp, &omss);
      while (pam_overlay_map_next_sorted(&omss, &loc, &overlay))
	{
	  if (overlay->kind & overlay_qtype)
	    {
	      printf("%s: ", overlay->name);
	      print_qtype_qf(printf, print_qual, overlay->qtype, NULL,
			     FALSE);
	    }
	  if (overlay->kind & overlay_fs_qtype)
	    {
	      printf("  (line %ld) ", loc->lineno);
	      print_qtype_qf(printf, print_qual, overlay->fs_qtype,
			     overlay->store, FALSE);
	    }
	  if (overlay->kind & (overlay_qtype | overlay_fs_qtype))
	    printf("\n");
	}
      printf("\n");
    }
}
