(*
 Copyright (c) 1999-2001 The Regents of the University of California.

 PAM is free software; you can redistribute it and/or modify
 it under the terms of the GNU General Public License as published by
 the Free Software Foundation; either version 2, or (at your option)
 any later version.

 PAM is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 GNU General Public License for more details.

 You should have received a copy of the GNU General Public License
 along with PAM; see the file COPYING.  If not, write to
 the Free Software Foundation, 59 Temple Place - Suite 330,
 Boston, MA 02111-1307, USA.
*)

(*
 * The new PAM interface!
 *)

signature PAM_STRUCT =
   sig
       (* Regions:
	* ------------------------------------------------------------
	* The type of a region (usually, just a starting and ending 
        * offset ('int * int'), but it can have more information if
	* you want). 
	*)
       type region_type


      (* get_region_range (region)
       * ------------------------------------------------------------
       * User supplied function.
       *
       * It should return the starting and ending offset of a node;
       * for instance, if the node is a variable which begins 50
       * characters from the start of the source file and the variable
       * is 5 characters long, you should return (50, 54).
       *)
      val get_region_range : region_type -> int * int
   end


(* PAM-3 Interface description *)
(* ============================================================ *)
signature PAM_3 =
   sig

      structure PamStruct: PAM_STRUCT
      type region_type = PamStruct.region_type


      (* Environments:
       * ------------------------------------------------------------

       * The environment; contains all information PAM needs to
       * maintain things for you.  Note that it has no 'ref' variables
       * in it, so you can (have to) use it safely and functionally.
       *
       * An environment has an associated `user information' field of
       * type 'a; you can get and set that field using get_info and
       * set_info below.
       *)
      type 'a environment

      val get_info: 'a environment -> 'a
      val set_info: 'a environment -> 'a -> 'a environment

      type comm_out
      type comm_in
      val streams_of: 'a environment -> comm_out * comm_in

      val set_default_path: string  -> 'a environment -> 'a environment


      (* Markup:
       * ------------------------------------------------------------
       * Each markup corresponds to an externally-defined style.
       * PAM also provides a set of default colors for hyperlinks,
       * which are guaranteed to work.
       *)
      type markup

      val face_to_markup: string -> markup
      val color_1: markup
      val color_2: markup
      val color_3: markup
      val color_4: markup
      val color_5: markup
      val color_6: markup
      val color_7: markup
      val color_8:  markup
      val highlight: markup

      (* Windows:
       * ------------------------------------------------------------
       * How to identify windows in the visualization system:
       *
       * RealWindow(file): A window with an associated file
       *               named `file'.
       * VirtualWindow(name): A window with no associated file;
       *               all data to be displayed has to be
       *               inserted using `Text' or `HyperText'
       *               messages (see below). `name' is the
       *               name used to refer to the window in
       *               all subsequent calls.
       * LastWindowClicked: Identifies the last window for which a
       *               a click was recieved in Emacs.
       *)
      datatype window = 
	  VirtualWindow of string
        | RealWindow of string
	| LastWindowClicked

      datatype access = READ_ONLY | READ_WRITE
      val set_access: window -> 'a environment -> access -> 'a environment

      val clear_window : window -> 'a environment -> 'a environment
      val change_window : window -> 'a environment -> 'a environment

      val set_window_up : window -> 'a environment -> 'a environment
      val set_window_down : window -> 'a environment -> 'a environment
      val split_window : window -> 'a environment -> 'a environment

      val goto_char : window -> int -> 'a environment -> 'a environment

      val close_window: window -> 'a environment -> 'a environment


      (* Messages:
       * -----------------------------------------------------------------
       * A message is anything that the ML process can send to the
       * visualizer. Currently four messages are implemented:
       *
       * - Text {attribOpt, text, regionOpt}
       * - Markup {attrib, region}
       * - Hyper {attribOpt, region, func}
       * - HyperText {attribOpt, text, regionOpt, func}
       *
       * with the following meaning: 
       *
       * For all messages, `attribOpt' specifies the appearance of the
       * specified text or region.  If it is NONE, no highlighting is done,
       * so that the hyperlink can only be followed by selecting the region
       * of the link.
       *
       * A `Text' message inserts text in the buffer at the specified region
       * (if non-NONE) *overwriting* any text already there. If the string
       * `text' is longer than the region, subsequent buffer-text is just
       * overwritten (i.e. the region can be interpreted as a starting
       * position only). If the region is NONE, the text is simply appended
       * to the end of the buffer.  (Note: the overwriting functionality may
       * not work correctly in the current version)
       *
       * A `Hyper' message marks a region as a hyper link. The
       * function `func' is called whenever the hyper link is
       * clicked. This function can return NONE to signal the end of
       * the analysis loop, or SOME env with a possibly updated
       * 'a environment.
       *
       * A 'Markup' message marks a region for highlighting purposes, but does
       * not associate a hyper link with it.
       *
       * A `HyperText' combines the behavior of the two above messages:
       * It inserts text into the buffer, and makes the inserted text a
       * hyper link.
       *
       * IMPLEMENTATION NOTE: Currently, `Text' and `HyperText' only works
       * for NONE regions (the region argument is discarded).
       *)
      datatype 'a message =
	  Text of 	  {attrib: markup option, 
			   text: string, 
			   region: region_type option}
        | Markup of       {attrib: markup,
			   region: region_type}
	| Hyper of        {attrib: markup option, 
			   region: region_type, 
			   func: 'a environment -> 'a environment option}
	| HyperText of    {attrib: markup option, 
			   text: string, 
			   region: region_type option, 
			   func: 'a environment -> 'a environment option}


      (* Send functions 
       * ------------------------------------------------------------
       * Functions for sending different kinds of messages to the 
       * visualizer.
       *)

      (* send window message env:
         Send the message to the visualization process. *)
      val send: window -> 'a message -> 'a environment -> 'a environment


      (* send_list buffer messages env: 
         Send the list of messages to the visualization process. *) 
      val send_list: window -> 'a message list -> 'a environment -> 'a environment


      (* send_debug(communicationOut, string)
         Send the string as a `debug message' (sent to a separate PAM
         PAM `window') to the visualizer. *)
      val send_debug: comm_out * string -> unit


      (* send_status(communicationOut, string)
         Send the string as a `status message' (e.g., sent to the minibuffer
         when in Emacs mode) to the visualizer. *)
      val send_status: comm_out * string -> unit


      (* start_pam outStream inStream info fileAnalysis analysis:
       * ------------------------------------------------------------
       * start_pam initializes internal PAM data structures.
       *
       * start_pam also sends a token to the visualizer to signify
       * that actual data transmissions are beginning.  Everything
       * earlier than the token is ignored by the visualizer.  This
       * means that you shouldn't send *any* of your own data along
       * the outstream or instream associated with PAM after calling
       * this function.  WARNING: the token is currently '///'
       * (without the single quotes).  Send this token yourself at
       * your own risk.
       *
       * (outStream, inStream): streams for communication between
       *                        the PAM process and the SML process.
       * info:                  initial user information.
       * fileAnalysis:          A function, which when called, should
       *                        put markup into the file name specified.
       * analysis:              A function, which when called, should
       *                        put markup into all relevant files.
       *)
      val start_pam: 
	  TextIO.outstream -> TextIO.instream -> 
	  'a ->
	  (string -> 'a environment -> 'a environment) ->
	  ('a environment -> 'a environment) 
	  -> 'a environment

      (* loop_until_done env:
       * ------------------------------------------------------------
       * Starts the interactive PAM loop. The loop continues until
       * some callback functions returns NONE.
       *)
      val loop_until_done: 'a environment -> 'a environment

   end
