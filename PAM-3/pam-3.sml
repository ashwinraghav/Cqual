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
 * the implementation of the new PAM interface!
 *)

functor PAM_3(PamStruct: PAM_STRUCT) : PAM_3 =
   struct
       
       structure PamStruct = PamStruct
       type region_type = PamStruct.region_type
       val get_region_range = PamStruct.get_region_range

       fun notImplemented msg = raise Fail("Not Implemented: " ^ msg)


(*
      fun safe_text_convert str =
	 let
	    fun safe_helper [] = []
	      | safe_helper (#"\n"::rest) = #"\\" :: #"n" :: safe_helper rest
	      | safe_helper (#"\""::rest) = #"\\" :: #"\"" :: safe_helper rest
	      | safe_helper (char::rest) = char :: safe_helper rest
	 in
	      implode (safe_helper (explode str))
	 end
*)

      (* safe_text_convert(string): convert string to a safe string
       * that Emacs can actually handle. *)
      fun safe_text_convert string =
	  String.translate (fn #"\n" => "\\n"
                             | #"\"" => "\\\""
			     | ch => Char.toString ch
			   )
                           string

      fun toString int = if int < 0 then "-" ^ Int.toString(Int.abs int)
			 else Int.toString int

      type markup = string

      datatype window = 
	  RealWindow of string
        | VirtualWindow of string
	| LastWindowClicked

      structure ReverseMap = BinaryMapFn(struct type ord_key = string; val compare = String.compare end)
      datatype 'a environment = 
	  Env of {last_filename : window option,
		  user_send_analysis_by_filename : string -> 'a environment -> 'a environment,
		  user_send_entire_analysis : 'a environment -> 'a environment,
		  streamOut : TextIO.outstream,
		  streamIn : TextIO.instream,
		  reverse_map : 'a reverse_map,
		  info: 'a}
      withtype 'a handle_func = 'a environment -> 'a environment option
      and 'a reverse_map = ('a environment -> 'a environment option) ReverseMap.map

      datatype 'a message =
	  Text of 	  {attrib: markup option, 
			   text: string, 
			   region: region_type option}
        | Markup of       {attrib: markup,
			   region: region_type}
	| Hyper of        {attrib: markup option, 
			   region: region_type, 
			   func: 'a handle_func}
	| HyperText of    {attrib: markup option, 
			   text: string, 
			   region: region_type option, 
			   func: 'a handle_func}



      (* ENVIRONMENTS:
       * ------------------------------------------------------------
       *)
      fun get_info (Env{info,...}) = info
      fun set_info (Env{last_filename,user_send_analysis_by_filename, user_send_entire_analysis,
			streamOut, streamIn, reverse_map, info}) new_info =
	  Env{last_filename=last_filename,
	      user_send_analysis_by_filename=user_send_analysis_by_filename,
	      user_send_entire_analysis=user_send_entire_analysis,
	      streamOut=streamOut, streamIn=streamIn,
	      reverse_map=reverse_map,
	      info=new_info}

      type comm_out = TextIO.outstream
      type comm_in = TextIO.instream
      fun streams_of (Env{streamOut,streamIn,...}) = (streamOut, streamIn)

      fun stream_of (Env{streamOut,...}) = streamOut
      fun last_file_of (Env{last_filename,...}) = last_filename

      fun addToMap (Env{last_filename, user_send_analysis_by_filename,
			user_send_entire_analysis,streamOut, streamIn, reverse_map,info})
	           (name,func) =
	  Env{last_filename=last_filename,
	      user_send_analysis_by_filename=user_send_analysis_by_filename,
	      user_send_entire_analysis=user_send_entire_analysis,
	      streamOut=streamOut, streamIn=streamIn,info=info,
	      reverse_map=ReverseMap.insert(reverse_map,name,func)}
      fun lookup (Env{reverse_map,...}) name =
	  ReverseMap.find (reverse_map, name)

      (* MARKUP:
       * ------------------------------------------------------------
       *)
      fun face_to_markup face = face
      val color_1 = "pam-color-1"
      val color_2 = "pam-color-2"
      val color_3 = "pam-color-3"
      val color_4 = "pam-color-4"
      val color_5 = "pam-color-5"
      val color_6 = "pam-color-6"
      val color_7 = "pam-color-7"
      val color_8 = "pam-color-8"
      val highlight = "pam-highlight-face"


      (* MESSAGES:
       * ------------------------------------------------------------
       *)
      (* HELPER PRINT FUNCTIONS:
       * ------------------------------------------------------------
       * These functions print Emacs commands to the specified stream.
       *
       * The available Emacs commands are documented in the file
       * emacs-functions.
       *)

      fun print_stream stream str = (TextIO.output (stream, str); TextIO.flushOut stream)
      fun send_cmd (strm, cmd) = print_stream strm (concat cmd)
      fun send_env_cmd (env, cmd) = (send_cmd (stream_of env, cmd); env)

      fun showOpt NONE = ""
        | showOpt (SOME text) = safe_text_convert(text)

      fun showQuoteOpt NONE = "\"\""
        | showQuoteOpt (SOME text) = "\"" ^ safe_text_convert(text) ^ "\""

			   
      (* WINDOWS:
       * ------------------------------------------------------------
       *)
      fun buffer_of env (RealWindow buffer) = buffer
        | buffer_of env (VirtualWindow buffer) = buffer
        | buffer_of env (LastWindowClicked) = 
	    case last_file_of env of
		NONE => raise Fail("LastWindowClicked wasn't set")
	    |   SOME(LastWindowClicked) => raise Fail("LastWindowClicked was ``LastWindowClicked''")
	    |   SOME(RealWindow buffer) => buffer
	    |   SOME(VirtualWindow buffer) => buffer

      fun type_of env (RealWindow buffer) = "file"
        | type_of env (VirtualWindow buffer) = "buffer"
        | type_of env (LastWindowClicked) = 
	    case last_file_of env of
		NONE => raise Fail("LastWindowClicked wasn't set")
	    |   SOME(LastWindowClicked) => raise Fail("LastWindowClicked was ``LastWindowClicked''")
	    |   SOME(RealWindow buffer) => "file"
	    |   SOME(VirtualWindow buffer) => "buffer"

      fun showWindowOpt env NONE = "()"
	| showWindowOpt env (SOME window) =
	  let val (buffer,typ) = (buffer_of env window, type_of env window)
	  in  "(\"" ^ buffer ^ "\" \"" ^ typ ^ "\")" end	  

      fun win_cmd (win, env, cmd) = 
	  send_env_cmd (env, ["(", cmd, " ", showWindowOpt env (SOME win), ")\n"])

      datatype access = READ_ONLY | READ_WRITE
      fun accessToString READ_ONLY = "read-only"
	| accessToString READ_WRITE = "read-write"
      fun set_access window env access = 
	  send_env_cmd (env, ["(pam-set-access ", showWindowOpt env (SOME window), " \"", accessToString(access), "\")\n"])

      fun clear_window win env = win_cmd (win, env, "pam-clear-window")
      fun change_window win env = win_cmd (win, env, "pam-change-window")
      fun set_window_up win env = win_cmd (win, env, "pam-set-window-up")
      fun set_window_down win env = win_cmd (win, env, "pam-set-window-down")
      fun split_window win env = win_cmd (win, env, "pam-split-window")

      fun close_window win env = win_cmd (win, env, "pam-close-window")

      fun goto_char win pos env = 
	  send_env_cmd (env, ["(pam-goto-char ", showWindowOpt env (SOME win), " ", toString(pos), ")\n"])


      (* SEND FUNCTIONS:
       * ------------------------------------------------------------
       *)
      fun show_region (a, b) = "(" ^ toString a ^ " " ^ toString b ^ ")"

      fun show_text env {text} = [" (text \"", safe_text_convert text, "\")"]

      fun show_hyper env {window,region=(a,b),attrib,text,name} =
		  [" (hyper ", 
		        (* " ", showWindowOpt env window, "", *)
		        " ", "\"", safe_text_convert name, "\"", 
			" ", show_region (a,b),
			" (", showOpt attrib, ")",
			" ", showQuoteOpt text,
                    ")"
		  ]

      fun show_markup env {window,region=(a,b), attrib,text} =
		  [" (markup",
		        (*" ", showWindowOpt env window, " ", *)
			" ", show_region (a,b),
			" (", showOpt attrib, ")",
			" ", showQuoteOpt text,
                     ")"
		  ]

      local val counter = ref 0
      in
	  (*fun buffer_name buffer = (counter := !counter + 1;buffer ^ "@<nowhere>#" ^ Int.toString (!counter))*)
	  fun buffer_name buffer = (counter := !counter + 1; "n" ^ Int.toString (!counter))
      end (* local *)

      fun send_msg window env msg =
	  let val stream = stream_of env
	      val buffer = buffer_of env window
	      fun send (Text{text,attrib=NONE,region=NONE}) = 
	              (send_cmd(stream, show_text env {text=text});
		       env)
		| send (Text{text,attrib=attrib as SOME _, region=NONE}) = 
		      (send_cmd(stream, show_markup env {window=NONE,region=(~1,~1),attrib=attrib,text=SOME text});
		       env)
		| send (Markup{attrib,region}) =
		      (send_cmd(stream, show_markup env {window=SOME window,attrib=SOME attrib,
							 region=get_region_range region,text=NONE});
		       env)
		| send (Hyper{attrib,region,func}) =
		      let val name = buffer_name buffer
		      in (send_cmd(stream, show_hyper env {window=SOME window,name=name,attrib=attrib,
							   region=get_region_range region,text=NONE});
			  addToMap env (name, func))
		      end
		| send (HyperText{text,attrib,region=NONE,func}) =
		      let val name = buffer_name buffer
		      in (send_cmd(stream, show_hyper env {window=SOME window,name=name,attrib=attrib,
							   text=SOME text,region=(~1,~1)});
			  addToMap env (name, func))
		      end
		| send (Text{region=SOME _,...}) = notImplemented("send(Text with region)")
		| send (HyperText{region=SOME _,...}) = notImplemented("send(HyperText with region)")
	  in
	      send msg
	  end

      val max_pr_line = 100
      fun send_list window [] env = env
        | send_list window (messages as msg::_) env =
	    (case msg of
		 Text _ => send_blobs window env messages
             |   HyperText _ => send_blobs window env messages
             |   Hyper _ => send_overlays window env messages
             |   Markup _ => send_overlays window env messages
            )
      and send_blobs window env messages =
	    let val stream = stream_of env (* streams are invariant in env's *)
		fun loop env (_, []) = send_env_cmd(env, [")\n"])
		  | loop env (0, messages) = (send_cmd(stream, [")\n"]); send_list window messages env)
                  | loop env (no, (msg as Text _)::messages) = loop (send_msg window env msg) (no-1,messages)
                  | loop env (no, (msg as HyperText _)::messages) = loop (send_msg window env msg) (no-1,messages)
                  | loop env (_, messages) = (send_cmd(stream, [")\n"]); send_list window messages env)
	    in
		send_cmd(stream, ["(pam-blobs (file ", showWindowOpt env (SOME window), ")"]);
		loop env (max_pr_line, messages)
	    end
      and send_overlays window env messages =
	    let val stream = stream_of env (* streams are invariant in env's *)
		fun loop env (_, []) = send_env_cmd(env, [")\n"])
		  | loop env (0, messages) = (send_cmd(stream, [")\n"]); send_list window messages env)
                  | loop env (no,(msg as Markup _)::messages) = loop (send_msg window env msg) (no-1,messages)
                  | loop env (no,(msg as Hyper _)::messages) = loop (send_msg window env msg) (no-1,messages)
                  | loop env (_, messages) = (send_cmd(stream, [")\n"]); send_list window messages env)
	    in
		send_cmd(stream, ["(pam-overlays (file ", showWindowOpt env (SOME window), ")"]);
		loop env (max_pr_line, messages)
	    end

      and send window message env = send_list window [message] env

      fun send_debug (stream, string) = send_cmd (stream, ["(pam-debug \"", safe_text_convert(string), "\")\n"])
      fun send_status (stream, string) = send_cmd (stream, ["(pam-message \"", safe_text_convert(string), "\")\n"])


      (* START PAM:
       * ------------------------------------------------------------
       *)
      fun start_pam out_stream in_stream info filename entire =
	 let val new_env = Env {last_filename = NONE,
				user_send_analysis_by_filename = filename,
				user_send_entire_analysis = entire,
				streamOut = out_stream,
				streamIn = in_stream,
				reverse_map = ReverseMap.empty, info=info}
	 in
	    send_env_cmd (new_env, ["///\n"])
	 end


      fun set_default_path the_path env = send_env_cmd (env, ["(pam-default-path \"", the_path, "\")\n"])


      (* loop_until_done env
       * ------------------------------------------------------------
       *)
      fun read_click (env as Env {last_filename, reverse_map, streamIn, streamOut,
				  user_send_analysis_by_filename, user_send_entire_analysis,info}) =

	 let
	    val name = TextIO.inputLine streamIn
	    val file = TextIO.inputLine streamIn
	    val file' = String.substring(file, 0, String.size(file)-1)
	    val typ = TextIO.inputLine streamIn
	    val window = if typ = "buffer" then VirtualWindow file' else RealWindow file'

	    (*val _ = send_debug (streamOut, file' ^ " with type " ^ typ)*)
	    val real_name = String.substring(name, 0, String.size(name)-1)
	    val env' = Env {last_filename=SOME window,
			       reverse_map=reverse_map, streamIn=streamIn, streamOut=streamOut,
			       user_send_analysis_by_filename=user_send_analysis_by_filename,
			       user_send_entire_analysis=user_send_entire_analysis,
			       info=info}
	 in
	     case lookup env' real_name of
		NONE => (send_status(streamOut, "Error: clicked object (" ^ real_name ^ ") not found."); loop_until_done env')
	      | SOME (handle_func) =>
		    (case handle_func env' of
			 SOME env'' => loop_until_done env''
                     |   NONE => (send_status(streamOut, "Analysis program exiting."); env')
                    )
	 end
      and read_file (env as Env {user_send_analysis_by_filename, user_send_entire_analysis,
				    streamIn, ...}) =
	 let
	    val filename = TextIO.inputLine streamIn
	    val real_name = String.substring(filename, 0, String.size(filename)-1)
	 in
	    (loop_until_done o (user_send_analysis_by_filename real_name)) env
	 end
      and loop_until_done (env as (Env {streamOut,streamIn, ...})) =
	 let
	    val next_value = TextIO.inputLine streamIn
	 in
	    case next_value of
	       "click\n" => read_click env
	     | "quit\n" => (send_status(streamOut, "Analysis program exiting."); env)
	     | "file\n" => read_file env
	     | _ => loop_until_done (send_status(streamOut, "Unrecognized command (" ^ next_value ^ ") returned from emacs."); env)
	 end

   end   
