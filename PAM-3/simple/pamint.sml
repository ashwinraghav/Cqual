(* Copyright (c) 1999-2001 The Regents of the University of California.
 *
 * PAM is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * PAM is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with PAM; see the file COPYING.  If not, write to
 * the Free Software Foundation, 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 *)

(* 

 * DON'T WORRY ABOUT THE EXACT ANALYSIS!
 * For this small example, you shouldn't think about the
 * `analysis' that is provided in text.sml, but rather 
 * focus on the PAM API.
 *)

structure PamInterface =
struct

    (* create an instance of the PAM-3 structure *)
    structure PAM3 = PAM_3(
	struct
	    type return_type = string
	    type source_type = string
	    type region_type = int * int
	    val get_filename = fn f => f
	    val get_filename_all = fn f => fn r => f
	    val get_region_range = fn (p1,p2) => (p1,p2)
	end)

    val clickColor = PAM3.color_1
    val listColor = PAM3.color_2
    val headlineColor = PAM3.highlight

    val main_window = PAM3.VirtualWindow "*Table of contents*"


    fun add_overlay window ((pos,string),env) = 
	let val hyper = PAM3.Hyper
	                   {region = (pos,pos+String.size string),
			    attrib = SOME listColor,
			    func = fn env => SOME env}
	in
	    PAM3.send window hyper env
	end

    fun printSpace ws = PAM3.Text{text=ws, attrib=NONE, region=NONE}
    fun printWord word = PAM3.Text{text=word, attrib=SOME listColor, region=NONE}

    fun analyze_file dict file env =
	let val (stream,_) = PAM3.streams_of env
	    val _ = PAM3.send_status (stream, "analyzing " ^ file)
	    val wordList = Text.analyze(dict, file)
	    val env = List.foldl (add_overlay (PAM3.RealWindow file)) env wordList
	    val _ = PAM3.send_status (stream, "done analyzing")
	in
	    env
	end

    fun handle_file_click dict file env =
	let val env = analyze_file dict file env
	    val env = PAM3.set_window_up (PAM3.RealWindow file) env
	    val env = PAM3.set_window_down main_window env
	    val env = PAM3.change_window (PAM3.RealWindow file) env
	in
	    SOME env
	end

    fun show_results dict file env =
	let 
	    (* 1. Make sure the main window is displayed *)
	    val env' = PAM3.change_window main_window env

	    (* 2. Get some information back from the analysis 
             * - in this case it is just a list of words each `printed'
             * using the printWord and printSpace functions above. *)
	    val words = Text.printDictionary printWord printSpace dict

	    (* 3. Present the result in PAM format *)
	    val text = [PAM3.Text {text = "\n", attrib=NONE, region=NONE},
			PAM3.Text {text = "Simple PAM", attrib=SOME headlineColor, region=NONE},
			PAM3.Text {text = "\n\n", attrib=NONE, region=NONE},
			PAM3.Text {text="Looking for:\n", attrib=NONE, region=NONE}] @
		       words @
		       [PAM3.Text {text="\nin ",attrib=NONE,region=NONE},
			PAM3.HyperText {text=file,
					attrib=SOME clickColor,
					func=handle_file_click dict file,
					region=NONE},
			PAM3.Text {text="\n", attrib=NONE,region=NONE}]

	    (* 4. Send the result to PAM. *)
	    val env'' = PAM3.send_list main_window text env'
	in
	    env''
	end


    fun pam_loop dict file =
	let val env = PAM3.start_pam
		        TextIO.stdOut TextIO.stdIn (* communication streams *)
	                ()                         (* type of information stored *)
			(fn file => fn env => env) (* per-file analysis function *)
                        (fn env => env)            (* global analysis function *)

	in
	    PAM3.loop_until_done (show_results dict file env)
	end

end (* structure PamInterface *)
