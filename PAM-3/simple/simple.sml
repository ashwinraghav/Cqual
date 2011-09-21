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

structure Simple =
struct

    fun pam_main (arg0, [wordlist,file]) =
	  (let val dict = Text.createDictionary wordlist
	       val _ = PamInterface.pam_loop dict file
	   in 
	       OS.Process.success
	   end
	   handle _ => OS.Process.failure
          )
      | pam_main (arg0, _) =
	(print ("Arguments are: <dictionary-file> <file-to-analyze>");
	 OS.Process.failure)

    fun export () =
	SMLofNJ.exportFn("simple", pam_main)

end (* structure Example *)
