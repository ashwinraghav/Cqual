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

structure Text:
    sig
	type dict
	val createDictionary: string -> dict
	val printDictionary: (string -> 'a) -> (string -> 'a) -> dict -> 'a list
	val analyze: dict * string -> (int*string) list
	val readWordList : string -> string list
    end =

struct

    exception Fail

    (* 
     * OPERATIONS ON DICTIONARIES 
     *)
    structure BinarySet = 
	BinarySetFn(struct
			type ord_key = string
			val compare = String.compare
		    end)
    type dict = BinarySet.set
    fun mkDict list =
	BinarySet.addList (BinarySet.empty, list)
    fun isMember dict word =
	BinarySet.member (dict, word)
    fun printDictionary wordFunc wsFunc dict =
	let val per_line = 5
	    val column_size = 15
	    val indent = 3
	    fun spaces n = String.implode (List.tabulate (n, fn _ => #" "))
	    fun fillColumn word = wsFunc (spaces (column_size - String.size word))
	    fun mkSpace n = wsFunc (spaces n)
	    fun loop 0 [] acc = acc
              | loop 0 dict acc = loop per_line dict (mkSpace indent :: wsFunc "\n" :: acc)
              | loop n [] acc = acc
              | loop n (word::dict) acc = loop (n-1) dict (fillColumn word :: wordFunc word :: acc)
	    val dict_list = BinarySet.listItems dict
	in
	    rev (wsFunc "\n" :: loop per_line dict_list [mkSpace indent])
	end

    (* 
     * OPERATIONS ON WORDS 
     *)
    fun getWords string =
	String.tokens (fn #" " => true
                        | #"\n" => true
                        | _ => false)
	              string
    val getFirstWord = hd o getWords
    fun getWordsAndPos startPos string =
	let fun skipBlanks p [] = ([], p)
              | skipBlanks p (#" "::rest) = skipBlanks (p+1) rest
	      | skipBlanks p (#"\t"::rest) = skipBlanks (p+1) rest
              | skipBlanks p str = (str, p)
	    fun scan ([], p, acc) = (p, acc)
	      | scan (#" " :: rest, p, acc) = 
	          let val (rest', p') = skipBlanks (p+1) rest
		  in 
		      scan (rest', p', (p',[]) :: acc)
		  end
	      | scan (#"\t" :: rest, p, acc) =
		let val (rest', p') = skipBlanks (p+1) rest
		in
		    scan (rest', p', (p',[]) :: acc)
		end
	      | scan (#"\n" :: _, p, acc) = (p+1, acc)
	      | scan (ch :: rest, p, (pos,word) :: acc) = 
		  scan (rest, p+1, (pos,ch :: word) :: acc)
	      | scan (ch :: rest, p, []) = 
		  scan (rest, p+1, [(p,[ch])])

	    val (string, p) = skipBlanks startPos (String.explode string)
	    val (endPos, words) = scan (string, p, [(p,[])])
	in
	    (endPos,
	     map (fn (pos, rev_word) =>
		    (pos, String.implode (rev rev_word))
		 )
	         words)
	end

    (*
     * CREATE A DICTIONARY
     *)
    fun readWordList file =
	let val is = TextIO.openIn file
	    fun readLines acc = 
		if TextIO.endOfStream is then (TextIO.closeIn is; acc)
		else let val line = TextIO.inputLine is
		     in
			 readLines (getFirstWord(line) :: acc)
			 handle List.Empty => readLines acc
		     end
	    val list = readLines []
	in
	    list 
	end
        handle IO.Io _ => (print ("Couldn't read file '" ^ file ^ "'\n");
			   raise Fail)
	    
    fun createDictionary file =
	let val words = readWordList file
	    val dict  = mkDict words
	in
	    dict
	end

    (*
     * ANALYZE
     *)
    fun analyze (dict, file) =
	let val is = TextIO.openIn file
	    fun readWords pos acc = 
		if TextIO.endOfStream is then (TextIO.closeIn is; acc)
		else let val line = TextIO.inputLine is
		         val (endPos,words) = getWordsAndPos pos line
			 fun add acc = 
			     List.foldl (fn ((p,w),acc) => 
					    if isMember dict w
					    then (p,w) :: acc
					    else acc)
			                acc
			                words
		     in
			 readWords (endPos) (add acc)
		     end
	in
	    readWords 1 []
	end
        handle IO.Io _ => (print ("Couldn't read file '" ^ file ^ "'\n");
			   raise Fail)
	    
end (* structure Text *)

