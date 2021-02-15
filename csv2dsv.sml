fun convertDelimiters(infilename: string, delim1: char,outfilename: string,delim2: char) =
	let 
		val instream = TextIO.openIn infilename
    	val outstream = TextIO.openOUT outfilename
    	fun convert (s:string) =
      		let
      		in
      		end
    	fun loop (accum: string list) =
      		case (TextIO.inputLine instream) of 
				"" => raise EmptyInputFile
          		| line => loop (convert(line)::accum)
          		(*(TextIO.output (outstream , line);TextIO.closeOut outstream)*)
          		(* esac *)
    	val lines = rev(loop [])
  	in 
	  	TextIO.closeIn instream
  	end

fun csv2tsv(infilename: string, outfilename: string) =
  	let
  		val delim1 = ","
  		val delim2 = "	"
  	in
  		convertDelimiters(infilename,delim1,outfilename,delim2)
  	end
fun tsv2csv(infilename: string, outfilename: string) =
  	let
  		val delim1 = "	"
  		val delim2 = ","
  	in
  		convertDelimiters(infilename,delim1,outfilename,delim2)
  	end

(*
fun convertNewlines(infilename, newline1, outfilename, newline2)

fun unix2dos(infilename, outfilename)

fun dos2unix(incilename, outfilename)
*)

