exception EmptyInputFile;
exception LineError of string;
fun cD(infilename: string, delim1: char,outfilename: string,delim2: char) =
    let
        val instream = TextIO.openIn infilename
        val outstream = TextIO.openOut outfilename
        fun converter(a: char option,i: int,count: int,num_col: int,quote_count: int,field: string,num: int) =
            case a of
                NONE => 
                    if(i=0) then (TextIO.closeIn instream; TextIO.closeOut outstream; raise EmptyInputFile)
                    else (TextIO.closeIn instream; TextIO.closeOut outstream)
                | SOME(c) => 
                    if(c = delim1) then 
                    	if (quote_count mod 2=1) then 
                    		if(num = 0) then converter(TextIO.input1 instream, i,count,num_col,quote_count,field^str(c),1)
                    		else if(num = 2) then converter(TextIO.input1 instream, i,count,num_col,quote_count,field^str(c),3)
                    		else converter(TextIO.input1 instream, i,count,num_col,quote_count,field^str(c),num)
                    	else 
                    		if (num = 1) then (TextIO.output(outstream,substring(field,1,size(field)-2)); TextIO.output1(outstream,delim2);converter(TextIO.input1 instream, i,count+1,num_col+1,quote_count,"",0))
                    		else if(num = 2) then (TextIO.output1(outstream,#"\"");TextIO.output(outstream,field);TextIO.output1(outstream,#"\"");TextIO.output1(outstream,delim2); converter(TextIO.input1 instream, i,count+1,num_col+1,quote_count,"",0))
                    		else (TextIO.output(outstream,field);TextIO.output1(outstream,delim2); converter(TextIO.input1 instream, i,count+1,num_col+1,quote_count,"",0))
                    else if(c = delim2) then 
                    	if(num = 0) then converter(TextIO.input1 instream, i,count,num_col,quote_count,field^str(c),2)
                    	else if(num = 1) then converter(TextIO.input1 instream, i,count,num_col,quote_count,field^str(c),3)
                    	else converter(TextIO.input1 instream, i,count,num_col,quote_count,field^str(c),3)
                    else if(c = #"\"") then converter(TextIO.input1 instream, i,count,num_col,quote_count+1,field^str(c),num)
                    else if (c = #"\n") then
                    	if (quote_count mod 2=1) then converter(TextIO.input1 instream, i,count,num_col,quote_count,field^str(c),3)
                    	else
						    if(i = 0) then 
						    	if (num = 1) then (TextIO.output(outstream,substring(field,1,size(field)-2)); TextIO.output1(outstream,c);converter(TextIO.input1 instream, i+1,count+1,1,quote_count,"",0))
                    			else if(num = 2) then (TextIO.output1(outstream,#"\"");TextIO.output(outstream,field);TextIO.output1(outstream,#"\"");TextIO.output1(outstream,c); converter(TextIO.input1 instream, i+1,count+1,1,quote_count,"",0))
                    			else (TextIO.output(outstream,field);TextIO.output1(outstream,c); converter(TextIO.input1 instream, i+1,count+1,1,quote_count,"",0))

						    else if ((count-num_col) = num_col) then 
						    	if (num = 1) then (TextIO.output(outstream,substring(field,1,size(field)-2)); TextIO.output1(outstream,c);converter(TextIO.input1 instream, i+1,num_col+1,1,quote_count,"",0))
                    			else if(num = 2) then (TextIO.output1(outstream,#"\"");TextIO.output(outstream,field);TextIO.output1(outstream,#"\"");TextIO.output1(outstream,c); converter(TextIO.input1 instream, i+1,num_col+1,1,quote_count,"",0))
                    			else (TextIO.output(outstream,field);TextIO.output1(outstream,c); converter(TextIO.input1 instream, i+1,num_col+1,1,quote_count,"",0))
						    else raise LineError("Expected: "^Int.toString(count-num_col)^" fields, Present: "^Int.toString(num_col)^" fields on Line "^Int.toString(i+1)^"\n")
				   	else converter(TextIO.input1 instream, i,count,num_col,quote_count,field^str(c),num)
    in
        converter(TextIO.input1 instream,0,1,1,0,"",0)
    end
    handle LineError error_message => print error_message
    
fun csv2tsv(infilename: string, outfilename: string) =
	let
  		val delim1 = #","
  		val delim2 = #"\t"
  	in
  		cD(infilename,delim1,outfilename,delim2)
  	end
fun tsv2csv(infilename: string, outfilename: string) =
  	let
  		val delim1 = #"\t"
  		val delim2 = #","
  	in
  		cD(infilename,delim1,outfilename,delim2)
  	end

