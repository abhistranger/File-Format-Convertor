exception emptyInputFile;         (*defining exception for empty file*)
exception UnevenFields of string;       (*defining exception for Uneven number of field*)
fun convertDelimiters(infilename: string, delim1: char,outfilename: string,delim2: char) =       (*function of converting delimiters in file*)
    let
        val instream = TextIO.openIn infilename         (* defining instream*)
        val outstream = TextIO.openOut outfilename       (* defining outstream*)
        fun converter(a: char option,i: int,count: int,num_col: int,quote_count: int,field: string,num: int) =  (*function for delimiter conversion process, a is char option, i is for counting the no of lines, count is for counting no of fields in the previous line+ in current line, num_col is for counting no of fiels in current line, quote_count is to count the no of double quote, field is for storing a fiel, num is for deciding how to output the string n==1 means remove double quote from field, if n==2 means add double quote to in the field and n==3 means do nothing with the field*)
            case a of        (* a is the char read by the input1 in instream*)
                NONE => 	(*if a is none that is there is no char*)
                    if(i=0) then (TextIO.closeIn instream; TextIO.closeOut outstream; raise emptyInputFile) (*if it is at the starting of file then it means the file is empty then raise emptyInputfile*)
                    else (TextIO.closeIn instream; TextIO.closeOut outstream)	(*whenn it is at the end of the file then close both the files*)
                | SOME(c) => 		(*if a is some char*)
                    if(c = delim1) then  (*if the char is delimiter then do*)
                    	if (quote_count mod 2=1) then  (*if no of double quote at this point is odd then this delim1 is a part of field so add this to the field string as usual and modify num according to current num value*)
                    		if(num = 0) then converter(TextIO.input1 instream, i,count,num_col,quote_count,field^str(c),1)
                    		else if(num = 2) then converter(TextIO.input1 instream, i,count,num_col,quote_count,field^str(c),3)
                    		else converter(TextIO.input1 instream, i,count,num_col,quote_count,field^str(c),num)
                    	else 	(*else if no of quote till now is even then this is the real delim1 so first we add the string field to output according to num which tell wheather to keep double quote or add it or remove it from the string filed and then delim2 in place of delim1. Make the string field empty and num=0 when calling converter again *)
                    		if (num = 1) then (TextIO.output(outstream,substring(field,1,size(field)-2)); TextIO.output1(outstream,delim2);converter(TextIO.input1 instream, i,count+1,num_col+1,quote_count,"",0))
                    		else if(num = 2) then (TextIO.output1(outstream,#"\"");TextIO.output(outstream,field);TextIO.output1(outstream,#"\"");TextIO.output1(outstream,delim2); converter(TextIO.input1 instream, i,count+1,num_col+1,quote_count,"",0))
                    		else (TextIO.output(outstream,field);TextIO.output1(outstream,delim2); converter(TextIO.input1 instream, i,count+1,num_col+1,quote_count,"",0))
                    else if(c = delim2) then    (*if the char is delim2 then update num value according to current num value and add this to string field as usual *)
                    	if(num = 0) then converter(TextIO.input1 instream, i,count,num_col,quote_count,field^str(c),2)
                    	else if(num = 1) then converter(TextIO.input1 instream, i,count,num_col,quote_count,field^str(c),3)
                    	else converter(TextIO.input1 instream, i,count,num_col,quote_count,field^str(c),3)
                    else if(c = #"\"") then converter(TextIO.input1 instream, i,count,num_col,quote_count+1,field^str(c),num) (*if char is double quote then increment no of double quote by 1*)
                    else if (c = #"\n") then 	(*if char is newline character*)
                    	if (quote_count mod 2=1) then converter(TextIO.input1 instream, i,count,num_col,quote_count,field^str(c),3) (*if no of double quote at this point is odd then this newline char is part of the field and so just update num=3*)
                    	else (*else if no of quote till now is even then check if it it first line or not, if not then check for uneven no of field, if it don't have uneven no of field in this line add the string to the output according to the num value and upadate parameters *)
							if(i = 0) then 
						    	if (num = 1) then (TextIO.output(outstream,substring(field,1,size(field)-2)); TextIO.output1(outstream,c);converter(TextIO.input1 instream, i+1,count+1,1,quote_count,"",0))
                    			else if(num = 2) then (TextIO.output1(outstream,#"\"");TextIO.output(outstream,field);TextIO.output1(outstream,#"\"");TextIO.output1(outstream,c); converter(TextIO.input1 instream, i+1,count+1,1,quote_count,"",0))
                    			else (TextIO.output(outstream,field);TextIO.output1(outstream,c); converter(TextIO.input1 instream, i+1,count+1,1,quote_count,"",0))

						    else if ((count-num_col) = num_col) then 
						    	if (num = 1) then (TextIO.output(outstream,substring(field,1,size(field)-2)); TextIO.output1(outstream,c);converter(TextIO.input1 instream, i+1,num_col+1,1,quote_count,"",0))
                    			else if(num = 2) then (TextIO.output1(outstream,#"\"");TextIO.output(outstream,field);TextIO.output1(outstream,#"\"");TextIO.output1(outstream,c); converter(TextIO.input1 instream, i+1,num_col+1,1,quote_count,"",0))
                    			else (TextIO.output(outstream,field);TextIO.output1(outstream,c); converter(TextIO.input1 instream, i+1,num_col+1,1,quote_count,"",0))
						    else raise UnevenFields("Expected: "^Int.toString(count-num_col)^" fields, Present: "^Int.toString(num_col)^" fields on Line "^Int.toString(i+1)^"\n")
				   	else converter(TextIO.input1 instream, i,count,num_col,quote_count,field^str(c),num)  (*else add the char to string field*)
    in
        converter(TextIO.input1 instream,0,1,1,0,"",0)   (*calling the converter func in in *)
    end
    handle UnevenFields error_message => print error_message  (*handling the unevenfieldexception by printing the message*)
    
fun csv2tsv(infilename: string, outfilename: string) =  (*function to convert csv to tsv*)
	let
  		val delim1 = #","
  		val delim2 = #"\t"
  	in
  		convertDelimiters(infilename,delim1,outfilename,delim2)
  	end
fun tsv2csv(infilename: string, outfilename: string) = (*function to convert tsv to csv*)
  	let
  		val delim1 = #"\t"
  		val delim2 = #","
  	in
  		convertDelimiters(infilename,delim1,outfilename,delim2)
  	end

