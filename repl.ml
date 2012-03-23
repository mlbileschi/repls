(*	@TODO try the commmand use "hw2.ml" in repl()... empty error?*)
datatype Expression = 
	Message of string
	| Number of int
	| Name of string
	| Bool of bool
	| Load of string
	| Application of Expression * Expression * Expression
	| Define of string * Expression;


(*********************** helper methods *************************)

(*Remove newline characters from a string *)
fun trimNewLines(s) = 
	let
		(*Remove all newlines from a char list.
			is a helper for trimString*)
		fun removeNewLine([]) = [] |
	   	removeNewLine(#"\n"::_) = [] |
	   	removeNewLine(h::t) = h::removeNewLine(t);
	in
		implode(removeNewLine(explode(s)))
	end;


fun lastIsQuote(str) = (substring(str, size(str)-1, 1)="\"") 
fun firstIsQuote(str) = (substring(str, 0, 1)="\"")

fun isLParen("(") = 1
	| isLParen(_) = 0;

fun isRParen(")") = 1
	| isRParen(_) = 0;

fun isSpace(" ") = 1
	| isSpace(_) = 0;

fun isAllSpace("") = true
 | isAllSpace(str) = isSpace(substring(str,0,1))=1 andalso isAllSpace(substring(str,1,size(str)-1));


(*worry about tabs?*)
fun trimEndSpace("") = ""
 | trimEndSpace(str) =
(
	case Char.fromString(String.extract(str,size(str)-1, NONE)) of
		SOME(c) =>
			if (Char.isSpace(c)) then
				trimNewLines(trimEndSpace(substring(str,0,size(str)-1)))
			else 
				trimNewLines(str)
		| NONE => trimNewLines(str)
);

fun trimLeadingSpace("") = ""
 | trimLeadingSpace(str) =
(
	case Char.fromString(String.substring(str, 0, 1)) of
		SOME(c) =>
			if (Char.isSpace(c)) then
				trimNewLines(trimLeadingSpace(String.extract(str,1,NONE)))
			else 
				trimNewLines(str)
		| NONE => trimNewLines(str)
);


fun readLine():string =
(
	case (TextIO.inputLine(TextIO.stdIn)) of
		NONE => "no input"
		| SOME(str) =>	str

);

fun printStrList([]) = print(")")
	| printStrList(strL) = 
	(
		print(hd(strL)^"| ");
		printStrList(tl(strL))
	);

fun listToString([]) = ""
	| listToString(strL) = hd(strL)^", "^listToString(tl(strL))

fun trimLeadingParens(str) =
	if(substring(str, 0, 1)="(" ) then
		substring(str,1,size(str)-1)
	else
		str;

fun trimEndingParens(str) = 
	if(substring(str, size(str)-1, 1)=")") then
		substring(str, 0, size(str)-1)
	else
		str;


fun lookup(key,[]) = NONE
	| lookup(key, table) = 
	(
		let
			val (k, v) = hd(table);
		in
			if(k = key) then
				SOME (v)
			else
				lookup(key,tl(table))	
		end
		
	);



(**************** end of helper methods ***************************)




fun expToStr(Message(str)) = str
	| expToStr(Number(n)) = Int.toString(n)
	| expToStr(Name(str)) = str
	| expToStr(Bool(bool)) = "#"^Bool.toString(bool)
	| expToStr(Define(str, exp)) = str^"==>"^expToStr(exp)
	| expToStr(Load(str)) = "load "^ str
	| expToStr(Application(e1, e2, e3)) = "( "^expToStr(e1)^" "^expToStr(e2)^" "^expToStr(e3)^" )";


fun reed(strA,table) =
( 
	if(isAllSpace(trimEndSpace(strA))) then
		Message("")
	else 
		let
			val str = (trimLeadingSpace(trimEndSpace(
							trimLeadingParens(trimEndingParens(
							trimLeadingSpace(trimEndSpace(strA)))))));
			fun buffer(inStr, numArgs, leftMarker, 
					rightMarker, leftParens, rightParens, wasPrevSpace, toret):string list =
			(
				(*print("instr = |"^inStr^"| "^Int.toString(numArgs)^" "^Int.toString(leftMarker)^" "^
						Int.toString(rightMarker)^" "^Int.toString(leftParens)^" "^
						Int.toString(rightParens)^"\n");
				print(listToString(toret));
				print "\n\n";*)
				if (rightMarker=size(inStr)-1) then
					toret@[String.extract(inStr, leftMarker, NONE)]
				else
					let
						val c = String.extract(inStr, rightMarker, SOME(1));
					in
	
	(*
				print ("c= " ^c^"   and lparen = "^Int.toString(isLParen(c))^"   and rparen = "^Int.toString(isRParen(c))	^"\n");
				print ("but leftParens = "^Int.toString(leftParens)^"  and rightParens = "^Int.toString(rightParens)	^"\n");
	*)
						if (isSpace(c)=1 andalso wasPrevSpace) then 
							buffer(inStr, numArgs, leftMarker+1, rightMarker+1, leftParens+isLParen(c), rightParens+isRParen	(c), true, toret)
						else if(leftParens+isLParen(c)=rightParens+isRParen(c) andalso isSpace(c)=1 ) then
							if(numArgs>2) then
								["was not able to match a type, wrong number of args"]
							else
								buffer(inStr, numArgs+1, rightMarker+1, rightMarker+1, 
									leftParens+isLParen(c), rightParens+isRParen(c), true,
									toret@[String.extract(inStr, leftMarker, SOME(rightMarker-leftMarker))])
						else if (leftParens+isLParen(c)=rightParens+isRParen(c) andalso rightMarker=size(inStr)-1) then
						(
							if(numArgs>2) then
								["was not able to match a type, wrong number of args"]
							else
								buffer(inStr, numArgs+1, rightMarker+1, rightMarker+1, 
									leftParens+isLParen(c), rightParens+isRParen(c), false,
									toret@[String.extract(inStr, leftMarker, NONE)])
						)
						else
							buffer(inStr, numArgs, leftMarker, rightMarker+1, leftParens+isLParen(c), rightParens+isRParen	(c), false, toret)
					end
			)
			in
			(
				(*watch for subscript oob (just a \n), empty... *)
				(*print(listToString(buffer(str,0,0,0,0,0,false,[])));*)
				(* @TODO switch from method to actually parse. check # of args... *)
				let
					val strArr = buffer(str,0,0,0,0,0,false,[])
				in	
					if(length(strArr)=1) then
						case lookup(hd(strArr), table) of 
							SOME(v) => v
							| NONE =>
								(*	( print("|"^hd(strArr)^"|\n"); *)
								case hd(strArr) of
									"+" => Name("+")
									| "-" => Name("-")
									| "*" => Name("*")
									| "/" => Name("/")
									| "#true" => Bool(true)
									| "#false" => Bool(false)
									| _ =>
										case Int.fromString(str) of
											SOME(n) => Number(n)
											| NONE => Message("?error: unbound name")

					else if (length(strArr)=2) then
						if (hd(strArr)="load" andalso firstIsQuote(hd(tl(strArr))) andalso lastIsQuote(hd(tl(strArr)))) then
							Load( substring(hd(tl(strArr)), 1, size(hd(tl(strArr)))-2) )
						else
							Message("wrong number of arguments/incorrect load call")
					else if (length(strArr)=3) then
						if (hd(strArr)="define") then
							Define( hd(tl(strArr)), reed( hd(tl(tl(strArr))), table ) )
						else
						(
							Application( 	reed(hd(strArr), table), 
												reed(hd(tl(strArr)), table), 
												reed(hd(tl(tl(strArr))), table))
						)
					else
						Message("not the correct number of arguments")
				end
			)
			end
);

(*needs to return table *)
fun load(fileName, table) =
(
	let
		val inHandle = TextIO.openIn(fileName);
		fun bufferExpString(readHandle, lParens, rParens, curStr) =
		(
			case TextIO.input1(readHandle) of
				SOME(c) =>
				( 
					if (lParens+isLParen(Char.toString(c))=rParens+isRParen(Char.toString(c))
								andalso not(isAllSpace(curStr)) 
								andalso isSpace(Char.toString(c))=1) then
						SOME(curStr)
					else if (lParens +isLParen(Char.toString(c)) = rParens+isRParen(Char.toString(c))) then
						SOME(curStr)
					else
						bufferExpString(readHandle, lParens+isLParen(Char.toString(c)), 
									rParens+isRParen(Char.toString(c)), curStr^Char.toString(c))
				)
				| NONE => 
				(
					if(curStr="") then
						NONE
					else
						SOME(curStr)
				)
		)
	in
		let 		
			fun loadString(inFileHandle, table) =
			(
				case bufferExpString(inFileHandle, 0, 0, "") of
					SOME(str) => 
					(
						let
							val (exp, table2) = eval(reed(str, table), table);
						in
						(
							loadString(inFileHandle,table2)
						)
						end
					)
					| NONE => (true, table)
			)
		in
			loadString(inHandle, table)
		end
	end
)

(*********************************************************)
and eval(Message(str), table) = (Message(str), table)
	| eval(Number(n), table) = (Number(n), table)
	| eval(Name(str), table) = (Name(str), table)
	| eval(Bool(bool), table) = (Bool(bool), table)
	| eval(Load(str), table) = 
	(
		let
			val (b, table2) = load(str, table)
		in
			(Bool(b), table2)
		end
	)

	| eval(Define(str, exp), table) = 
	(
		case lookup(str, table) of
			SOME(v) => (Message("?error: bound name: "^ str ^ " with value "^ expToStr(v)), table)
			| NONE => (Message(""), table@[(str, exp)] )
	)

	| eval(Application(e1, e2, e3), table) =
	(
		let
			val (evaled1,table1) = eval(e1, table);
			val (evaled2,table2) = eval(e2, table1);
			val (evaled3,table3) = eval(e3, table2)
		in
			case evaled1 of
				Name(str) =>
				(
					case str of
						"+" =>
						(
							case [evaled2,evaled3] of
								[Number(n1),Number(n2)] => (Number(n1+n2), table3)
								| _ => (Message("can't perform + on nonnumerics"), table3)
						)
						| "-" =>
						(
							case [evaled2,evaled3] of
								[Number(n1),Number(n2)] => (Number(n1-n2), table3)
								| _ => (Message("can't perform - on nonnumerics"), table3)
						)
						| "*" =>
						(
							case [evaled2,evaled3] of
								[Number(n1),Number(n2)] => (Number(n1*n2), table3)
								| _ => (Message("can't perform * on nonnumerics"), table3)
						)
						| "/" =>
						(
							if(evaled3=Number(0)) then
								(Message("can't divide by 0"), table3)
							else
								case [evaled2,evaled3] of
									[Number(n1),Number(n2)] => (Number(n1 div n2), table3)
									| _ => (Message("can't perform / on nonnumerics"), table3)
						)
						| _ => (Message("no evaluation rules for "^str), table3)
				)
				| _ => (Message("no evaluation rules for non name first arguments"), table3)
		end
	)	;





fun repl() =
(
	let
		fun replH(table) =
		(
			print "repl> ";
			let		
				val x = readLine();
			in
				if(x = "(quit)\n") then
					()
				else
				(
					let
						val (toPrint, table2) = eval(reed(x,table),table) 	(*ok to use same table
																							because haven't eval'ed yet*)
					in
						(*print(expToStr(reed(x)));*)
						print( ""^expToStr(toPrint)^"\n" );
						replH(table2)
					end
				)
			end	
		)
	in
		replH([])
	end
);
