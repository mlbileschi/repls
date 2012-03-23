import java.io._
import java.lang.Integer
import java.lang.System
import scala.collection.mutable.HashMap


object repl
{
	var localVars = new HashMap[String, Expression];
	var predefined = "([\\Q+-*/\\E(define)(quit)])".r //check this //@todo load problems
	val whiteSpaceRegex = "\\s+".r
	def main(args: Array[String])
	{
		while(true)
		{
			System.out.print("repl> ");
			var wasRead = reed(readLine(""))		
			var evaluated = wasRead.eval()
			System.out.println(evaluated)
		}
	}


	def loadFile(fileName:String):Bool =
	{

		//try to parse as int
		try
		{
			val inFile = scala.io.Source.fromFile(fileName)
			var expStr = bufferExpressionFromFile(inFile)
			while(expStr!=null)
			{
				reed(expStr).eval()
				expStr = bufferExpressionFromFile(inFile)
			}
			return new Bool(true)
		}
		catch
		{
			case fnfe: FileNotFoundException => return new Bool(false)
								//return new Message("file " + fileName + " not found!")
			case e: Exception => return new Bool(false)
					//return new Message("unable to open file" + e.printStackTrace() + " " + e.getMessage())
		}


	}


	
	//returns null on eof
	def bufferExpressionFromFile( inFile:scala.io.BufferedSource ):String =
	{
	
		var leftParens = 0
		var rightParens = 0
		var bufferUntilWhiteSpace = 0
		var curStr = ""

		//buffer until whitespace or until leftparens = right parens.
		while(inFile.hasNext)
		{
			var c = inFile.next()
			curStr += c

			if (c == '(')
			{
				leftParens+=1
			}
			else if(c == ')')
			{
				rightParens+=1
			}	

			//if leftparens=0, then want to buffer until whitespace then return
			//never need to set back to 0, because we want it to return that token
			//once bufferUntilWhitespace is on.
			if(leftParens == 0 && curStr.replaceAll("\\s+","")!="")
			{
				bufferUntilWhiteSpace = 1
			}

			if(bufferUntilWhiteSpace==1 && curStr.replaceAll("\\s+","")!="")
			{
				c+"" match
				{
					case whiteSpaceRegex(w) => //is whitespace
						if(!inFile.hasNext)
							return curStr;
					case _ => ;//continue
				}
			}

			if(leftParens==rightParens)
			{
				return (curStr);
			}
		}
		return null;
	}



	//determines type and returns an object of that type
	def reed(in:String):Expression = 
	{


		//preprocessing
		var str = in.replaceAll("\\s+", " ") //change all whitespace to " "
		str = str.replaceAll("^\\s+", "") //kill beginning whitespace
		str = str.replaceAll("\\s+$", "") //kill trailing whitespace

		//if it's only whitespace, print nothing
		if(str.replaceAll("\\s+","")=="")
		{
			return new Message("")
		}

		//if # left braces == left braces and first and last chars are ( and ) , 
		//then we've got an application or a define/load form (quit was first in the method)
		if (str.length>0 && 
			str(0)=='(' && 
			str(str.length-1) == ')' &&
			str.count( _ == '(' ) == str.count( _ == ')' ) )
		{
			//preprocessing
			str = str.substring(1, str.length-1) //kill outer parens
			str = str.replaceAll("^\\s+", "") //kill beginning whitespace
			str = str.replaceAll("\\s+$", "") //kill trailing whitespace

			var numArgs = 0
			var leftMarker = 0
			var leftParens = 0
			var rightParens = 0
			var toret = new Array[String](3)
			var i = 0

			for(rightMarker<-0 to str.length-1)
			{
				if (str(rightMarker) == '(')
				{
					leftParens+=1
				}
				else if(str(rightMarker) == ')')
				{
					rightParens+=1
				}

				//we have another argument
				if(leftParens == rightParens && str(rightMarker)==' ')
				{
					numArgs+=1
					if(numArgs>3)
					{
						return new Message("wasn\'t able to match a type: wrong number of arguments")
					}

					toret(numArgs-1) = str.substring(leftMarker, rightMarker)
					leftMarker = rightMarker+1
				}
				//end of string
				if(leftParens == rightParens && rightMarker==str.length-1)
				{
					numArgs+=1
					if(numArgs>3)
					{
						return new Message("wasn\'t able to match a type: wrong number of arguments")
					}
					toret(numArgs-1) = str.substring(leftMarker)
				}
			}
			if (toret(0) == "quit")
			{
				if(numArgs ==1)
				{
					System.exit(0)
				}
				else
				{
					return new Message("malformed quit expression, quit is reserved")
				}
			}
			else if (toret(0)=="load" && toret(1)(0)=='\"' && toret(1)(toret(1).length-1)=='\"')
			{
				if(numArgs==2)
				{
					return loadFile(toret(1).substring(1,toret(1).length-1))//kill off outer parens
				}
				else
				{
					return new Message("malformed load expression, load is reserved")
				}
			}
			else if(numArgs!=3)
			{
				return new Message("wasn\'t able to match a type: wrong number of arguments ")
			}
			else if (toret(0)=="define")
			{
				return new Define(toret(1), reed(toret(2)));
			}
			else
			{
				return new Application(reed(toret(0)), reed(toret(1)), reed(toret(2)))
			}
		}
		// ----- end of process application ----


		//try to parse as int
		try
		{
			val i = Integer.parseInt(str)
			return new Number(i)
		}
		catch
		{
			case nfe: NumberFormatException => ;//Do nothing!
		}

		//ry to parse as name
		if(str=="+")
			return new Name("+")
		else if(str=="-")
			return new Name("-")
		else if(str=="*")
			return new Name("*")
		else if(str=="/")
			return new Name("/")

		//try to parse as boolean
		else if(str=="#true")
			return new Bool(true)
		else if(str=="#false")
			return new Bool(false)

		//look up in define
		localVars.get(str) match
		{
			case Some(exp) => return exp;
			case None => ;//Nothin!
		}
		

		return new Message("?error: unbound name")
	
	}




	trait Expression
	{
		def toString():String
		def eval():Expression
	}

	class Define(val key:String, val value:Expression) extends Expression
	{
		override def toString():String = 
		{
			return "key: " + key + " value: " + value.toString();
		}

		def eval():Expression = 
		{
			localVars.get(key) match
			{
				case Some(name) => return new Message("?error: bound name: "+ key + " with value " + name)
				case None => 
					key match
					{
						case predefined(name) => return new Message("?error: bound name: "+ key + " with value " + name)
						case _ => 	localVars.put( key, value); //no need to eval toret(1)
										return new Message("");//print nothing on eval of define
					}
					
			}
		}
	}

	class Bool(val value:Boolean) extends Expression
	{
		override def toString():String = 
		{
			if(value==true)
				return "#true";
			return "#false";
		}
		def eval() = this
	}

	class Number(val value:Int) extends Expression
	{
		override def toString():String = 
		{
			return ""+value
		}
		
		def eval() = this
	}

	class Name(val str:String) extends Expression
	{
		override def toString() = str

		def eval() = this
	}

	class Message(val str:String) extends Expression
	{
		override def toString() = str
		def eval() = this
	}

	class Application(exp1:Expression, exp2:Expression, exp3:Expression) extends Expression
	{
		override def toString():String =
		{
			"(" + exp1 + " " + exp2 + " " + exp3 + ")"
		}

		def eval():Expression = 
		{
			var toret = new Array[Expression](3)
			exp1 match 
			{
				case x:Application => toret(0) = exp1.eval()
				case _ => toret(0) = exp1 //other cases do not need to be evaluated
			}
			exp2 match 
			{
				case x:Application => toret(1) = exp2.eval()
				case _ => toret(1) = exp2
			}
			exp3 match
			{
				case x:Application => toret(2) = exp3.eval()
				case _ => toret(2) = exp3
			}
			
			if (toret(0).toString()=="+")
			{
				List(toret(1), toret(2)) match
				{
					case List(e1:Number, e2:Number) => return new Number(toret(1).asInstanceOf[Number].value + toret(2).asInstanceOf[Number].value)
					case _ => return new Message("can\'t add non-numerics")
				}
			}
			else if (toret(0).toString()=="*")
			{
				List(toret(1), toret(2)) match
				{
					case List(e1:Number, e2:Number) => return new Number(toret(1).asInstanceOf[Number].value * toret(2).asInstanceOf[Number].value)
					case _ => return new Message("can\'t multiply non-numerics")
				}
			}
			else if (toret(0).toString()=="-")
			{
				List(toret(1), toret(2)) match
				{
					case List(e1:Number, e2:Number) => return new Number(toret(1).asInstanceOf[Number].value - toret(2).asInstanceOf[Number].value)
					case _ => return new Message("can\'t subtract non-numerics")
				}
			}
			else if (toret(0).toString()=="/")
			{
				List(toret(1), toret(2)) match
				{
					case List(e1:Number, e2:Number) => 
						if(toret(2).asInstanceOf[Number].value == 0)
						{
							return new Message("divide by zero error")
						}
						else
							return new Number(toret(1).asInstanceOf[Number].value / toret(2).asInstanceOf[Number].value)
					case _ => new Message("can\'t add non-numerics")
				}
			}
			else
			{
				return new Message("evaluation rules are not defined for the given operation")
			}

		}
		
	}
}
