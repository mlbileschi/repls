
$localVars = Hash.new()


def repl()
	print "repl> "
	wasRead = reed(gets())
	print(wasRead.eval().to_s + "\n")
	repl()
end	

def reed(str)
	#kill beginning and trailing whitespace
	str.gsub!(/^\s+/, "") 
	str.gsub!(/\s+$/, "") 
	#replace all whitespace with single spaces
	str.gsub!(/\s+/, " ") 
	
	#if it's only whitespace, print nothing
	if(str =~ /^\s*$/)
		return Message.new("")
	end

	#try to parse as an application
	#if # left braces == left braces and first and last chars are ( and ) , 
	#then we've got an application or a define/load form (quit was first in the method)
	if (str =~/^\(/ && 
		str =~ /\)$/ &&
		str.scan(/\(/).size == str.scan(/\)/).size )

		#preprocessing
		str = str[1..-2] #kill outer parens

		#kill beginning and trailing whitespace
		str.gsub!(/^\s+/, "") 
		str.gsub!(/\s+$/, "")
 
		numArgs = 0
		leftMarker = 0
		leftParens = 0
		rightParens = 0
		toret = Array.new(3) # will contain 3 args of each expression within the application
		i = 0

		for rightMarker in 0..str.length-1

			if str[rightMarker] == '('
				leftParens+=1
			elsif str[rightMarker] == ')' 
				rightParens+=1
			end

			#we have another argument
			if(leftParens == rightParens && str[rightMarker]==' ')
				numArgs+=1
				if(numArgs>3)
					return new Message("wasn\'t able to match a type: wrong number of arguments")
				end

				toret[numArgs-1] = str[leftMarker..rightMarker-1] #check this @TODO
				leftMarker = rightMarker+1
			end

			#end of string
			if(leftParens == rightParens && rightMarker==str.length-1)
				numArgs+=1
				if(numArgs>3)
					return new Message("wasn\'t able to match a type: wrong number of arguments")
				end

				toret[numArgs-1] = str[leftMarker..-1] #check this
			end
		end

		if (toret[0] == "quit")
			if(numArgs ==1)
				abort
			else
				return Message.new("malformed quit expression, quit is reserved")
			end

		elsif (toret[0].eql?("load") && toret[1]=~/\"$/ && toret[1]=~/^\"/)
			if(numArgs==2)
				return loadFile(toret[1][1..-2])#kill off outer quotes @check
			else
				return Message.new("malformed load expression, load is reserved")
			end

		elsif(numArgs!=3)
			return Message.new("wasn\'t able to match a type: wrong number of arguments ")

		elsif (toret[0]=="define")
			
			return Define.new(toret[1], reed(toret[2]));

		else
			return Application.new(reed(toret[0]), reed(toret[1]), reed(toret[2]))
		end
	end
	# ----- end of process application ----

	#try to parse as an int
	if(str =~ /^\d+$/)
		return Number.new(str.to_i)
	end

	#try to parse as name
	if(str =~ /^[\+\*\/\-]$/ )
		return Name.new(str)
	end

	#try to parse as Bool
	if(str =~ /^(\#true)$/)
		return Bool.new(true)
	elsif (str =~ /^(\#false)$/)
		return Bool.new(false)
	end

	#look up in define
	if ($localVars.has_key?(str)) 
		return $localVars[str];
	end

	return Message.new("?error: unbound name\n")
end

def loadFile(fileName)
	if File::exists?( fileName )
		inFile = File.open(fileName) 
	else
		print "couldn't open file"
		return Bool.new(false)
	end

	expStr = bufferExpressionFromFile(inFile)

	while(expStr!=nil)
		reed(expStr).eval()
		expStr = bufferExpressionFromFile(inFile)
	end

	return Bool.new(true)
end

#returns nil on eof
def bufferExpressionFromFile( inFile ) 
	leftParens = 0
	rightParens = 0
	bufferUntilWhiteSpace = 0
	curStr = ""
	#buffer until whitespace or until leftparens = right parens.
	while(c = inFile.getc())
		
		curStr += c
		if (c == '(')
			leftParens+=1
		elsif(c == ')')
			rightParens+=1
		end

		#if leftparens=0, then want to buffer until whitespace then return
		#never need to set back to 0, because we want it to return that token
		#once bufferUntilWhitespace is on.
		if(leftParens == 0 && curStr.gsub(/\s+/, "")!="")
			bufferUntilWhiteSpace = 1
		end

		if(bufferUntilWhiteSpace==1 && curStr.gsub(/\s+/,"")!="")
			if(c=~/\s+/)
				if(!inFile.eof?)
					return curStr;
				end
			end
		end

		if(leftParens==rightParens)
			return (curStr);
		end

	end #while

	return nil;
end


#for all intents an abstract class/interface
class Expression
	def to_s()
		return null
	end

	def eval()
		return null
	end
end

#holds a number
#evaluates to itself
class Number < Expression
	def initialize(val)
		@value = val
	end

	def eval()
		return self
	end

	def value()
		return @value
	end

	def to_s()
		return @value.to_s
	end
end

#holds a string
#evaluates to itself
class Message < Expression
	def initialize(val)
		@value = val
	end

	def eval()
		return self
	end

	def to_s()
		return @value
	end
end

#holds three expressions
#does not evaluate to itself
class Application < Expression
	def initialize(exp1, exp2, exp3)
		@exp1 = exp1
		@exp2 = exp2
		@exp3 = exp3
	end

	def eval()
		toret = Array.new(3)
		toret[0] = @exp1
		toret[1] = @exp2
		toret[2] = @exp3
		while toret[0].eval != toret[0]
			toret[0] = toret[0].eval
		end
		while toret[1].eval != toret[1]
			toret[1] = toret[1].eval
		end
		while toret[2].eval != toret[2]
			toret[2] = toret[2].eval
		end

		if (toret[0].to_s()=="+")
			if (toret[1].is_a?(Number) && toret[2].is_a?(Number))
				return Number.new(toret[1].value + toret[2].value)
			else
				return Message.new("can\'t add non-numerics")
			end
		elsif (toret[0].to_s()=="-")
			if (toret[1].is_a?(Number) && toret[2].is_a?(Number))
				return Number.new(toret[1].value - toret[2].value)
			else
				return Message.new("can\'t subtract non-numerics")
			end
		elsif (toret[0].to_s()=="/")
			if (toret[1].is_a?(Number) && toret[2].is_a?(Number) && toret[2]!=0)
				return Number.new(toret[1].value / toret[2].value)
			end
			if (toret[2]==0)
				return Message.new("can't divide by zero")
			else
				return Message.new("can\'t divide non-numerics")
			end
		elsif (toret[0].to_s()=="*")
			if (toret[1].is_a?(Number) && toret[2].is_a?(Number))
				return Number.new(toret[1].value * toret[2].value)
			else
				return Message.new("can\'t multiply non-numerics")
			end
		else
			return Message.new("evaluation rules are not defined for the given operation")
		end
	end

	def to_s()
		return "App: (" + @exp1.to_s + " " + @exp2.to_s + " " + @exp3.to_s + ")"
	end
end

class Bool < Expression

	def initialize(val)
		@value = val
	end

	def to_s()
		if(@value==true)
			return "#true";
		end
		return "#false";
	end

	def eval()
		self
	end
end

#holds a string
#evaluates to itself
class Name < Expression
	def initialize(val)
		@value = val
	end

	def eval()
		return self
	end

	def to_s()
		return @value
	end
end

class Define < Expression

	def initialize(k, val)
		@key = k
		@value = val
	end

	def to_s()
		return "key: " + @key + " value: " + @value.to_s();
	end

	def eval()
		if($localVars.has_key?(@key))
			return Message.new("?error: bound name: "+ @key.to_s + " with value " + @value.to_s)
		else
			if(@key.to_s =~ /[\+\-\*\/(define)(quit)]/ || @key.to_s=="load")
				return Message.new("?eaarror: bound name: " + @key.to_s + " with value " + @value.to_s)
			else
				$localVars[@key] = @value; 	#no need to eval toret[1]
				return Message.new("");			#print nothing on eval of define
			end
		end
	end
end


repl()
