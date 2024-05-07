sealed trait token

case class tChar(value: Char) extends token
case class tString(value: String) extends token
case object tWhiteSpace extends token
case class tMany(value: List[token]) extends token

case object tBracketOpen extends token
case object tBracketClose extends token
case object tForSign extends token
case object tInSign extends token
case object tIfSign extends token
case object tElseSign extends token

type LexData = (token, String)

class LexException(s: String) extends Exception(s) {}

def lex(): Unit =
    val file = "{ for x in xs }"

    val res = lex_brackets_open(file)
    println(res)

// --------------
// Lexer function
// --------------

def lex_brackets_open(file: String): LexData =
    val result = lex_char(file, '{')
    return (tBracketOpen, result._2)

def lex_brackets_close(file: String): LexData =
    val result = lex_char(file, '}')
    return (tBracketClose, result._2)

def lex_for(file: String): LexData = 
    val result = lex_str(file, "for")
    return (tForSign, result._2)

@throws(classOf[LexException])
def lex_char(file: String, c: Char): LexData =

    if (file.length() == 0)
        throw LexException("No file left to parse")

    if (file(0) != c)
        throw LexException(s"Incorrect char, excpected ${c}, but got ${file(0)}")

    return (tChar(file(0)), file.drop(1))

@throws(classOf[LexException])
def lex_whitespace(file: String): LexData = 
    return lex_char(file, ' ')

@throws(classOf[LexException])
def lex_str(file: String, str: String): LexData =
    var res = lex_char(file, str(0))

    for (c <- str.drop(1))
        res = lex_char(res._2, c)
    
    return (tString(str), res._2)

// ---------------
// Abstract lexers
// ---------------

@throws(classOf[LexException])
def lex_any(file: String) : LexData = 
    if (file.length() == 0)
        throw LexException("No file left to parse")

    return (tChar(file(0)), file.drop(1))

// Requires at least one token to be parsed correctly
def lex_some(file: String, callback: (String) => LexData): LexData =
    var lex_result: LexData = lex_many(file, callback)

    val token: tMany = lex_result._1 match
        case tMany(value) => tMany(value)
        case other => throw LexException(s"Expected lex_many to return a token of type tMany, but got ${other}")

    if token.value.length == 0 then
        throw LexException("At least one token expected to be parsed")

    return lex_result

// Expects zero, one or more tokens to be parsed
@throws(classOf[Throwable])
def lex_many(file: String, callback: (String) => LexData): (tMany, String) =
    var newFile: String = file
    var tokens: List[token] = List.empty[token]
    var continue_parsing = true
    while continue_parsing do
        try {
            val res = callback(newFile)
            newFile = res._2
            tokens = tokens :+ res._1
        } catch {
            case e: LexException => continue_parsing = false
            case e: Throwable => throw e
        }

    return (tMany(tokens), newFile)
