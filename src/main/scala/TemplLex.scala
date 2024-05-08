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

def lex_brackets_open(data: LexData): LexData =
    val result = lex_char(data, '{')
    return (replace_token(result, BracketOpen))

def lex_brackets_close(data: LexData): LexData =
    val result = lex_char(data, '}')
    return (replace_token(result, tBracketClose))

def lex_for(data: LexData): LexData = 
    val result = lex_str(data, "for")
    return (replace_token(result, tForSign))

@throws(classOf[LexException])
def lex_char(data: LexData, c: Char): LexData =
    val file = data._2

    if (file.length() == 0)
        throw LexException("No file left to parse")

    if (file(0) != c)
        throw LexException(s"Incorrect char, excpected ${c}, but got ${file(0)}")

    return (data._1 :+ tChar(file(0)), file.drop(1))

@throws(classOf[LexException])
def lex_whitespace(data: LexData): LexData = 
    return lex_char(data, ' ')

@throws(classOf[LexException])
def lex_str(data: LexData, str: String): LexData =
    var res = lex_char(data, str(0))

    for (c <- str.drop(1)) {
        res = lex_char(res, c)
    }

    return (tString(str), res._2)

// ---------------
// Abstract lexers
// ---------------

def replace_token(data: LexData, t: token) : LexData = 
    tokens = data._2

    return (tokens.pop(1) :+ t, data._1)

@throws(classOf[LexException])
def lex_any(data: LexData) : LexData = 
    val file = data._2    
    if (file.length() == 0)
        throw LexException("No file left to parse")

    return (data._1 :+ tChar(file(0)), file.drop(1))

// Requires at least one token to be parsed correctly
def lex_some(data: LexData, callback: (String) => LexData): LexData =
    var lex_result: LexData = lex_many(data, callback)

    val token: tMany = lex_result._1 match
        case tMany(value) => tMany(value)
        case other => throw LexException(s"Expected lex_many to return a token of type tMany, but got ${other}")

    if token.value.length == 0 then
        throw LexException("At least one token expected to be parsed")

    return lex_result

// Expects zero, one or more tokens to be parsed
@throws(classOf[Throwable])
def lex_many(file: LexData, callback: (String) => LexData): (tMany, String) =
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
