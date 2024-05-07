sealed trait token

case class tChar(value: Char) extends token
case class tString(value: String) extends token
case object tWhiteSpace extends token
case class tMany(value: List[token]) extends token

type LexData = (token, String)

class LexException(s: String) extends Exception(s) {}

def lex(file: String): Unit =
    val res = lex_char(file, 'h')
    println(res)

    val res2 = lex_some("hhhhallo", (f) => lex_char(f, 'h'))
    println(res2)

// --------------
// Lexer function
// --------------

@throws(classOf[LexException])
def lex_char(file: String, c: Char): LexData =
    val content: String = file

    if (content.length() == 0)
        throw LexException("No file left to parse")

    if (content(0) != c)
        throw LexException(s"Incorrect char, excpected ${c}, but got ${content(0)}")

    return (tChar(content(0)), content.drop(1))

@throws(classOf[LexException])
def lex_str(file: String, str: tString): LexData =
    var str_value = str.value
    var parsed_res = Seq.empty[token]

    while str_value.length() > 0 do
        val res = lex_char(file, str_value(0))
        str_value = str_value.drop(1)

    return (tString("hallo"), file)

// ---------------
// Abstract lexers
// ---------------

// Requires at least one token to be parsed correctly
def lex_some(file: String, callback: (String) => LexData): LexData =
    var lex_result: LexData = lex_many(file, callback)

    val token: tMany = lex_result._1 match
        case tMany(value) => tMany(value)
        case other => throw LexException(s"Expected lex_many to return a token of type tMany, but got ${other}")

    if token.value.length == 0 then
        throw LexException("At least one token expected to be parsed")

    return lex_result

    // if lex_result_some._1 then
    //     throw LexException(s"At least one token expected of type: $t")
    //     throw LexException(s"At least one token expected of type: $t")

    // return newFile

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