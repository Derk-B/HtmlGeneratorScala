import scala.NonEmptyTuple
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

type LexData = (List[token], String)

class LexException(s: String) extends Exception(s) {}

def lex(): Unit =
    val file = "{ for x in xs }"
    val lexDataInit = (List.empty[token], file)

    val res1 = lex_str(lexDataInit, "{ for ")
    print(res1)

    val res = lex_brackets_open(lexDataInit)
    println(res)

// --------------
// Lexer function
// --------------

def lex_brackets_open(data: LexData): LexData =
    val result = lex_char(data, '{')

    return replace_last_token(result, tBracketOpen)

def lex_brackets_close(data: LexData): LexData =
    val result = lex_char(data, '}')

    return replace_last_token(result, tBracketClose)

def lex_for(data: LexData): LexData = 
    val result = lex_str(data, "for")

    return replace_last_token(result, tForSign)

def lex_char(data: LexData, c: Char): LexData =
    val file = data._2

    if (file.length() == 0)
        print("No file left to parse")
        return (List.empty[token], file)

    if (file(0) != c)
        print(s"Incorrect char, excpected ${c}, but got ${file(0)}")
        return (List.empty[token], file)

    return (data._1 :+ tChar(file(0)), file.drop(1))

def lex_whitespace(data: LexData): LexData = 
    return lex_char(data, ' ')

def lex_str(data: LexData, str: String): LexData =
    val str_zipped = str.zip(data._2.take(str.length()))
    val res1 = str_zipped.fold(tString(value = "")){
        (acc, cs) => 
            print(cs)
            // if (cs._1 == cs._2) {
            //     tString(acc.value :+ cs._2)
            // } else {
            //     tString(value = "")
            // }
            tString("")
    }

    print(res1)

    return (res, data._2.drop(str.length()))
    
    // var res = (List[token](), "")
    
    // lex_char(data, str(0))

    // for (c <- str.drop(1)) {
    //     res = lex_char(res, c)

    //     if (res._1.length == 0) {
    //         return res
    //     }
    // }

    // return (data._1 :+ tString(str), res._2)

// ---------------
// Abstract lexers
// ---------------

def replace_last_token(data: LexData, t: token) : LexData = 
    val tokens = data._1

    if (tokens.isEmpty) {
        return (List.empty[token], data._2)
    }

    return (tokens.dropRight(1) :+ t, data._2)

def lex_any(data: LexData) : LexData = 
    val file = data._2    
    if (file.length() == 0)
        print("No file left to parse")
        return (List[token](), file)

    return (data._1 :+ tChar(file(0)), file.drop(1))

// Requires at least one token to be parsed correctly
def lex_some(data: LexData, callback: (LexData) => LexData): LexData =
    var lex_result: LexData = (List.empty, "")
    
    lex_result = lex_many(data, callback)

    var token: tMany = tMany(value = List.empty)
    
    lex_result._1.last match
        case tMany(value) => token = tMany(value)
        case other => {
            print(s"Expected lex_many to return a token of type tMany, but got ${other}")
        }

    if token.value.length == 0 then
        print("At least one token expected to be parsed")
        return (List.empty[token], lex_result._2)

    return lex_result

// Expects zero, one or more tokens to be parsed
def lex_many(data: LexData, callback: (LexData) => LexData): LexData =
    var newData: LexData = data
    var tokens: List[token] = List.empty[token]
    var continue_parsing = true
    while continue_parsing do {
        newData = callback(newData)
        tokens = tokens :+ newData._1.last 
    }

    return (data._1 :+ tMany(tokens), newData._2)

