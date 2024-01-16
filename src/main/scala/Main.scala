import java.io.PrintWriter
import java.io.File
import io.circe._
import io.circe.{Decoder, Encoder}
import io.circe.generic.auto._
import io.circe.syntax._
import io.circe.parser._
import io.circe.Encoder.AsObject.importedAsObjectEncoder
import io.circe.Encoder.LowPriorityAsObjectEncoders

@main def main: Unit =
  val path: os.Path = os.pwd/"test.html"

  val content: String = os.read(path)

  // The syntax for adding for loop and if statements is: {# some-statement #}
  // Splitting on {#, makes sure that the statement is always at the start of a substring.
  // By splitting each string of the resulting array on #}, we can make sure that a statement section is always
  // the first element of one of the resulting small lists.
  // So we can pattern match on only the first element of each small list.
  val parts = content.split("\\{# ").map(_.split(" #}"))

  // Convert to a list of tuples
  // Each tuple contains a optional keyword, like for and if or no keyword.
  // The second value of the tuple indicates the corresponding html content.
  // [(None, html content), (Some(for a in b), html content), (None, html content), (Some(if a), html content)]
  val tokens = for p <- parts yield tokenize(p)

  // This is some example data that will be used to build the html file
  case class MyJSON(myList: Array[String], myCondition: Boolean)
  val data = MyJSON(myList = Array("first", "second", "third"), myCondition = true)

  val x = generate(tokens, data.asJson.noSpaces)


// -------------------------
// File generation functions
// -------------------------

// Use the properties in the data object to generate an html file
def generate(tokens: Array[(Option[For | If], String)], data: String) : Unit =
  val path = os.pwd/"out.html"

  val writer = new PrintWriter(new File("out.html"))

  for token <- tokens do
    token match
      case (Some(For(_, gen)), content) => printForContent(data, gen, content, writer)
      case (Some(If(pred)), content) => printIfContent(data, pred, content, writer)
      case (None, content) => writer.printf(content)
    
  writer.close()
  
def printForContent(data: String, generator: String, content: String, writer: PrintWriter) : Unit =
    val jsonResult: Either[ParsingFailure, Json] = parse(data)

    jsonResult match
      case Left(error) => println(error)
      case Right(json) => 
        val generatorList = (json \\ generator)(0).asArray
        generatorList match
          case None => return
          case Some(list) => 
            println(list)
            for e <- list do
              writer.print(content)


def printIfContent(data: String, pred: String, content: String, writer: PrintWriter) : Unit =
    val jsonResult: Either[ParsingFailure, Json] = parse(data)

    jsonResult match
      case Left(error) => println(error)
      case Right(json) => 
        val conditions = json \\ pred
        if conditions(0).asBoolean == Some(true) then
          writer.print(content)


// ----------------------
// File parsing functions
// ----------------------

def parseFor(section: String): Option[For] =
  val parts = section.split(" ")

  (parts(0), parts(2)) match
    case ("for", "in") => Some(For(parts(1), parts(3)))
    case _ => None

def parseIf(section: String): Option[If] =
  val parts = section.split(" ")

  parts(0) match
    case ("if") => Some(If(parts(1)))
    case _ => None

def tokenize(section: Array[String]): (Option[For | If], String) =
  val head = section(0).split(" ")

  head(0) match
    case "for" => (parseFor(section(0)), section(1))
    case "if" => (parseIf(section(0)), section(1))
    case "endfor" => (None, section(1))
    case "endif" => (None, section(1))
    case _ => (None, section(0))

// These classes are created when parsing the keywords.
// These will be used when generating the final html file.
// For example the predicate in the If class will be evaluated to determine
// whether to add html content to the final html file.
case class For(iter: String, generator: String)
case class If(predicate: String)