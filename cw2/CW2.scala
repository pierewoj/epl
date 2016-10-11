
import scala.util.parsing.combinator.RegexParsers
import scala.util.parsing.combinator.syntactical.StandardTokenParsers

object CW2 {

  /* =====  Abstract syntax tree for MiniMD  ===== */
 abstract class MiniMDExpr
 // A block of MiniMD expressions. The top-level of a program is a MDDoc.
 case class MDDoc(contents: List[MiniMDExpr]) extends MiniMDExpr

 // A paragraph of free text / bold / italic / section headers
 case class MDPar(contents: List[MiniMDExpr]) extends MiniMDExpr
 case class MDVerbatim(contents: String) extends MiniMDExpr

 // Free text
 case class MDFreeText(text: String) extends MiniMDExpr
 case class MDBold(text: String) extends MiniMDExpr
 case class MDItalic(text: String) extends MiniMDExpr
 case class MDUnderlined(text: String) extends MiniMDExpr


 // List items
 case class MDListItem(exprs: List[MiniMDExpr]) extends MiniMDExpr

 case class MDBulletedList(listItems: List[MDListItem]) extends MiniMDExpr
 case class MDNumberedList(listItems: List[MDListItem]) extends MiniMDExpr

 // Sections; `text` is the section header name. 
 case class MDSectionHeader(text: String) extends MiniMDExpr
 case class MDSubsectionHeader(text: String) extends MiniMDExpr

 // Links
 case class MDLink(text: String, url: String) extends MiniMDExpr


 /* ===== Printing Language ===== */
import scala.language.implicitConversions


// ======================================================================
// Part 1: Pretty-printing
// ======================================================================

trait Printer {
  // The abstract Doc type
  type Doc

  // An empty document
  val nil : Doc

  // A block of text
  def text(s: String): Doc

  // A line break / newline
  val line: Doc

  // Append two Docs together
  def append(x:Doc, y: Doc): Doc

  // A doc which is nested to a certain level.
  def nest(i: Int, doc: Doc): Doc

  // A doc whose content is rendered without indentation
  // (nesting is suspended)
  def unnest(doc: Doc): Doc

  // A method which prints the Doc to a string.
  def print(doc: Doc): String

  // some Scala incantations to allow infix <> document append operation
  class AppendAssoc(x: Doc) {
    def <> (y: Doc): Doc = append(x,y)
  }
  implicit def doc2AppendAssoc(x: Doc): AppendAssoc = new AppendAssoc(x)



  // ======================================================================
  //  Exercise 1             
  // ====================================================================== 

  def quote(d1: Doc): Doc = text("\"") <> d1 <> text("\"")

  def braces(d1: Doc): Doc = text("{") <> d1 <> text("}")

  def anglebrackets(d1: Doc): Doc = text("<") <> d1 <> text(">")

  /* ======================================================================
   *  Exercise 2
   * ====================================================================== */

  def sep(d: Doc, ds: List[Doc]): Doc = 
    ds.foldLeft(List[Doc]())((acc,el) => d :: el :: acc).tail
      .foldLeft(nil)((acc,el) => el <> acc)
}


// ======================================================================
// Exercise 3
// ======================================================================

//  The following instance is a stub, provided just to make the
// rest of the code compile.

object MyPrinter extends Printer {
  type Doc = (Int, Boolean) => String
  val nil = {(i:Int,s:Boolean) => ""}
  def text(s: String) = {(i:Int,susp:Boolean) => s}
  val line = {(i:Int, susp:Boolean) =>  if(susp){"\n"} else{"\n" + " "*i}}
  def append(x:Doc, y: Doc) = {(i:Int,s:Boolean) =>x(i,s) + y(i,s)}
  def nest(i: Int, doc: Doc) = {(int:Int, susp:Boolean) => doc(int+i,susp)}
  def unnest(doc: Doc) = {(int:Int, susp:Boolean) => doc(int,true)}
  def print(doc: Doc) = doc(0,false)
}

// Import it for use in the rest of the program
import MyPrinter._

// A Formatter[T] knows how to turn T into a Doc.
trait Formatter[T] {
  // The abstract method that needs to be implemented.
  def format(e: T): Doc
  // The method formatList applies format to each element of a list, concatenating the results
  // It can be called from format().
  def formatList(xs: List[T]): Doc = sep(nil,xs.map{x: T => format(x)})
}

// ======================================================================
// Exercise 4
// ======================================================================

object MarkdownFormatter extends Formatter[MiniMDExpr] {

  def format(e: MiniMDExpr) = 
    e match 
    {
      case MDDoc(l) => formatList(l)
      case MDPar(l) => formatList(l) <> line <> line
      case MDVerbatim(s) => text("{{{")<> line <> text(s) <> line <> text("}}}") <> line
      case MDFreeText(s) => text(s)
      case MDBold(s) => text("*") <> text(s) <> text("*")
      case MDItalic(s) => text("`") <> text(s) <> text("`")
      case MDUnderlined(s) => text("_") <> text(s) <> text("_")
      case MDListItem(l) => formatList(l)
      case MDBulletedList(l) => 
        l.foldLeft(nil){ case (acc,MDListItem(exprs)) => 
        acc <> text("* ") <> formatList(exprs) <> line } <> line
      case MDNumberedList(l) => 
        l.foldLeft((nil,1)){ case ((acc,num),MDListItem(exprs)) => 
        (acc <> text(num.toString) <> text(". ") <> formatList(exprs) <> line , num+1)}._1 <> line
      case MDSectionHeader(s) => 
        text("== ") <> text(s) <> text(" ==") <> line
      case MDSubsectionHeader(s) => 
        text("=== ") <> text(s) <> text(" ===") <> line
      case MDLink(s,url) =>
        text("(") <> text(s) <> text(")[") <> text(url) <> text("]")
      case _ => sys.error("Unmatched case in MarkDownFormatter")
    }

}

/* ======================================================================
 *  Exercise 5
 * ====================================================================== */

object LatexFormatter extends Formatter[MiniMDExpr] {

  def format(e: MiniMDExpr) =
    e match 
    {
      case MDDoc(l) => formatList(l)
      case MDPar(l) => formatList(l)  <> line <> line
      case MDVerbatim(s) => text("\\begin{verbatim}") <> line <> unnest(text(s)) <> 
      text("\\end{verbatim}") <> line
      case MDFreeText(s) => text(s)
      case MDBold(s) => text("\\textbf{") <> text(s) <> text("}")
      case MDItalic(s) => text("\\textit{") <> text(s) <> text("}")
      case MDUnderlined(s) => text("\\underline{") <> text(s) <> text("}")
      case MDListItem(l) => formatList(l)
      case MDBulletedList(l) => 
        text("\\begin{itemize}") <> nest(2,
          l.foldLeft(nil){ case (acc,list) => 
          acc <> line <> text("\\item ") <> format(list)}) <> 
        line <> text("\\end{itemize}") <> line <> line
      case MDNumberedList(l) => 
        text("\\begin{enumerate}") <> nest(2,
          l.foldLeft(nil){ case (acc,list) => 
          acc <> line <> text("\\item ") <> format(list)}) <> 
        line <> text("\\end{enumerate}") <> line <> line <> line
      case MDSectionHeader(s) => 
        text("\\section{") <> text(s) <> text("}") <> line
      case MDSubsectionHeader(s) => 
        text("\\subsection{") <> text(s) <> text("}") <> line
      case MDLink(s,url) =>
        text("\\href{") <> text(url) <> text("}{") <> text(s) <> text("}")
      case _ => sys.error("Unmatched case in LatexFormatter")
    }

}

// ======================================================================
//  Exercise 6
// ====================================================================== 


object HTMLFormatter extends Formatter[MiniMDExpr] {

  def format(e: MiniMDExpr) = 
    e match 
    {
      case MDDoc(l) => formatList(l)
      case MDPar(l) => text("<p>") <> formatList(l) <> text("</p>")  <> line <> line
      case MDVerbatim(s) => text("<pre>") <> unnest(text(s)) <> text("</pre>") <> line
      case MDFreeText(s) => text(s)
      case MDBold(s) => text("<b>") <> text(s) <> text("</b>")
      case MDItalic(s) => text("<i>") <> text(s) <> text("</i>")
      case MDUnderlined(s) => text("<u>") <> text(s) <> text("</u>")
      case MDListItem(l) => formatList(l)
      case MDBulletedList(l) => 
        text("<ul>") <> nest(2,
          l.foldLeft(nil){ case (acc,list) => 
          acc <> line <> text("<li>") <> format(list) <> text("</li>")}) <> 
        line <> text("</ul>") <> line
      case MDNumberedList(l) => 
        text("<ol>") <> nest(2,
          l.foldLeft(nil){ case (acc,list) => 
          acc <> line <> text("<li>") <> format(list) <> text("</li>")}) <> 
        line <> text("</ol>") <> line
      case MDSectionHeader(s) => 
        text("<h1>") <> text(s) <> text("</h1>") <> line
      case MDSubsectionHeader(s) => 
        text("<h2>") <> text(s) <> text("</h2>") <> line
      case MDLink(s,url) =>
        text("<a href=\"") <> text(url) <> text("\">") <> text(s) <> text("</a>")
      case _ => sys.error("Unmatched case in HTMLFormatter")
    }

}

// ======================================================================
// Part 2: Random test case generation
// ======================================================================

object Rng {


  // A Gen[T] has a method called get that generates a random T.
  trait Gen[+A] { 
    val rng = scala.util.Random

    def get(): A // abstract

    // Gen[T] also supports the map and flatMap methods, so for-comprehensions
    // can be used with them.
    def map[B](f: A => B):Gen[B] =
    {
      val self = this
      new Gen[B] { def get() = f(self.get()) }
    } 

    // **********************************************************************
    // Exercise 7
    // **********************************************************************

    def flatMap[B](f: A => Gen[B]):Gen[B] = 
    {
      val self = this
      new Gen[B] { def get() = f(self.get()).get}
    }
  }

  // **********************************************************************
  // Exercise 8
  // **********************************************************************

  def const[T](c: T): Gen[T] =
    new Gen[T] {
      def get() = c;
    }

  def flip: Gen[Boolean] =
    new Gen[Boolean] {
      def get() = rng.nextBoolean()
    }

  def range(min: Integer, max: Integer): Gen[Integer] =
    new Gen[Integer] {
      def get() = min + rng.nextInt(max-min+1)
    }


  def fromList[T](items: List[T]): Gen[T] =
    new Gen[T] {
      def get() = items(rng.nextInt(items.length))
    }

  // **********************************************************************
  // Exercise 9
  // **********************************************************************

  def genSectionText: Gen[String] =
    fromList(List(
      "Chapter 1", "Introduction", "Conclusion"
    ))

  def genSubsectionText: Gen[String] =
    fromList(List(
      "Section 1.1", "Table of Contents", "References"
    ))

  def genListitemText: Gen[String] =
    fromList(List(
      "Apples", "Oranges", "Spaceship"
    ))

  def genLink: Gen[(String, String)] =
    fromList(List(
      ("Google","http://www.google.com"), 
        ("Facebook", "http://www.facebook.com")
          ))

  def genFreeText: Gen[String] =
    fromList(List(
      "It was a dark and stormy night",
      "It was the best of times"
    ))

  def genVerbatimText: Gen[String] =
    fromList(List(
      "== This isn't valid MiniMD ===", 
      "Neither_ _is' 'this*"
    ))


  // **********************************************************************
  // Exercise 10
  // **********************************************************************

  def genList[T](n: Integer, g: Gen[T]): Gen[List[T]] = 
    new Gen[List[T]] {
      def get() =  (1 to n).toList.map( _ => g.get ) //m-m-magic 
    }

  def genFromList[A](gs: List[Gen[A]]): Gen[A] = 
    fromList(gs).map(gen => gen.get)
  

  // **********************************************************************
  // Exercise 11
  // **********************************************************************

  def GenMDPar : Gen[MiniMDExpr] = 
    range(3,5).flatMap(i => genList(i,
      genFromList( 
        List(
          genFreeText.map(x => MDFreeText(x)),
          genFreeText.map(x => MDBold(x)),
          genFreeText.map(x => MDItalic(x)),
          genFreeText.map(x => MDUnderlined(x)),
          genLink.map( {case (t,l) => MDLink(t,l)} ),
          genVerbatimText.map(x => MDVerbatim(x))
        ) 
      )
    )
    ).map(x => MDPar(x))

  // Gen[MDListItem]
  val GenMDListItem : Gen[MDListItem] = 
    genListitemText.map( x => MDFreeText(x) ).map( x => MDListItem( List(x) ))

  val GenMDBulletedList : Gen[MiniMDExpr] =
    range(2,4).flatMap(i => genList(i, GenMDListItem)).map(x => MDBulletedList(x))

  val GenMDNumberedList : Gen[MiniMDExpr] = 
    range(2,4).flatMap(i => genList(i, GenMDListItem)).map(x => MDNumberedList(x))

  def genMiniMDExpr(n: Integer): Gen[MiniMDExpr] = 
    genList(n, 
      genFromList( 
        List(
          GenMDPar,  
          GenMDBulletedList,
          GenMDNumberedList,
          genSectionText.map(x => MDSectionHeader(x)),
          genSubsectionText.map(x => MDSubsectionHeader(x))
        ) 
      ) 
    
    ).map( l => MDDoc(l))
  
}




/*======================================================================
  The rest of this file is support code, which you should not (and do not
    need to) change.
====================================================================== */
  /* ===== MiniMD Parser ===== */

 object MiniMDParser extends RegexParsers {
   val eol = sys.props("line.separator")

   // Here be dragons.

   type P[+A] = Parser[A]
   private val eoi = """\z""".r // end of input
   private val separator = eoi | eol

   override val skipWhitespace = false

   // Paragraph: Either a long line of free text with the whitespace
   // stripped out, or a list with whitespace kept in
   type Line = String
   type Paragraph = String



   // Parses an input file
   def parse(input: String): MiniMDExpr = {
     val source = scala.io.Source.fromFile(input)
     val lines = try source.mkString finally source.close()
     val paragraphs = tokeniseParagraphs(lines)

     println("Paragraphs:")
     println(paragraphs + "\n\n")
     val parsedParagraphs = paragraphs.flatMap((par: Paragraph) => parseParagraph(par))

     normalise(MDDoc(parsedParagraphs))
   }

   // Top-level parse function: takes a string, returns a MiniMDExpr.
   // Throws an error upon failure.
   def parseParagraph(input: String): List[MiniMDExpr] = {
     if (input.trim.length == 0) { Nil }
     else {
       parseAll(paragraph, input) match {
         case Success(ast, _) => List(ast)
         case (e: NoSuccess) => {
           sys.error(e.msg + ", " + e.next.pos + ", " + e.next.source)
           Nil
         }
       }
     }
   }


   // Given an input string, generates a list of paragraphs
   def tokeniseParagraphs(input: String): List[Paragraph] = {

     def isEmptyLine(s: String) = s.trim.length == 0

     def isBulletListItem(s: String) = s.startsWith("* ")
     def isNumberListItem(s: String) = """^\d+\. """.r.findFirstIn(s).isDefined
     def isListItem(s: String) = isBulletListItem(s) || isNumberListItem(s)

     def isStartVerbatim(s: String) = s.trim == "{{{"
     def isEndVerbatim(s: String) = s.trim == "}}}"
     def isSection(s: String) = s.trim.startsWith("==")


     def gatherList(isBulleted: Boolean, par: Paragraph,
       remainder: List[Line]): (Paragraph, List[Line]) = remainder match {
         case Nil => (par, remainder)
         case x::xs =>
           if (isBulleted && isBulletListItem(x)) {
             gatherList(isBulleted, par + x, xs)
           } else if (!isBulleted && isNumberListItem(x)) {
             gatherList(isBulleted, par + x, xs)
           } else if (isEmptyLine(x)) {
             (par, xs)
           } else {
             (par, remainder)
           }
       }

       def gatherParagraph(par: Paragraph, remainder: List[Line]):
       (Paragraph, List[Line]) =
         remainder match {
           case Nil => (par, remainder)
           case x::xs =>
             if (isEmptyLine(x)) {
               (par, xs)
             } else if (isListItem(x) || isStartVerbatim(x)) {
               (par, remainder)
             } else {
               gatherParagraph(par + x.stripLineEnd + " ", xs)
             }
         }

         def gatherVerbatim(par: Paragraph, remainder: List[Line]):
         (Paragraph, List[Line]) =
           remainder match {
             case Nil => (par, remainder)
             case x::xs =>
               if (isEndVerbatim(x)) {
                 (par + x.trim, xs)
               } else {
                 gatherVerbatim(par + x, xs)
               }
           }

           def eatEmptyLines(remainder: List[Line]):
           (List[Line]) =
             remainder match {
               case Nil => Nil
               case x::xs =>
                 if (isEmptyLine(x)) {
                   eatEmptyLines(xs)
                 } else {
                   remainder
                 }
             }

             def doTokeniseParagraphs(remainder: List[Line]): List[Paragraph] =
               remainder match {
                 case Nil => Nil
                 case x::xs =>
                   if (isEmptyLine(x)) {
                     doTokeniseParagraphs(xs)
                   } else if (isSection(x)) {
                     x.stripLineEnd::(doTokeniseParagraphs(xs))
                   } else if (isStartVerbatim(x)) {
                     val (par, newRemainder) = gatherVerbatim("", remainder)
                     par::(doTokeniseParagraphs(newRemainder))
                   } else if (isBulletListItem(x)) {
                     val (par, newRemainder) = gatherList(true, "", remainder)
                     par::(doTokeniseParagraphs(newRemainder))
                   } else if (isNumberListItem(x)) {
                     val (par, newRemainder) = gatherList(false, "", remainder)
                     par::(doTokeniseParagraphs(newRemainder))
                   } else {
                     val (par, newRemainder) = gatherParagraph("", remainder)
                     par::(doTokeniseParagraphs(newRemainder))
                   }
               }

             val linesList = input.linesWithSeparators.toList
             doTokeniseParagraphs(linesList)
   }

   def delimitedSequence(delimiter: Char, breakOnNL: Boolean): P[String] = {
     (rep(delimitedChar(delimiter, breakOnNL)) <~ aChar) ^^ {
       case seq => seq.mkString
     }
   }

   def num: P[Int] = "[0-9]+".r ^^ { case n => n.toInt }

   def delimitedChar(delimiter: Char, breakOnNL: Boolean): P[Char] =
     acceptIf {ch => (ch != delimiter) || (breakOnNL && ch == eol)} {_ => "Delimiter char reached"}

   def aChar: P[Char] = Parser { in =>
     if (in.atEnd) {
       Failure("End of input", in)
     } else {
       Success(in.first, in.rest)
     }
   }

   def isDelimiterChar(c: Char): Boolean =
     c match {
       case '*' => true
       case '_' => true
       case '`' => true
       case '=' => true
       case '\r' => true
       case '\n' => true
       case '(' => true
         case '{' => true
           case _ => false
         }

         def bulletedListItem = {
           ("* " ~> listExpr) <~ eol ^^ { case e => MDListItem(e) }
         }

         def bulletedList: P[MiniMDExpr] = {
           rep1(bulletedListItem) ^^ { case seq => MDBulletedList(seq) }
         }

         def numberedListItem: P[MDListItem] =
           ((num <~ ". ") ~ listExpr) <~ eol ^^ {
             case (num ~ e) => MDListItem(e)
           }

         def numberedList: P[MiniMDExpr] = {
           rep1(numberedListItem) ^^ { case seq => MDNumberedList(seq) }
         }

         def parseDelimiterRun(breakOnNL: Boolean): P[MiniMDExpr] = {
           ("*" ~> delimitedSequence('*', breakOnNL) ^^ { case str => MDBold(str) }) |
           ("`" ~> delimitedSequence('`', breakOnNL) ^^ { case str => MDItalic(str) }) |
           ("_" ~> delimitedSequence('_', breakOnNL) ^^ { case str => MDUnderlined(str) })
         }

         def freeChar: P[Char] =
           acceptIf {ch => !isDelimiterChar(ch)} {_ => "Delimiter char reached"}

         // Parse until we hit a delimiter character.
         def freeText: P[MiniMDExpr] =
           aChar ~ rep(freeChar) ^^ { case ch ~ xs => MDFreeText((ch::xs).mkString) }

         def subsectionHeader: P[MiniMDExpr] =
           ("===" ~> "[^=]+".r <~ "===[ ]*".r) ^^ { case headertxt => MDSubsectionHeader(headertxt.trim) }

         def sectionHeader: P[MiniMDExpr] =
           ("==" ~> "[^=]+".r <~ "==[ ]*".r) ^^ { case headertxt => MDSectionHeader(headertxt.trim) }

         def link: P[MiniMDExpr] =
           ("(" ~> ("[^)]*".r) <~ ")") ~ ("[" ~> ("[^\\]]*".r) <~ "]") ^^ {
             case desc~url => MDLink(desc, url)
           }

         def verbatim: P[MiniMDExpr] =
           ("{{{" ~> eol ~> """(?s).*?(?=}}})""".r.unanchored <~ "}}}") ^^ { case vrb => MDVerbatim(vrb) }

         def listExpr: P[List[MiniMDExpr]] = {
           rep1((guard(not(eol))) ~> (parseDelimiterRun(true) | freeText))
         }

         def expr: P[MiniMDExpr] = {
           parseDelimiterRun(false) | link | freeText
         }

         def plainPar: P[MiniMDExpr] = {
           rep1(expr) ^^ { xs => MDPar(xs) }
         }

         def paragraph: P[MiniMDExpr] =
           subsectionHeader | sectionHeader | bulletedList | numberedList | verbatim | plainPar



         /* Normalisation pass.
          * We'll get a stream of FreeText / lists / section headers from
          * the parser.
          * We want to ensure that if we have two consecutive FreeTexts,
          * that they're combined into one.
          */
         def normaliseInner(es: List[MiniMDExpr]): List[MiniMDExpr] = es match {
           case Nil => Nil
           case MDFreeText(s1)::MDFreeText(s2)::xs => normaliseInner(MDFreeText(s1 ++ s2)::xs)
           case e::xs => normalise(e)::normaliseInner(xs)
         }

         def normalise(e: MiniMDExpr): MiniMDExpr = e match {
           case MDDoc(xs) => MDDoc(normaliseInner(xs))
           case MDPar(xs) => MDPar(normaliseInner(xs))
           case MDBulletedList(xs) =>
             MDBulletedList(xs.map(x => normalise(x).asInstanceOf[MDListItem]))
           case MDNumberedList(xs) =>
             MDNumberedList(xs.map(x => normalise(x).asInstanceOf[MDListItem]))
           case MDListItem(xs) => MDListItem(normaliseInner(xs))
           case e => e
         }

  }


  object Main {
    def usage() {
      println("Usage: scala CW2Solution.jar <infile> <mode> <outfile>")
      println("<infile> is the input file name, or RANDOM to generate a random test")
      println("<mode> is one of \"html\", \"md\" or \"latex\", and defaults to \"md\"")
      println("<outfile> is optional and defaults to \"output\"")
    }

    def getInput(infile: String): MiniMDExpr = {
      if (infile == "RANDOM") {
        println("Generating random test file...")
        Rng.genMiniMDExpr(100).get()
      }
    else {
      println("Reading " + infile)
      MiniMDParser.parse(infile)
    }
    }

    def printToFile(f: java.io.File)(op: java.io.PrintWriter => Unit) {
      val p = new java.io.PrintWriter(f)
      try { op(p) } finally { p.close() }
    }

    def writeHtml(outfile:String, html: String) {
      println("Writing " + outfile)
      printToFile(new java.io.File(outfile)) { out =>
        out.write("<html>\n<body>\n<!-- Beginning of MiniMD code -->\n")
        out.write(html)
        out.write("<!-- End of MiniMD code -->\n</body>\n")
      }
    }

    def writeMd(outfile: String, md: String) {
      println("Writing " + outfile)
      printToFile(new java.io.File(outfile)) { out =>
        out.write(md)
      }
    }

    def writeLatex(outfile: String, latex: String) {
      println("Writing " + outfile)
      printToFile(new java.io.File(outfile)) { out =>
        out.write("\\documentclass{article}\n" +
          "\\usepackage[colorlinks=true]{hyperref}\n" +
          "\\begin{document}\n" +
          "%%% Beginning of MiniMD content\n")
        out.write(latex)
        out.write("%%% End of MiniMD content\n" +
          "\\end{document}\n")
      }
    }

  }
  def main(args: Array[String]): Unit = {
    if (args.length >= 1 ) {
      val infile = args(0)
      val mode =  if (args.length >= 2) { args(1) } else { "md" }
      // Check that outfile is one of html, md or latex
      if (mode == "html" || mode == "md" || mode == "latex") {

        val parsed = Main.getInput(infile)

        mode match {
          case "html" => {
            println("Generating HTML...")
            val genHtml = print(HTMLFormatter.format(parsed))
            println(genHtml)
            if (args.length >= 3) {
              Main.writeHtml(args(2), genHtml)
            }
          }
          case "md" => {
            println("Generating MiniMD...")
            val genMd = print(MarkdownFormatter.format(parsed))
            println(genMd)
            if (args.length >= 3) {
              Main.writeMd(args(2), genMd)
            }
          }
          case "latex" => {
            println("Generating LaTeX...")
            val genLatex = print(LatexFormatter.format(parsed))
            println(genLatex)
            if (args.length >= 3) {
              Main.writeLatex(args(2),genLatex)
            }
          }
        }
        } else {
          Main.usage()
        }
        } else {
          Main.usage()
        }
  }
}
