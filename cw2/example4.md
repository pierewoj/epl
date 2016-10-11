== Overview ==

This coursework assignment asks you to combine several concepts
covered in the course so far, to implement a simple `domain
  specific language`.  The language a lightweight `markdown
  language`.  "Markdown" is a human-readable format for hypertext,
ofren used in Wiki pages.  There are a number of different syntaxes
for markdown, and we will consider one with a core subset of features
that make it easy to translate into two other domain-specific
languages: HTML (used in Web browsers) and LaTeX (used for more
professional / technical document preparation.)

Although it is relatively straightforward to translate a markdown
language to HTML or LaTeX text directly, we ask you instead to do this
via a `pretty-printing` library, which is essentially a simple
domain-specific language embedded in Scala.  One advantage of this
approach is that it separates out some low-level concerns (such as
dealing with indentation of nested structures properly) from the
high-level translation.  You will also implement the pretty-printing
DSL itself.

Finally, you will also implement a random generator for markdown
documents that can be used to test the "correctness" of your
translations under different circumstances (e.g. by rendering the same
document in different formats and comparing).  Again, while it is possible
particularly difficult to generate random documents directly, we ask
you to do so using a `random generation` library, which is
another example of a domain-specific language embedded in Scala.
You will complete the implementation of this DSL.

In this assignment, you may use Scala standard library functions and
any features of Scala.  The assignment can be completed using
concepts covered up to lecture 11 and does not rely on mutable data
structures or imperative programming (although it may be convenient to
use them).


=== Objectives ===

The provided code file CW2.scala defines the abstract syntax
of MiniMD as well as a parser and pretty-printer for it (the latter
using the Printer interface, which you need to implement.)
We also provide a sample solution CW2Solution.jar and some
example MiniMD inputs.

The CW2.scala and CW2Solution.jar files can be
loaded into Scala as follows:
{{{
scala> :load CW2.scala
scala> :require CW2Solution.jar
}}}
In addition, both programs can be run by providing them as arguments
to Scala:
{{{
scala CW2.scala <infile> <mode> <outfile>
scala CW2Solution.jar <infile> <mode> <outfile>
}}}
Here, infile is an input file name, or RANDOM to
indicate that the input should gbe generated randomly.  The
mode is one of md, latex or html and is
used to choose which format to use for the results. Finally,
outfile is an optional output file name; if omitted, the
output is just printed to the screen.

You do not need to worry about dealing with ("escaping") LaTeX or
HTML special characters embedded in the AST strings, as in the
following MiniMD document:
{{{
This will \emph{render} <b>differently</b> in 
<em>HTML</em> and \textbf{LaTeX}!
}}}
This is a legitimate problem that would need to be solved in a
full-scale MiniMD rendering engine, but it is not particularly
significant from our point of view and we will not deduct credit for
failure to handle such scenarios correctly.


