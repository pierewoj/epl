import CW2._
import CW2.MyPrinter._


// Basics:
type Test = (String,Doc)

def test(test: Test) {
  println("Test " + test._1)
  println("----------------------------")
  println(print(test._2))
  println("----------------------------")
}



/* Expected:
Test helloWorldPlain
----------------------------
helloworld
----------------------------
*/
val helloWorldPlain: Test =
  ("helloWorldPlain",text("hello") <> text("world"))

/* Expected:
Test helloWorldNewline
----------------------------
hello
world
----------------------------
 */
val helloWorldNewline: Test =
  ("helloWorldNewline",text("hello") <> line <> text("world"))

/* Expected:
Test helloWorldNewlineNest
----------------------------
hello
  world
----------------------------
 */
val helloWorldNewlineNest: Test =
  ("helloWorldNewlineNest",
    text("hello") <> nest(2, line <> text("world")))

/* Expected:
hello
world
 */
val helloWorldNewlineNotNested: Test =
  ("helloWorldNewlineNotNested", text("hello") <> line <> nest(2, text("world")))

/* Expected:
Test ifStatement
----------------------------
def f(x: Int) = {
  if (x < 1) {
    println(x + 1)
  }
}
---------------------------- 
 */
val ifStatement: Test = ("ifStatement",
    // Nesting level: 0
    text("def f(x: Int) = {") <>
    // Nesting level: 2
    nest(2, line <> text("if (x < 1) {") <>
      // Nesting level: 4
      nest(2, line <> text("println(x + 1)")) <>
      // Nesting level: 2
      line <> text("}")
    ) <>
    // Nesting level: 0
    line <> text("}")
  )


/* Expected:
Test unnestInsideNest
----------------------------
not nested
  nested at level 2
    nested at level 4
not nested
still not nested
    nested at level 4
  nested at level 2
not nested
----------------------------

 */
val unnestInsideNest: Test = ("unnestInsideNest",
    text("not nested") <>
    nest(2, line <> text("nested at level 2") <>
      nest(2, line <> text("nested at level 4") <>
        unnest(line <> text("not nested") <>
          nest(2, line <> text("still not nested"))) <>
      line <> text("nested at level 4")) <>
    line <> text("nested at level 2")) <>
    line <> text("not nested")
  )


test(helloWorldPlain)
test(helloWorldNewline)
test(helloWorldNewlineNest)
test(helloWorldNewlineNotNested)
test(ifStatement)
test(unnestInsideNest)




// vim: set ts=2 sw=2 et sts=2:
