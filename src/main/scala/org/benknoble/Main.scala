package org.benknoble.loner

import org.benknoble.ebnf.EbnfParser

import scala.io.Source

object Main {

  val usage = """usage: loner [-h] [-q] [file]

Recieves grammar on stdin. Writes it on stdout if LL(1).

Exits 0 if grammar is LL(1); 1 otherwise.

Options:
  -h  --  this help message
  -q  --  silent: do not print formatted grammar
                  do not print error messages
  [file]  --  read from the provided file instead of standard in
"""

  def error(msg: String): Unit = System.err.println(msg)

  def display_usage_and_exit: Unit = {
    error(usage)
    sys.exit(2)
  }

  def main(args: Array[String]): Unit = {
    var vargs = args

    if (vargs.contains("-h")) display_usage_and_exit
    vargs = args.filterNot(_ == "-h")

    val silent = vargs.contains("-q")
    vargs = args.filterNot(_ == "-q")

    if (vargs.length > 1) display_usage_and_exit

    val input =
      if (vargs.length == 1)
        Source.fromFile(vargs(0))
      else
        // don't accidentally close stdin?
        Source.stdin.withClose(() => {})

    val (result, exit_code) =
      try {
        EbnfParser(input.getLines.mkString("\n"))
          .fold(
            msg => (msg, 1),
            g => if (Loner(g)) (g.format, 0) else ("Not LL(1)", 1))
      } finally input.close()

    if (!silent)
      println(result)
    sys.exit(exit_code)
  }

}
