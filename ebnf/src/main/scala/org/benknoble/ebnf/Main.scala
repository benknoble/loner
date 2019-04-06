package org.benknoble.ebnf

import scala.io.Source

object Main {

  val usage = """usage: ebnf [-h] [-q] [file]

Formats EBNF grammar from standard in to standard out.

Exits 0 if grammar is EBNF; 1 otherwise.

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
          .map(_.format)
          .fold(l => (l, 1), r => (r, 0))
      } finally input.close()

    if (!silent)
      println(result)
    sys.exit(exit_code)
  }

}
