package org.benknoble.loner

/** Provides classes for dealing with context-free grammars in EBNF form.
  *
  * Provides a [[org.benknoble.loner.ebnf.Grammar]] model, complete with scala
  * DSL for creating complex grammars in code, and an
  * [[org.benknoble.loner.ebnf.EbnfParser]] to generate Grammars from a String.
  *
  * ==Overview==
  * The main class is [[org.benknoble.loner.ebnf.Grammar]]:
  * {{{
  * scala> val grammar = Grammar(
  *      |   new Production(
  *      |     Nonterminal('A),
  *      |     Alternation(Nonterminal('A),
  *      |     Sequence(Repetition(Terminal("abc")), Option(Terminal("def"))))))
  * grammar: org.benknoble.loner.ebnf.Grammar = Grammar(List(Production(Nonterminal('A), Alternation(Nonterminal('A),Sequence(Repetition(Terminal(abc)),Option(Terminal(def)))))))
  *
  * scala> val s = grammar.format
  * s: String = <A> ::= <A>|{abc}[def] ;
  * }}}
  *
  * If you include [[org.benknoble.loner.ebnf.ExprImplicits]] and take advantage
  * of [[org.benknoble.loner.ebnf.Expr]] syntax, it looks a lot cleaner:
  * {{{
  * scala> val grammar = Grammar('A ::= 'A || Terminal("abc").* ~ "def".?)
  * grammar: org.benknoble.loner.ebnf.Grammar = Grammar(List(Production(Nonterminal('A), Alternation(Nonterminal('A),Sequence(Repetition(Terminal(abc)),Option(Terminal(def)))))))
  *
  * scala> val s = grammar.format
  * s: String = <A> ::= <A>|{abc}[def] ;
  * }}}
  *
  * Finally, you can take advantage of [[org.benknoble.loner.ebnf.EbnfParser]]:
  * {{{
  * scala> val grammar = EbnfParser("&lt;A&gt; ::= &lt;A&gt;|{abc}[def] ;")
  * grammar: Either[String,org.benknoble.loner.ebnf.Grammar] = Right(Grammar(List(Production(Nonterminal('A), Alternation(Nonterminal('A),Sequence(Repetition(Terminal(abc)),Option(Terminal(def))))))))
  *
  * scala> val msg = grammar.fold(s => s, g => g.format)
  * msg: String = <A> ::= <A>|{abc}[def] ;
  * }}}
  *
  */
package object ebnf {
}
