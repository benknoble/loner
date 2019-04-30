const indent_amt = 2

function format_scala_rep(s) {
  let indent = 0
  const newline = match => (match === ")" ? "" : "\n")
  const formatted = s.replace(/(\w+\()|\)/g, match => {
    if (match === ")") {
      indent = (indent - indent_amt) < 0 ? 0 : indent - indent_amt
      return match
    } else {
      const old_indent = indent
      indent = indent + indent_amt
      return ' '.repeat(old_indent) + match + "\n"
    }
  }).replace(/,\s*/g, match => {
    // no loner necessary; can just be ",\n", but just in case...
    const whitespace = match.replace(/,/, "")
    const white_len = whitespace.length
    const new_white = ' '.repeat(
      white_len % indent_amt == 0
      ? white_len
      : white_len - 1)
    return ",\n" + new_white
  }).replace(/\w+\(\n((.*,)|(.+\)+))/g, match => match.split("\n").join(""))
  return formatted
}

function output_handler() {
  handle('format_output', g => EbnfParser.parseJS_format(g))
  handle('scala_output', g => {
    const scala_rep = EbnfParser.parseJS_scala(g)
    return format_scala_rep(scala_rep)
  })
  handle('LLone_output', g => Loner.isLLone(EbnfParser.parseJS_grammar(g)))
}

function handle(id, func) {
  const input = document.getElementById('input').value
  const result = func(input)
  const code = document.createElement('pre')
  const text = document.createTextNode(result)
  code.appendChild(text)
  const output = document.getElementById(id)
  remove_children(output)
  output.appendChild(code)
}

function remove_children(element) {
  while(element.firstChild) { element.firstChild.remove(); }
}

function setup_input_and_output() {
  const initial_text = '<A> ::= a | b | {c};'
  const input = document.getElementById('input')
  input.innerText = initial_text
  output_handler()
}

/* equivalent to $(document).ready
 * see also: https://stackoverflow.com/q/799981/4400820
 */
document.addEventListener("DOMContentLoaded", function(event) {
  //do work
  setup_input_and_output()
});
