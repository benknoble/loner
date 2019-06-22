const indent_amt = 2
const eps = '\u03b5'

function format_scala_rep(s) {
  let indent = 0
  const newline = match => (match === ")" ? "" : "\n")
  // unicode is eps as defined above. Didn't want to mess around with string
  // escapes to get a variable into a regex
  const formatted = s.replace(/\u03b5|(\w+\()|\)/g, match => {
    if (match === ")") {
      indent = (indent - indent_amt) < 0 ? 0 : indent - indent_amt
      return match
    } else if (match === eps) {
      return ' '.repeat(indent) + match
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
  const initial_text = '# This is a comment\n' +
                       "&lt;A&gt; ::= 'a' | 'b' | {&lt;C&gt;};\n" +
                       "&lt;B&gt; ::= [&lt;A&gt;]d&lt;C&gt;;\n" +
                       "&lt;C&gt; ::= 'c';\n" +
                       "&lt;A&gt; ::= 'edit me' | " + eps + ';\n'
  const input = document.getElementById('input')
  input.innerHTML = initial_text
  output_handler()
}

/* equivalent to $(document).ready
 * see also: https://stackoverflow.com/q/799981/4400820
 */
document.addEventListener("DOMContentLoaded", function(event) {
  //do work
  setup_input_and_output()
});
