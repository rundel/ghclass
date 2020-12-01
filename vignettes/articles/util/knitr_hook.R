find_pat = function(s, pat, start = 1, loc = c("after", "before")) {
  loc = match.arg(loc)

  s = substr(s, start, nchar(s))
  p = regexec(pat, s)[[1]]
  if (p == -1) return((start-1) + nchar(s))

  l = attr(p, "match.length")
  attributes(p) = NULL

  if (loc == "after")
    (start-1) + p + l
  else
    (start-1) + p
}

find_input = function(str, start = 1, engine = "r", fence_char = "`") {
  s = substr(str, start, nchar(str))

  engine = tolower(engine)
  input_pattern = paste0("\n[", fence_char, "]{3,}", engine, "\n")
  output_pattern = paste0("\n[", fence_char, "]{3,}\n")

  cur = find_pat(s, input_pattern, start = 1)
  write_cur = 1
  result = ""

  while(cur != nchar(s)) {

    cur = find_pat(s, output_pattern, start = cur)

    next_out = find_pat(s, output_pattern, start = cur, loc = "before")
    next_in  = find_pat(s, input_pattern,  start = cur, loc = "before")

    if (next_in <= next_out) {
      # <= covers the case where we're at the end of the chunk
      cur = next_in
      next
    }

    # There is output to save, so save everything up to this point into result
    result = paste0(result, substr(s, write_cur, next_out - 1))
    write_cur = next_in

    # Fix the output text

    out_txt = substr(s, next_out, next_in - 1)
    out_txt = gsub(
      paste0("\n([", fence_char, "]{3,})\n+\\1\n"),
      "\n", out_txt
    )
    out_txt = gsub(
      paste0("^[\n", fence_char, "]*|[\n", fence_char, "]*$"),
      "", out_txt
    )
    if (fansi::has_sgr(out_txt)) {
      out_txt = fansi::html_code_block(
        fansi::sgr_to_html(
          fansi::html_esc(out_txt)
        ),
        class = "fansi"
      )
    } else {
      out_txt = paste0("```\n", out_txt, "\n```\n")
    }

    result = paste0(result, "\n", out_txt, "\n")
    cur = next_in
  }

  if (write_cur != nchar(s))
    result = paste0(result, substr(s, write_cur, nchar(s)))

  result
}
