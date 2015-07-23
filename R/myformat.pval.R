myformat.pval <- function(p, cutoff = 0.0001){
  ## to format p value:
  ##- if <cutoff: report <cutoff; otherwise use only 2 decimal digits
  ## get number of digits: only work if the number of decimal places <15 (add 1 to avoid R mis-understanding)
  digits <- nchar(unlist(strsplit(as.character(cutoff + 1), split = "[.]"))[2])
  ## format
  out <- ifelse(p >= cutoff,
                format.pval(p, digits = 2),
                paste("<", formatC(cutoff, digits, format = "f"), sep = ""))
  return(out)
}