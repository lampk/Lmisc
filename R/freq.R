freq <- function(input, prop = TRUE, digits = 0, short = TRUE){
  ## to display frequency of input
  ##- prop: display percentage
  ##- digits: number of decimal digits
  ##- short: display <1 if prop < 1%
  tmp <- factor(input, levels = c(TRUE, FALSE))
  tmp1 <- paste(table(tmp)["TRUE"], sum(table(tmp)), sep = "/")
  if (prop){
    p <- 100 * table(tmp)["TRUE"]/sum(table(tmp))
    if ((p < 1) & (short == TRUE)){out <- "<1"} else {out <- as.numeric(round(p, digits))}
    return(paste(tmp1, " (", out, "%)", sep = ""))
  } else {
    return(tmp1)
  }
}