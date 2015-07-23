prop <- function(input, digits = 0, percent = TRUE, percent_sign = TRUE, short = TRUE, ci = FALSE, conf.level = 0.95){
  ## to display proportion of input
  ##- input: an expression
  ##- digits: number of decimal digits in output
  ##- percent: display percent or proportion
  ##- percent_sign: display %
  ##- short: display "<1%" if percentage of input <1
  ##- ci: display confidence interval of proportion
  ##- conf.level: confidence level

  ## check argument
  if (percent_sign == TRUE & percent == FALSE) percent_sign <- FALSE

  tmp <- factor(input, levels = c(TRUE, FALSE))
  tmp2 <- table(tmp)

  p <- tmp2["TRUE"]/sum(table(input))

  if (ci){
    prop_ci <- as.numeric(prop.test(tmp2["TRUE"], sum(tmp2), conf.level = conf.level)$conf.int)
    if (percent) prop_ci <- 100 * prop_ci
    if (percent_sign){
      ci_out <- paste("(", paste(formatC(prop_ci, digits, format = "f"), "%", sep = "", collapse = ", "), ")", sep = "")
    } else {
      ci_out <- paste("(", paste(formatC(prop_ci, digits, format = "f"), collapse = ", "), ")", sep = "")
    }
  }

  if (p < 0.01 & (short == TRUE)){
    p_out <- ifelse(percent, "<1", "<0.01")
  } else {
    p_out <- formatC(ifelse(percent, 100 * p, p), digits, format = "f")
  }

  if (percent_sign) p_out <- paste(p_out, "%", sep = "")
  out <- ifelse(ci, paste(p_out, ci_out), p_out)
  return(out)
}
