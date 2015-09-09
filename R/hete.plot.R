#' To produce heterogeneity plot
#'
#' @param formula outcome ~ independent variable
#' @param data required data
#' @param value value of independent variable
#'
#' @return graph
#' @export
#'
hete.plot <- function(formula, data, value = NULL) {
  ## get text
  tmptext <- hete.est(formula, data, value = NULL)
  ## plot
  rmeta::forestplot(labeltext = tmptext[, 1:4],
                    mean = c(NA, as.numeric(tmptext[-1, 4])),
                    lower = c(NA, as.numeric(tmptext[-1, 6])),
                    upper = c(NA, as.numeric(tmptext[-1, 7])),
                    zero = 0,
                    is.summary = c(TRUE, rep(FALSE, nrow(tmptext) - 1)))
}

hete.est <- function(formula, data, value = NULL) {
  # check formula
  if (length(formula) != 3) {stop("Please check formula !!!")}
  # data
  tmpdat <- model.frame(formula, data = data, na.action = NULL)
  # get summary table
  tmp <- table(tmpdat[,2], tmpdat[,1])
  n <- apply(tmp, 1, sum)
  p <- tmp[, "TRUE"]/n
  se_p <- sqrt(p * (1 - p) / n)
  tmp2 <- cbind(n,
                tmp[, "TRUE"],
                formatC(p, format = "f", digits = 2),
                se_p,
                p - 2 * se_p,
                p + 2 * se_p)
  tmp3 <- tmp2[tmp2[, 2] > 0,]
  if (is.null(value)) {
    value <- rownames(tmp3)
  }
  out <- rbind(c("Value", "n", "Freq", "Prop", "SE", "lo", "hi"),
               cbind(row_names, tmp3))
  dimnames(out) <- NULL
  return(out)
}

