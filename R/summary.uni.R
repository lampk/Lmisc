summary.uni <- function(fit, adjust = NULL, ...){
  # create list of new formulas
  old <- formula(fit)
  x <- attr(terms(old), "term.labels")
  if (is.null(adjust) == FALSE) x <- paste(x, adjust, sep = "+")
  new <- sapply(x, function(z) as.formula(paste(old[2], z, sep = "~")))

  # create list of fit
  fits <- mapply(update, formula. = new, MoreArgs = list(object = fit), SIMPLIFY = FALSE)

  # get df for each variable
  ## get original data
  if (as.character(fit$call[1]) == "glm") {
    dat <- fit$data
  } else {
      dat <- eval(parse(text = fit$call['data']), envir = parent.frame())
  }
  dat <- droplevels(dat)
  ndf <- attr(model.matrix(formula(fit), data = dat), "assign")[-1]
  df <- sapply(unique(ndf), function(x){sum(ndf == x)})
  dataClass <- attr(terms(fit), "dataClasses")[-1]
  df <- ifelse(dataClass == "factor", df + 2, df)

  # create output
  if (length(fits) == 1) {
    output <- summary.fit(fits, ...)[1:df[1],]
  } else {
    output <- do.call(rbind,
                      mapply(function(fits, df, ...) {
                        summary.fit(fits, ...)[1:df,]
                      }, fit = fits, df = df, MoreArgs = list(...), SIMPLIFY = FALSE))
    colnames(output) <- colnames(summary.fit(fits[[1]], ...))
  }

  # out
  rownames(output) <- NULL
  return(output)
}