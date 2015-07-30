linear.test <- function(model, var, trans = c("quadratic", "spline"), df = 4, data){
  if (length(trans) > 1) trans <- "quadratic"

  org_formula <- formula(model)
  termlabs <- labels(terms(var))
  old_formula <- paste(". ~ . - ", termlabs, "+", termlabs)
  new_formula <- switch(trans,
                        quadratic = paste(". ~ . - ", termlabs, "+", termlabs, "+ I(", termlabs, "^2)"),
                        spline = paste(". ~ . - ", termlabs, "+", "ns(", termlabs, ", df = ", df, ")"))

  if ("mids" %in% class(data)){
    requireNamespace("mice")
    tmp <- mapply(function(x, y){
      call1 <- model$call[1:3]; call1[["formula"]] <- update(org_formula, as.formula(x))
      call0 <- model$call[1:3]; call0[["formula"]] <- update(org_formula, as.formula(y))
      fit1 <- with(data, eval(parse(text = paste("expression(", paste(deparse(call1), collapse = ""), ")"))))
      fit0 <- with(data, eval(parse(text = paste("expression(", paste(deparse(call0), collapse = ""), ")"))))
      mice::pool.compare(fit1, fit0, method = "likelihood", data = data)$pvalue
    }, x = new_formula, y = old_formula)
    output <- tmp
    names(output) <- termlabs
  } else {
    tmp <- mapply(function(x, y){
      fit1 <- update(model, as.formula(x), data = data)
      fit0 <- update(model, as.formula(y), data = data)
      as.numeric(anova(fit0, fit1, test = "Chisq")[2, c(4, 3, 5)])
    }, x = new_formula, y = old_formula, SIMPLIFY = FALSE)
    output <- do.call(rbind, tmp)
    colnames(output) <- c("Deviance", "Df", "Pr(>Chi)")
    rownames(output) <- termlabs
  }

  return(output)
}