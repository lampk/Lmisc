#' @export
stepmi <- function(formula, data, method = c("Majority", "Wald"), p = 1, p_value = 0.05){
  require(mice)
  if (length(method) > 1) method <- "Wald"

  if (method == "Majority"){
    fit <- with(data, eval(parse(text = paste("expression(f1 <- glm(", paste(deparse(formula), collapse = ""), ", family = binomial), f2 <- step(f1, trace = 0))"))))
    formulas <- lapply(fit$analyses, formula)
    terms <- lapply(formulas, terms)
    res <- table(unlist(lapply(terms, labels)))
    vars <- paste(names(res)[res >= (p * data$m)], collapse = "+")
    output <- list(formula = update(formula, as.formula(paste(". ~ ", vars))), detail = res)
  }

  if (method == "Wald"){
    i <- 1
    res <- list(formula)
    cat("Step:", "\n")
    while (i > 0) {
      cat(i)
      cat("\r")
      termlabs <- labels(terms(formula))
      formula_character0 <- paste(". ~ . - ", termlabs)
      formula_character1 <- paste(". ~ . + ", termlabs)
      formula0 <- sapply(formula_character0, function(x) update(formula, as.formula(x)))
      formula1 <- mapply(function(x, y){update(x, as.formula(y))}, x = formula0, y = formula_character1)

      tmp <- mapply(function(x, y){
        fit1 <- with(data, eval(parse(text = paste("expression(glm(", paste(deparse(x), collapse = ""), ", family = binomial))"))))
        fit0 <- with(data, eval(parse(text = paste("expression(glm(", paste(deparse(y), collapse = ""), ", family = binomial))"))))
        pool.compare(fit1, fit0, method = "Wald")$pvalue
      }, x = formula1, y = formula0)

      res <- c(res, list(tmp))
      idx <- which.max(tmp)
      if (tmp[idx] > p_value){
        formula <- formula0[[idx]]
        i <- i + 1
      } else {i <- 0}
    }
    cat("\n")
    output <- list(formula = formula, detail = res)
  }
  return(output)
}