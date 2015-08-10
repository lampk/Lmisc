#' To summarize fitted model
#'
#' @param fit fitted models
#' @param meta metadata
#' @param exp exponential regression coefficients and their CIs?
#' @param method estimation method?
#' @param digits decimal digit number
#' @param sep Separation between lower and upper confidence interval values
#' @param p cut-off of p value
#' @param tidy Tidy up result?
#'
#' @return Matrix
#' @export
mySummary.fit <- function(fit, meta = NULL, exp = FALSE, method = 'LRT',
                          digits = 2, sep = '-', p = 0.001, tidy = TRUE){
  # to summarise fitting result
  #browser()

  # define fit method
  fit_method <- as.character(fit$call[1])
  if (!fit_method %in% c("lm", "glm", "glm.nb", "rq", "coxph")) stop('This fitting method is not implemented yet !!!')

  # get original formula
  org_formula <- formula(fit)
  # update formula
  new_formula <- R306:::mod.formula(org_formula, meta)

  # get original data
  if (fit_method == "glm"){dat <- fit$data} else {dat <- eval(parse(text = fit$call['data']), envir = parent.frame())}
  dat <- droplevels(dat)
  attr(dat, "class") <- "data.frame"


  # update fit
  fit <- update(fit, formula. = new_formula)

  # get the content
  ## get coefficient's estimates
  est <- coef(fit)
  if (fit_method == "coxph") se <- summary(fit)$coef[, 3] else se <- summary(fit)$coef[, 2]

  ## get p values and CIs
  ### if use Wald's test
  if (method == 'Wald'){
    tmp <- cbind('coef' = est, 'se' = se,
                 'lower.CI' = (est - qnorm(0.975) * se), 'upper.CI' = (est + qnorm(0.975) * se),
                 'p.value' = 2 * pnorm(-abs(est/se)))
  }
  ### if use likelihood ratio test
  if (method == 'LRT'){
    # warning if there is interaction
    if (any(attr(terms(attr(new_formula, 'org_formula')), 'order') > 1)) stop('!!! There is interaction in your model. Use Wald method instead !!!')
    # ci
    ci <- data.frame(confint(fit))
    pval <- as.data.frame(drop1(fit, test = 'Chisq'))[-1,]
    # output
    tmp <- cbind('coef' = est, 'se' = se, 'lower.CI' = ci[, 1], 'upper.CI' = ci[, 2], 'p.value' = NA)
  }

  if (fit_method != "coxph") tmp <- tmp[-1,]
  dim(tmp) <- c(length(colnames(model.matrix(fit))[colnames(model.matrix(fit)) != "(Intercept)"]), 5)

  ## if exponential
  if (exp) {tmp1 <- cbind(exp(tmp[,1]), tmp[,2], exp(tmp[,3]), exp(tmp[,4]), tmp[,5])
  } else tmp1 <- tmp

  # get the label
  info <- data.frame(label=attr(terms(new_formula),"term.labels"), order=attr(terms(new_formula),"order"), stringsAsFactors=F)
  # info <- as.data.frame(attr(new_formula, 'label_info'), stringsAsFactors = FALSE)
  factor_matrix <- as.data.frame(attr(terms(attr(new_formula, 'org_formula')), 'factors'))[-1,]
  ## get degrees of freedom
  if (fit_method == 'coxph'){
    ndf <- attr(model.matrix(fit, data = dat), "assign")
  } else {ndf <- attr(model.matrix(formula(fit), data = dat), "assign")[-1]}

  info$df <- sapply(unique(ndf), function(x){sum(ndf == x)})

  ## get position in the summary output
  info$index_sum <- cumsum(info$df) - info$df + 1
  ## get indicator of duplicate variables
  info$notdup <- apply(info, 1, FUN = function(x){
    ifelse(nrow(info) == 1, 1,
           ifelse(x['order'] > 1, 1,
                  ifelse(sum(factor_matrix[unlist(x['label']),]) > 1, 0, 1)))
  })
  ## get position in the drop1 output
  info$index_drop <- cumsum(info$notdup)

  ## get fullname
  if (is.null(dim(factor_matrix))){
    fullvar <- fullname <- rownames(attr(terms(attr(new_formula, 'org_formula')), 'factors'))[-1]
  } else {
    fullvar <- fullname <- apply(factor_matrix, 2, function(x){rownames(factor_matrix)[x == 1]})
  }
  info$var <- sapply(info$label, function(x){all.vars(as.formula(paste(x, "~ .")))[1]})

  for (i in (1:length(fullvar))){
    for (j in (1:length(fullvar[[i]]))){
      if (attr(terms(update(fit, as.formula(paste("~", fullvar[[i]][j])))), "dataClasses")[-1] == "factor") {
        fullname[[i]][j] <- list(paste(fullvar[[i]][j], levels(dat[, info$var[info$label == fullvar[[i]][j]]])[-1], sep = ':'))
      } else {
        fullname[[i]][j] <- fullvar[[i]][j]
      }
    }
  }

  if (length(which(info$order > 1)) > 0){
    for (i in (1:length(which(info$order > 1)))){
      fullname[[which(info$order > 1)[i]]] <- list(apply(expand.grid(fullname[[which(info$order > 1)[i]]]), 1, paste, collapse = ' * '))
    }
  }

  # get the output
  result1 <- NULL
  for (i in (1:nrow(info))){
    id1 <- info$index_sum[i]; id2 <- info$index_sum[i] + info$df[i] - 1
    output.i <- matrix(tmp1[id1 : id2,], ncol = ncol(tmp1), nrow = info$df[i])
    dataClass <- attr(terms(update(fit, as.formula(paste("~", info$label[i])))), "dataClasses")[-1]
    ## add baseline
    if (length(dataClass) > 1) {
      output.add <- matrix(rep('', ncol(output.i)), nrow = 1)
      output.lab <- c(info$label[i], paste('-', unlist(fullname)[id1:id2]))
    } else {
      if (dataClass == "factor") {
        output.add <- rbind(rep('', ncol(output.i)), c(ifelse(exp, 1, 0), rep('', ncol(output.i) - 2), NA))
        output.lab <- c(info$label[i], paste('-', levels(dat[, info$var[i]])))
      } else {
        output.add <- NULL
        output.lab <- info$label[i]
      }
    }

    ## correct p value with LRT method
    if (method == 'LRT'){
      if (dataClass == "factor") {
        output.add[1, ncol(output.i)] <- ifelse(info$notdup[i] == 0, NA, pval[info$index_drop[i], "Pr(>Chi)"])
      } else {
        output.i[, ncol(output.i)] <- ifelse(info$notdup[i] == 0, NA, pval[info$index_drop[i], "Pr(>Chi)"])
      }
    }
    ## final output
    output <- cbind(output.lab, rbind(output.add, output.i))
    result1 <- rbind(result1, output)
  }

  # format output
  result2 <- result1[, -3]
  if (is.null(dim(result2))) dim(result2) <- c(1, length(result2))
  colnames(result2) <- c('Covariate', ifelse(exp, 'exp(Coefficient)', 'Coefficient'), 'lowerCI', 'upperCI', 'p value')

  if (tidy){
    result2 <- cbind(result1[, 1],
                     ifelse(is.na(as.numeric(result1[, 2])), '', formatC(as.numeric(result1[, 2]), digits, format = 'f')),
                     paste('(', ifelse(is.na(as.numeric(result1[, 4])), '', formatC(as.numeric(result1[, 4]), digits, format = 'f')), ', ',
                           ifelse(is.na(as.numeric(result1[, 5])), '', formatC(as.numeric(result1[, 5]), digits, format = 'f')), ')', sep = ''),
                     ifelse(is.na(as.numeric(result1[, 6])), '', R306::myformat.pval(as.numeric(result1[, 6]), cutoff = p)))
    result2[, 3] <- ifelse(result2[, 3] == '(, )', '', result2[, 3])
    colnames(result2) <- c('Covariate', ifelse(exp, 'exp(Coefficient)', 'Coefficient'), '(95% CI)', 'p value')
  }
  rownames(result2) <- NULL
  # output
  result2
}

#' @describeIn mySummary.fit To summarize fitted model (univariate analysis)
#' @export
mySummary.uni <- function(fit, adjust = NULL, ...){
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
                        mySummary.fit(fits, ...)[1:df,]
                      }, fit = fits, df = df, MoreArgs = list(...), SIMPLIFY = FALSE))
    colnames(output) <- colnames(mySummary.fit(fits[[1]], ...))
  }

  # out
  rownames(output) <- NULL
  return(output)
}