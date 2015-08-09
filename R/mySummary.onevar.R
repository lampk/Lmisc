#' @export
mySummary.onevar <- function(varname, variable, group = NULL, continuous = NA, contSummary = "med.IQR", test = FALSE, digits = 1){
  if (is.na(continuous)) continuous <- ifelse(is.factor(variable) | length(unique(na.omit(variable))) <= 5, FALSE, TRUE)

  mycont.summary <- function(variable,group,test,digits) {
    if (is.null(group)) {
      ngroup <- 1
      summarystat.nice <- cont.Summary(unclass(variable), contSummary = contSummary, digits = digits, n = FALSE)
      n <- length(na.omit(variable))
    } else {
      ngroup <- length(levels(group))
      summarystat.nice <- by(unclass(variable), group, cont.Summary, contSummary = contSummary, digits = digits, n = FALSE)
      n <- c(by(variable, group, function(x) length(na.omit(x))))
    }

    result <- matrix("", ncol = ngroup * 2 + 1, nrow = 1)
    result[1, seq(2, ncol(result), by = 2)] <- n
    result[1, seq(3, ncol(result), by = 2)] <- unlist(summarystat.nice)
    if (test == TRUE & !is.null(group)) {
      # overall Kruskal-Wallis test for group differences
      pval <- myformat.pval(kruskal.test(variable ~ group)$p.value, cutoff = 0.0001)
      result <- cbind(result, pval)
    }
    result
  }

  mycat.summary <- function(variable, group, test, digits) {
    if (is.null(group)) {
      ngroup <- 1
      ta <- table(variable)
      ta.prop <- ta/sum(ta)
      dim(ta) <- c(ngroup, length(ta))
      colnames(ta) <- names(table(variable))
    } else {
      ngroup <- length(levels(group))
      ta <- table(group, variable)
      ta.prop <- unclass(ta/apply(ta, 1, sum))
    }

    ta.nice <- matrix(paste(ta," (", formatC(100*unclass(ta.prop), digits, format = "f"), "%", ")", sep = ""),
                      nrow = nrow(ta), ncol = ncol(ta))
    result <- matrix("", ncol = ngroup * 2 + 1, nrow = ncol(ta) + 1)
    result[2:nrow(result), 1] <- paste("- ", colnames(ta))
    result[2:nrow(result), seq(3, ncol(result), by = 2)] <- t(ta.nice)
    result[1, seq(2, ncol(result), by = 2)] <- apply(ta, 1, sum) # n's
    if (test) {
      # Fisher's exact test for group differences
      pval <- myformat.pval(fisher.test(ta)$p.value, cutoff = 0.0001)
      result <- cbind(result, "")
      result[1,ngroup * 2 + 2] <- pval
    }
    result
  }

  if (continuous) r <- mycont.summary(variable, group, test, digits)
  else r <- mycat.summary(variable, group, test, digits)
  r[1, 1] <- varname
  r
}