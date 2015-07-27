mySummary.allvar <- function(formula, data, pooledGroup = FALSE, contSummary = "med.IQR",
                             caption = NULL, kable = FALSE, test = FALSE, continuous = NA,
                             digits = 1){
  # contSummary can be median (90% range) "med.90" or median (IQR) "med.IQR" or median (range) "med.range" or "mean.sd"

  if (pooledGroup&test){
    warning("Display of both pooled groups and test currently not implemented. Tests not displayed.")
    test <- FALSE
  }

  dat <- model.frame(formula, data = data, na.action = NULL)
  if (length(formula)==2) {blvars <- dat; group <- factor(rep("All patients",nrow(data)),levels="All patients"); gr.lev <- levels(group)}
  else {
    blvars <- dat[,-1]
    if (is.null(ncol(blvars))) {
      dim(blvars) <- c(length(blvars), 1)
      colnames(blvars) <- as.character(formula[[2]])
    }
    group <- droplevels(factor(dat[,1]))
    if (is.logical(dat[,1])) gr.lev <- as.character(unique(dat[,1])) else gr.lev <- levels(dat[,1])
    if (pooledGroup) {
      mylabels <- getlabel(blvars)
      blvars <- rbind(blvars,blvars)
      for (i in 1:ncol(blvars)) attr(blvars[,i], "label") <- mylabels[i]
      group <- c(as.character(group),rep("All patients",nrow(data)))
      group <- factor(group,levels=c("All patients",gr.lev))
      gr.lev <- levels(group)
    }
  }

  gr.lev <- levels(group)
  header1 <- c("", c(rbind(rep("", length(gr.lev)), paste(gr.lev, " (N=", table(group), ")", sep = ""))))
  header2 <- c("Characteristic", rep(c("n", "Summary statistic"), length(gr.lev)))
  if (test) {
    header1 <- c(header1, "Comparison")
    header2 <- c(header2, "(p-value)")
  }
  result <-  rbind(header1, header2)
  if (length(continuous) == 1) continuous <- rep(continuous, ncol(blvars))
  if (length(digits) == 1) digits <- rep(digits, ncol(blvars))

  for (i in 1:ncol(blvars)){
    result.i <- mySummary.onevar(varname = ifelse(getlabel(blvars[, i]) != "", getlabel(blvars[, i]), getlabel(blvars)[i]),
                                 blvars[, i], group, contSummary = contSummary, test = test,
                                 continuous = continuous[i], digits = digits[i])
    result <- rbind(result, result.i)
  }
  rownames(result) <- rep("", nrow(result))

  if (kable){
    if (test){
      align <- c("l", "r", "c", "r", "c", "c")
    } else {
      align <- c("l", "r", "c", "r", "c")
    }
    result[2,] <- paste("_", result[2,], "_", sep = "_")
    tab <- kable(result[-1,],
                 row.names = FALSE,
                 col.names = header1,
                 align = align,
                 caption = caption)
    footnote <- paste("_Summary statistic is absolute count (%) for categorical variables and",
                      switch(contSummary,
                             med.IQR = "median (IQR)",
                             med.90  = "median (90% range)",
                             med.range = "median (range)",
                             mean.sd = "mean (sd)"),
                      "for continuous data._")
    if (test) footnote <- c(footnote,
                            "",
                            "_p-values based on Kruskal-Wallis/Mann-Whitney U-test (continuous data) and Fisher's exact test (categorical data)._")
    ## output
    structure(c(tab, "", footnote), format = "markdown", class = "knitr_kable")
  } else {
    result
  }
}