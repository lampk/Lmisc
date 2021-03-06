
# function to convert a long data frame with derived variables into mids object
# source: http://stackoverflow.com/questions/26667162/perform-operation-on-each-imputed-dataset-in-rs-mice, mice
as.mids2 <- function(data2, .imp = 1, .id = 2, .idx){
  m <- max(as.numeric(levels(data2[,  .imp])))
  dat <- data2[data2[, .imp] == 0, -c(.imp, .id)]

  ## run mice
  ini <- mice::mice(dat, m = m, maxit=0)
  names  <- names(ini$imp)
  if (!is.null(.id)){
    rownames(ini$data) <- data2[data2[, .imp] == 0, .id]
  }

  ## create data frame to store imputed values for additional variables
  ini$imp[[names[.idx]]] <- as.data.frame(matrix(ncol = m, nrow = sum(is.na(dat[, names[.idx]]))))

  ## "correct" imputed values
  for (i in 1:length(names)){
    for(j in 1:m){
      if(!is.null(ini$imp[[i]])){
        indic <- data2[, .imp] == j &  is.na(data2[data2[, .imp]==0, names[i]])
        ini$imp[[names[i]]][j] <- data2[indic, names[i]]
      }
    }
  }

  ## output
  return(ini)
}


# function to perform drop1 for geeglm objects ----------------------------

drop1.geeglm <- function(object) {
  ## prepare
  all <- formula(object)
  x <- attr(object$terms, "term.labels")
  new <- sapply(x, function(z) update(all, paste(". ~ .", z, sep = "-")))
  data <- object$data

  ## get output
  output <- do.call(rbind, mapply(function(z) {
    newfit <- update(object, formula = z, data = data)
    anova(object, newfit)
  }, z = new, SIMPLIFY = FALSE))

  ## modify output attributes
  attr(output, "heading") <- c("Single term deletions",
                               paste("Model:", deparse(all)))
  names(output)[names(output) == "P(>|Chi|)"] <- "Pr(>Chi)"

  ## return
  return(output)
}


# function to modify formula (drop each variable or create univari --------

adj.formula <- function(formula, adjust = NULL, type = c("drop", "uni")) {
  if (length(type) > 1) {type <- "drop"}
  x <- attr(terms(formula), "term.labels")
  if (type == "drop") {
    newformula <- sapply(x, function(z) update(formula, paste(". ~ .", z, sep = "-")))
  } else {
    newformula <- sapply(x, function(z) update(formula, paste(". ~ ", z)))
  }

  if (is.null(adjust) == FALSE) {
    newformula <- sapply(paste(newformula, adjust, sep = " + "), as.formula)
  }

  return(newformula)
}


# function to convert OUCRU data dictionary into my favorite varia --------

#' @export
convert.info <- function(oucru_info, oucru_category) {
  require(dplyr)
  require(car)
  ## get value & level for factors
  cat_tmp <- distinct(oucru_category) %>%
    mutate(database_value = ifelse(is.na(suppressWarnings(as.numeric(`Database value`))),
                                   paste("'", `Database value`, "'", sep = ""),
                                   `Database value`),
           title_en = paste("'", `Title EN`, "'", sep = "")) %>%
    group_by(`Category Code`) %>%
    summarise(value = paste(database_value, title_en, sep = "=", collapse = ";"),
              level = paste(title_en, collapse = ";")) %>%
    ungroup() %>%
    rename(`Value range` = `Category Code`)

  ## convert
  output <- merge(oucru_info, cat_tmp, by = "Value range", all.x = TRUE) %>%
    transmute(varname = `Field Name`,
              label   = `Title EN`,
              type    = Recode(`Data Type`,
                               recodes = "c('Category', 'ExCategory') = 'factor';
                               c('Free Text', 'Title', 'CombinedKey', 'Time', 'Check') = 'character';
                               c('EDateTime', 'SDateTime') = 'datetime';
                               c('Integer', 'Float') = 'numeric';
                               else = NA"),
              unit    = NA,
              value   = ifelse(is.na(type), NA,
                               ifelse(type == "factor", value,
                                      ifelse(type == "character", NA,
                                             ifelse(type == "datetime", "ymd_hms",
                                                    ifelse(is.na(`Value range`), NA,
                                                           gsub(pattern = "-", replacement = ";", x = `Value range`)))))),
              levels  = ifelse(is.na(type), NA,
                               ifelse(type != "factor", NA, level)),
              missing = NA)
  return(output)
}


# update R and related software (for Window) ------------------------------

#' @export
my.update <- function() {
  require(installr)
  ## update R
  updateR(browse_news = FALSE,
          install_R = TRUE,
          copy_packages = TRUE,
          copy_Rprofile.site = ,
          keep_old_packages = FALSE,
          update_packages = TRUE,
          start_new_R = FALSE,
          quit_R = FALSE,
          silent = TRUE)

  ## install Rtools
  install.Rtools()

  ## install pandoc
  install.pandoc()

  ## update RStudio
  install.RStudio()
}


# load my favourite R packages --------------------------------------------

#' @export
myLibrary <- function(packages = c("tidyverse", "lubridate", "scales", "Hmisc", "R306")){
  id <- packages[packages %in% installed.packages()[, "Package"]]
  for (i in id){library(i, character.only = TRUE)}
  cat("The following R packages are loaded: ", paste(id, collapse = ","), ".\n", sep = "")
  if (length(id) < length(packages)){
    install.packages(pkgs = packages[!packages %in% id], dependencies = TRUE)
  }
}

# draw Venn Diagram -------------------------------------------------------

#' @export
myVennCount <- function(data, variables) {
  tmpdat <- data[, variables]
  n <- length(variables)
  idx <- lapply(1:n, function(x) combn(1:n, x))
  out <- lapply(1:n, function(x){
    tmp <- idx[[x]]
    tmp2 <- matrix(paste("(tmpdat[,", tmp, "] == 1)", sep = ""), ncol = ncol(tmp))
    value <- apply(tmp2, 2, function(y){eval(parse(text = paste("sum(", paste(y, collapse = "&"), ")", sep = "")))})
    names(value)  <- apply(tmp,  2, function(z){ifelse(x == 1, ifelse(n > 1, paste("area", z, sep = ""), "area"), paste("n", paste(z, collapse = ""), sep = ""))})
    if (n == 2){names(value)[names(value) == "n12"] <- "cross.area"}
    return(value)
  })
  return(unlist(out))
}

#' @export
myVenn <- function(data, variables){
  require(VennDiagram)
  m <- length(variables)
  n <- myVennCount(data, variables)
  func <- ifelse(m == 1, "draw.single.venn",
                 ifelse(m == 2, "draw.pairwise.venn",
                        ifelse(m == 3, "draw.triple.venn",
                               ifelse(m == 4, "draw.quad.venn",
                                      ifelse(m == 5, "draw.quintuple.venn", NA)))))
  if (is.na(func)){stop("Unable to draw Venn diagram for more than 5 variables !!!")}
  eval(parse(text = paste(func, "(", paste(paste(paste(names(n), n, sep = "="), collapse = ","), ",category = c(", paste(paste("'", variables, "'", sep = ""), collapse = ","), ")", sep = ""), ")", sep = "")))
}


# emulate ggplot2 colors --------------------------------------------------

#' @export
gg_color <- function(n) {
  ## source from: https://stackoverflow.com/questions/8197559/emulate-ggplot2-default-color-palette
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}


# gam (mgcv) fit ----------------------------------------------------------

#' @export
my.gam.plot <- function(gamfit, filename, width = 4000, height = 4000, res = 300, label = NULL, unit = NULL, rug = TRUE, se = TRUE, shade = TRUE, scale = -1, lwd = 1.5, ...) {
  require(mgcv)
  ## set up
  n_smooth <- length(gamfit$smooth)
  nrow <- floor(sqrt(n_smooth))
  if (nrow * (nrow + 1) < n_smooth) {nrow <- nrow + 1}
  if (nrow * (nrow + 1) > n_smooth) {ncol <- nrow} else {ncol <- nrow + 1}
  if (is.null(label)) {
    label <- sapply(1:n_smooth, function(i){gamfit$smooth[[i]]$vn})
  }
  label_y <- paste("s(", label, ")", sep = "")
  if (!is.null(unit)) {
    label_x <- paste(label, unit)
  } else {
    label_x <- label
  }

  ## plot
  png(filename = filename, width = width, height = height, res = 300)
  par(mfrow = c(nrow, ncol))
  for (i in (1:n_smooth)) {
    gamfit$smooth[[i]]$label <- label_y[i]
    plot(gamfit, select = i, xlab = label_x[i], rug = rug, se = se, shade = shade, scale = scale, lwd = lwd, ...)
    abline(h = 0, lty = 2)
  }
  par(mfrow = c(1, 1))
  dev.off()
}


# get vcov and prediction or effect ---------------------------------------

#' @export
my.vcov <- function(fit){
  if ("geeglm" %in% class(fit)) {
    out <- fit$geese$vbeta
  } else {
    out <- vcov(fit)
  }
  return(out)
}

#' @export
get.prediction <- function(fit, X, exp = FALSE, alpha = 0.05, sub = NULL) {
  #browser()
  if (is.null(sub)) {
    idx <- 1:ncol(X)
  } else {
    idx <- sub
  }
  Xnew <- X[, idx]
  if (length(idx) == 1) {
    dim(Xnew) <- c(nrow(X), 1)
  }

  pred <- (coef(fit)[idx] %*% t(Xnew))[1,]
  pred_vcov <- my.vcov(fit)[idx, idx]
  pred_se <- sqrt(diag(Xnew %*% pred_vcov %*% t(Xnew)))
  out <- cbind(pred = pred,
               lo = pred - qnorm(1 - alpha/2) * pred_se,
               hi = pred + qnorm(1 - alpha/2) * pred_se)
  if (exp) {out <- exp(out)}
  return(out)
}


# password ----------------------------------------------------------------

create.pw <- function(keynum, length = 12) {
  m <- round(length/3)
  punc <- c("!", "#", "$", "%", "&", "(", ")", "*", "+" , "-", ".", "/", ":", ";", "<", "=", ">", "?", "@", "[", "]", "^", "_", "{", "|", "}", "~")
  set.seed(keynum)
  p0 <- c(sample(letters, m), sample(LETTERS, m), sample(punc, length - (2 * m)))
  return(paste(sample(p0, length(p0)), collapse = ""))
}

create.pwd <- function(keynum, length = 12) {
  m <- round(length/4)
  punc <- c("!", "#", "$", "%", "&", "(", ")", "*", "+" , "-", ".", "/", ":", ";", "<", "=", ">", "?", "@", "[", "]", "^", "_", "{", "|", "}", "~")
  set.seed(keynum)
  p0 <- c(sample(letters, m), sample(LETTERS, m), sample(0:9, m), sample(punc, length - (3 * m)))
  return(paste(sample(p0, length(p0)), collapse = ""))
}


# file --------------------------------------------------------------------

#' @export
file.move <- function(from, to) {
  ## ref: https://stackoverflow.com/questions/10266963/moving-files-between-folders
  ## copy and rename file
  todir <- dirname(to)
  if (!isTRUE(file.info(todir)$isdir)) dir.create(todir, recursive = TRUE)
  file.rename(from = from,  to = to)
}

