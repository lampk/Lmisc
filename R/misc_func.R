
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
