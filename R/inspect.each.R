#' @export
inspect.each <- function(x, varname, type, value = NA, check_missing = TRUE){
  tmp <- NULL

  ## find missing values
  if (check_missing) {
    if (anyNA(x) | any(as.character(x) == "")) {
      tmp <- rbind(tmp,
                   data.frame(index = which(is.na(x) | as.character(x) == ""),
                              error = paste("Missing value for", varname)))
    }
  }

  ## find outlier/out of range values
  ### datetime
  ### numeric
  if (type == "numeric" & (!is.na(value) & value != "")) {
    range <- as.numeric(unlist(strsplit(value, split = ";")))
    if (any(x < range[1] & !is.na(x))) {
      tmp <- rbind(tmp,
                   data.frame(index = which(!is.na(x) & x < range[1]),
                              error = paste("Out of range:", varname, "<", range[1])))
    }
    if (any(x > range[2] & !is.na(x))) {
      tmp <- rbind(tmp,
                   data.frame(index = which(!is.na(x) & x > range[2]),
                              error = paste("Out of range:", varname, ">", range[2])))
    }
  }
  ### factor
  if (type == "factor" & !is.na(value)) {
    range <- sapply(unlist(strsplit(value, split = ";")),
                    function(x) gsub("[']", "", unlist(strsplit(x, split = "="))[2]),
                    USE.NAMES = FALSE)

    if (any(!x %in% range & !is.na(x))) {
      xx <- x[!is.na(x) & !x %in% range]
      tmp <- rbind(tmp,
                   data.frame(index = which(!is.na(x) & !x %in% range),
                              error = paste("Out of range:", varname, "=", xx)))
    }
  }

  ## output
  return(tmp)
}