#' @export
myformat.each <- function(x, label = NA, type, unit = NA, scale = NA, center = NA, value = NA, levels = NA, missing = NA){
  type <- tolower(type)
  ## missing data
  if (!is.na(missing)){
    missing <- gsub("['']", "", unlist(strsplit(as.character(missing), split = ";")))
    for (i in missing) x[x == i] <- NA
  }

  ## check type
  if (!type %in% c("numeric", "datetime", "character", "factor")){
    stop("This type of data is not implemented yet !!!")
  }

  ## numeric data
  if (type == "numeric"){
    tmp <- as.numeric(x)
    structure(tmp,
              class = c("avector", class(x)),
              label = ifelse(is.na(label), "", label),
              unit = ifelse(is.na(unit), "", unit),
              scale = ifelse(is.na(scale), 1, scale),
              center = ifelse(is.na(center), 0, center))
  }

  ## datetime data
  if (type == "datetime"){
    requireNamespace("lubridate")
    tmp <- eval(parse(text = paste("lubridate::", value, "(as.character(x))", sep = "")))
    structure(tmp, class = c("avector", class(tmp)), label = ifelse(is.na(label), "", label))
  }

  ## character data
  if (type == "character"){
    x[x == ""] <- NA
    if (is.na(value)){tmp <- as.character(as.vector(x))} else {
      requireNamespace("car")
      tmp <- as.character(car::Recode(var = x, recodes = value, as.factor.result = FALSE))
      structure(tmp, class = c("avector", class(tmp)), label = ifelse(is.na(label), "", label))
    }
  }

  ## factor data
  if (type == "factor"){
    requireNamespace("car")
    if (is.na(levels)|levels == ""){
      tmp <- car::Recode(var = x, recodes = value, as.factor.result = TRUE)
    } else {
      tmp <- car::Recode(var = x, recodes = value, as.factor.result = TRUE,
                         levels = gsub("['']", "", unlist(strsplit(levels, split = ";"))))
    }
    structure(tmp, class = c("avector", class(tmp)), label = ifelse(is.na(label), "", label))
  }

  ## return
  return(tmp)
}
