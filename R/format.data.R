#' @export
format.data <- function(data, info){
  # info should have these columns
  #- varname: name of variables
  #- label: label of variables
  #- type: type of variables (numeric, factor, character, datetime)
  #- unit: units of variables
  #- value: values of variables (format of datetime variables/values of categorical variables)
  #- levels: order of levels of categorical variables
  #- missing: coding for missing values

  ## recognize variable in data
  flag <- names(data)[names(data) %in% na.omit(info$varname)]
  if (length(flag) == 0){stop("No variable in this data present in info !!!")}
  info <- info[na.omit(match(flag, info$varname)),]

  ## select variables mentioned in info
  tmp <- data[, flag]

  ## if scale & center are missing --> using NA
  if (!"scale" %in% names(info)){info$scale <- NA}
  if (!"center" %in% names(info)){info$center <- NA}

  ## set type to lower case and no space
  info$type <- gsub(" ", "", tolower(info$type))

  ## perform formating
  return(data.frame(mapply(format.each,
                           x = as.list(tmp),
                           label = info$label,
                           type = info$type,
                           unit = info$unit,
                           scale = info$scale,
                           center = info$center,
                           value = info$value,
                           levels = info$levels,
                           missing = info$missing,
                           SIMPLIFY = FALSE),
                    stringsAsFactors = FALSE))
}
