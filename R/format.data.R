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
  flag <- names(data)[names(data) %in% info$varname]
  info <- info[na.omit(match(info$varname, flag)),]

  ## select variables mentioned in info
  tmp <- data[, flag]

  ## if scale & center are missing --> using NA
  if (!"scale" %in% names(info)){info$scale <- NA}
  if (!"center" %in% names(info)){info$center <- NA}

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