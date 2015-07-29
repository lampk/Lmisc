getlabel.data <- function(data, meta = NULL, units = TRUE, fit = FALSE){
  # to create polished label for variables in data frame data from meta information

  out <- names(data)

  if (!is.null(meta)) {
    namedat <- merge(data.frame(name = out), meta, by = 'name', all.x = TRUE)

    # extract meta info
    namedat <- transform(.label = ifelse(is.na(label), name, label),
                         .units = ifelse(is.na(units), '', units),
                         .scale = ifelse(is.na(scale),  1, scale))

    # create label
    if (units == FALSE) {
      out <- as.character(namedat$.label)
    } else {
      if (fit) {namedat$.units <- with(namedat, paste('+', .scale, ' ', .units, sep = ''))}
      out <- with(namedat, ifelse(type == 'factor', as.character(.label), as.character(paste(.label, ' [', .units, ']', sep = ''))))
    }
  } else {
    if (ncol(data) == 1) {
      out <- getlabel.default(data)
    } else {
      out <- sapply(data, getlabel.default)
    }
    if (any(out == "")){
      tmp <- colnames(data)
      out <- ifelse(out == "", tmp, out)
    }
  }

  # return
  out
}