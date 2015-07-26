getlabel.one <- function(x, meta = NULL, units = TRUE, fit = FALSE){
  # to create polished label for variable x from meta information

  if (!is.null(meta)) {
    # extract meta info
    .label <- ifelse(is.na(meta$label[meta$name == x]), '', meta$label[meta$name == x])
    .units <- ifelse(is.na(meta$units[meta$name == x]), '', meta$units[meta$name == x])
    .scale <- ifelse(is.na(meta$scale[meta$name == x]), 1, meta$scale[meta$name == x])
    .type <- meta$type[meta$name == x]

    # create label
    if (.type == 'factor' | units == FALSE) {
      # if factor or do not need unit
      out <- as.character(.label)
    } else {
      # if not factor and need unit
      new_unit <- ifelse(fit == FALSE, .units, paste('+', .scale, ' ', .units, sep = ''))
      out <- as.character(paste(.label, ' [', new_unit, ']', sep = ''))
    }
  } else {
    # if no meta information
    out <- getlabel.default(x, units = units, fit = fit)
  }

  # return
  out
}