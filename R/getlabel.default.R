getlabel.default <- function(x, units = TRUE, fit = FALSE){
  if (is.null(attr(x, "type"))) attr(x, "type") <- ""
  if (is.null(attr(x, "label"))) attr(x, "label") <- ""

  # to create polished label
  if ((is.factor(x) | attr(x, 'type') == 'factor') | units == FALSE) {
    # if factor or do not need unit
    as.character(attr(x, 'label'))
  } else {
    # if not factor and need unit
    new_unit <- ifelse(fit == FALSE, ifelse(is.null(attr(x, 'units')), '', attr(x, 'units')),
                       paste('+',
                             ifelse(is.null(attr(x, 'scale')), 1, attr(x, 'scale')),
                             ' ',
                             ifelse(is.null(attr(x, 'units')), '', attr(x, 'units')), sep = ''))
    ifelse(new_unit == "",
           as.character(attr(x, 'label')),
           as.character(paste(attr(x, 'label'), ' [', new_unit, ']', sep = '')))

  }
}