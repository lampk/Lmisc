#' @export
getlabel <- function(x, meta = NULL, units = TRUE, fit = FALSE){
  if (is.null(dim(x))) {
    getlabel.one(x, meta = meta, units = units, fit = fit)
  } else {
    getlabel.data(x, meta = meta, units = units, fit = fit)
  }
}
