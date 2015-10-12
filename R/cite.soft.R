#' To create BibTex file for references of software
#'
#' @param soft list of citation or package want to reference
#' @param key key for inline citation
#' @param output output file
#'
#' @return Bibtex file
#' @export
cite.soft <- function(soft, key, output = "software.bib", append = FALSE) {

  capture.output(invisible(mapply(function(key_i, soft_i) {
    ## get citation
    if ("citation" %in% class(soft_i)) {
      tmp <- add.key(key_i, soft_i)
    } else {
      tmp <- add.key(key_i, citation(soft_i)[1])
    }
    ## output
    print(toBibtex(tmp))
    },
         key_i = key, soft_i = soft)), file = output, append = append)
}

add.key <- function(key, obj){
  # to add key into obj
  obj$key <- key
  return(obj)
}

