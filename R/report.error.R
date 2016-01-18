#' @export
report.error <- function(data, id, criteria, message = "Untitled", csv.file, print = FALSE){
  data <- as.data.frame(data)
  if (missing(id)) {
    tmp <- paste("with(data", ", which(", criteria, "))", sep = "")
  } else {
    tmp <- paste("subset(data", ", subset = ", criteria, ", select =", id, ", drop = TRUE", ")", sep = "")
  }

  if (print) print(tmp) else {
    idx <- eval(parse(text = tmp))
    if (length(idx) > 0) {
      tmp2 <- paste("with(data", ", which(", criteria, "))", sep = "")
      idx2 <- eval(parse(text = tmp2))
      if (missing(id)) {
        output <- data.frame(id = NA,
                             index = idx2,
                             error = message)
      } else {
        output <- data.frame(id = paste('"=""', idx, '"""', sep = ""),
                             index = idx2,
                             error = message)
      }
      if (!missing(csv.file)) {
        if (file.exists(csv.file)) {
          write.table(output,
                      file = csv.file, quote = FALSE, sep = ",", dec = ".", qmethod = "double",
                      row.names = FALSE, append = TRUE, col.names = FALSE)
        } else {
          write.table(output,
                      file = csv.file, quote = FALSE, sep = ",", dec = ".", qmethod = "double", row.names = FALSE)
        }
      }
      return(output)
    } else {cat("No id fulfils this criteria.")}
  }
}